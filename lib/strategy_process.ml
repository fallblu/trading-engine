type t = {
  input : Eio.Flow.sink_ty Eio.Resource.t;
  close_input : unit -> unit;
  output : Eio.Buf_read.t;
  await_process : unit -> Eio.Process.exit_status;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  transcript : Strategy_transcript.t;
  instruments : Instrument.t list;
  timeout : float;
  mutable next_sequence : int64;
}

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let valid_timeout value = Float.is_finite value && Float.compare value 0.0 > 0

let next_sequence session =
  if Int64.equal session.next_sequence Int64.max_int then
    Error "strategy protocol sequence is exhausted"
  else
    let current = session.next_sequence in
    session.next_sequence <- Int64.succ current;
    Ok current

let exception_message stage exception_ =
  match exception_ with
  | End_of_file -> stage ^ ": external strategy closed stdout"
  | Eio.Buf_read.Buffer_limit_exceeded ->
      stage ^ ": strategy response exceeds the maximum message size"
  | _ -> stage ^ ": " ^ Printexc.to_string exception_

let exchange session ~stage ~expected_sequence request =
  let* () =
    Strategy_transcript.append session.transcript
      ~direction:Strategy_protocol.Engine_to_strategy request
  in
  let request_line = Strategy_protocol.message_to_string request ^ "\n" in
  let response =
    try
      match
        Eio.Time.with_timeout session.clock session.timeout (fun () ->
            Eio.Flow.copy_string request_line session.input;
            Ok (Eio.Buf_read.line session.output))
      with
      | Ok response -> Ok response
      | Error `Timeout -> Error (stage ^ ": external strategy timed out")
    with exception_ -> Error (exception_message stage exception_)
  in
  let* response = response in
  let* response, response_json =
    Strategy_protocol.response_of_string ~expected_sequence response
  in
  let* () =
    Strategy_transcript.append session.transcript
      ~direction:Strategy_protocol.Strategy_to_engine response_json
  in
  Ok response

let exchange_at session ~stage ~sequence make_request =
  exchange session ~stage ~expected_sequence:sequence (make_request ~sequence)

let initialize session initialization =
  let* sequence = next_sequence session in
  let* response =
    exchange_at session ~stage:"strategy initialization" ~sequence
      (fun ~sequence ->
        Strategy_protocol.initialize_message ~sequence initialization)
  in
  match response with
  | Strategy_protocol.Ready identity -> Ok identity
  | Failed message ->
      Error ("external strategy initialization failed: " ^ message)
  | Intents _ | Stopped ->
      Error "external strategy returned the wrong initialization response"

let on_event session context event =
  let* sequence = next_sequence session in
  let* response =
    exchange_at session ~stage:"strategy event" ~sequence (fun ~sequence ->
        Strategy_protocol.event_message ~sequence
          ~instruments:session.instruments context event)
  in
  match response with
  | Strategy_protocol.Intents intents -> Ok intents
  | Failed message -> Error ("external strategy failed: " ^ message)
  | Ready _ | Stopped ->
      Error "external strategy returned the wrong event response"

let shutdown session =
  let* sequence = next_sequence session in
  let* response =
    exchange_at session ~stage:"strategy shutdown" ~sequence
      Strategy_protocol.shutdown_message
  in
  match response with
  | Strategy_protocol.Stopped -> Ok ()
  | Failed message -> Error ("external strategy shutdown failed: " ^ message)
  | Ready _ | Intents _ ->
      Error "external strategy returned the wrong shutdown response"

let await_exit session =
  session.close_input ();
  let status =
    try
      match
        Eio.Time.with_timeout session.clock session.timeout (fun () ->
            Ok (session.await_process ()))
      with
      | Ok status -> Ok status
      | Error `Timeout -> Error "external strategy did not exit after shutdown"
    with exception_ ->
      Error (exception_message "waiting for external strategy" exception_)
  in
  let* status = status in
  match status with
  | `Exited 0 -> (
      match Eio.Buf_read.peek_char session.output with
      | None -> Ok ()
      | Some _ ->
          Error "external strategy wrote data after its stopped response")
  | `Exited code ->
      Error (Printf.sprintf "external strategy exited with code %d" code)
  | `Signaled signal ->
      Error (Printf.sprintf "external strategy was killed by signal %d" signal)

let with_session ~env ~command ~timeout ~transcript_path
    ~(initialization : Strategy_protocol.initialization) use =
  if not (valid_timeout timeout) then
    Error "strategy response timeout must be finite and positive"
  else
    match command with
    | [] -> Error "external strategy command must not be empty"
    | executable :: _ when String.length executable = 0 ->
        Error "external strategy executable must not be empty"
    | executable :: _ -> (
        match Strategy_transcript.create transcript_path with
        | Error _ as error -> error
        | Ok transcript -> (
            let fail result =
              Strategy_transcript.close_preserving_partial transcript;
              result
            in
            try
              let result =
                Eio.Switch.run ~name:"external-strategy" @@ fun switch ->
                let process_manager = Eio.Stdenv.process_mgr env in
                let child_stdout, strategy_stdout =
                  Eio.Process.pipe ~sw:switch process_manager
                in
                let strategy_stdin, child_stdin =
                  Eio.Process.pipe ~sw:switch process_manager
                in
                let process =
                  Eio.Process.spawn ~sw:switch process_manager
                    ~stdin:strategy_stdin ~stdout:strategy_stdout
                    ~stderr:(Eio.Stdenv.stderr env) ~executable command
                in
                Eio.Flow.close strategy_stdin;
                Eio.Flow.close strategy_stdout;
                let close_input () = Eio.Flow.close child_stdin in
                let session =
                  {
                    input = (child_stdin :> Eio.Flow.sink_ty Eio.Resource.t);
                    close_input;
                    output =
                      Eio.Buf_read.of_flow
                        ~max_size:(Strategy_protocol.max_message_bytes + 1)
                        child_stdout;
                    await_process = (fun () -> Eio.Process.await process);
                    clock = Eio.Stdenv.clock env;
                    transcript;
                    instruments = initialization.instruments;
                    timeout;
                    next_sequence = 1L;
                  }
                in
                let* identity = initialize session initialization in
                let* value = use session in
                let* () = shutdown session in
                let* () = await_exit session in
                Ok (value, identity)
              in
              match result with
              | Error _ as error -> fail error
              | Ok value -> (
                  match Strategy_transcript.commit transcript with
                  | Ok () -> Ok value
                  | Error _ as error -> error)
            with exception_ ->
              fail
                (Error
                   (exception_message "external strategy process" exception_))))
