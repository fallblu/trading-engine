type t = {
  input : Eio.Flow.sink_ty Eio.Resource.t;
  close_input : unit -> unit;
  output : Eio.Buf_read.t;
  child : child;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  transcript : Strategy_transcript.t;
  effects : Boundary_effects.t;
  timeout : float;
  mutable next_sequence : int64;
}

and child = {
  process : Eio_unix.Process.ty Eio.Resource.t;
  pgid : int;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  effects : Boundary_effects.t;
  mutable status : Eio.Process.exit_status option;
}

external enable_child_subreaper : unit -> int
  = "trading_engine_enable_child_subreaper"

let graceful_termination_timeout = 1.0
let forced_reap_timeout = 5.0
let process_poll_interval = 0.01

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let valid_timeout value = Float.is_finite value && Float.compare value 0.0 > 0

let diagnostic ?sequence ~code message =
  Diagnostic.make ?sequence ~code ~phase:Diagnostic.Strategy message

let next_sequence session =
  if Int64.equal session.next_sequence Int64.max_int then
    Error
      (diagnostic ~sequence:session.next_sequence
         ~code:Diagnostic.Strategy_protocol
         "strategy protocol sequence is exhausted")
  else
    let current = session.next_sequence in
    session.next_sequence <- Int64.succ current;
    Ok current

let exception_diagnostic ?sequence stage exception_ =
  let code, message =
    match exception_ with
    | End_of_file ->
        ( Diagnostic.Strategy_protocol,
          stage ^ ": external strategy closed stdout" )
    | Eio.Buf_read.Buffer_limit_exceeded ->
        ( Diagnostic.Strategy_protocol,
          stage ^ ": strategy response exceeds the maximum message size" )
    | _ ->
        ( Diagnostic.Strategy_process,
          stage ^ ": " ^ Printexc.to_string exception_ )
  in
  Diagnostic.of_exception ?sequence ~code ~phase:Diagnostic.Strategy ~message
    exception_

let await_child (child : child) =
  match child.status with
  | Some status -> status
  | None ->
      let status =
        Boundary_effects.perform child.effects Boundary_effects.Reap_process
          (fun () -> Eio.Process.await child.process)
      in
      child.status <- Some status;
      status

let await_child_for (child : child) timeout =
  match child.status with
  | Some _ as status -> status
  | None -> (
      try
        match
          Eio.Time.with_timeout child.clock timeout (fun () ->
              Ok (await_child child))
        with
        | Ok status -> Some status
        | Error `Timeout -> None
      with exception_ ->
        raise
          (Failure
             (Diagnostic.to_human
                (exception_diagnostic "waiting for external strategy" exception_)))
      )

let process_group_exists pgid =
  try
    Unix.kill (-pgid) 0;
    true
  with
  | Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | Unix.Unix_error (Unix.EPERM, _, _) -> true

let signal_process_group pgid signal =
  try
    Unix.kill (-pgid) signal;
    Ok ()
  with
  | Unix.Unix_error (Unix.ESRCH, _, _) -> Ok ()
  | Unix.Unix_error (code, operation, target) as exception_ ->
      Error
        (Diagnostic.of_exception ~code:Diagnostic.Strategy_process
           ~phase:Diagnostic.Strategy
           ~message:
             (Printf.sprintf
                "could not signal external strategy process group: %s(%s): %s"
                operation target (Unix.error_message code))
           exception_)

let rec reap_descendants pgid =
  try
    match Unix.waitpid [ Unix.WNOHANG ] (-pgid) with
    | 0, _ -> ()
    | _, _ -> reap_descendants pgid
  with Unix.Unix_error (Unix.ECHILD, _, _) -> ()

let rec wait_for_process_group (child : child) deadline =
  reap_descendants child.pgid;
  if not (process_group_exists child.pgid) then true
  else
    let remaining = deadline -. Eio.Time.now child.clock in
    if Float.compare remaining 0.0 <= 0 then false
    else (
      Eio.Time.sleep child.clock (Float.min process_poll_interval remaining);
      wait_for_process_group child deadline)

let terminate_process_group_direct (child : child) =
  Eio.Cancel.protect (fun () ->
      let graceful_deadline =
        Eio.Time.now child.clock +. graceful_termination_timeout
      in
      let* () = signal_process_group child.pgid Sys.sigterm in
      let direct_status =
        match child.status with
        | Some _ as status -> status
        | None ->
            let remaining = graceful_deadline -. Eio.Time.now child.clock in
            if Float.compare remaining 0.0 <= 0 then None
            else await_child_for child remaining
      in
      let group_stopped =
        match direct_status with
        | None -> false
        | Some _ -> wait_for_process_group child graceful_deadline
      in
      if group_stopped then Ok ()
      else
        let forced_deadline = Eio.Time.now child.clock +. forced_reap_timeout in
        let* () = signal_process_group child.pgid Sys.sigkill in
        let direct_status =
          match direct_status with
          | Some _ as status -> status
          | None -> await_child_for child forced_reap_timeout
        in
        match direct_status with
        | None ->
            Error
              (diagnostic ~code:Diagnostic.Strategy_process
                 "external strategy did not exit after forced termination")
        | Some _ ->
            if wait_for_process_group child forced_deadline then Ok ()
            else
              Error
                (diagnostic ~code:Diagnostic.Strategy_process
                   "external strategy descendants remained after forced \
                    termination"))

let terminate_process_group (child : child) =
  try
    Boundary_effects.perform child.effects Boundary_effects.Terminate_process
      (fun () -> terminate_process_group_direct child)
  with exception_ ->
    Error (exception_diagnostic "terminating external strategy" exception_)

let append_cleanup_error result child =
  match terminate_process_group child with
  | Ok () -> result
  | Error cleanup -> (
      match result with
      | Ok _ -> Error cleanup
      | Error original -> Error (Diagnostic.combine original cleanup))

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
            Ok
              (Boundary_effects.perform session.effects
                 Boundary_effects.Exchange_process (fun () ->
                   Eio.Flow.copy_string request_line session.input;
                   Eio.Buf_read.line session.output)))
      with
      | Ok response -> Ok response
      | Error `Timeout ->
          Error
            (diagnostic ~sequence:expected_sequence
               ~code:Diagnostic.Strategy_timeout
               (stage ^ ": external strategy timed out"))
    with exception_ ->
      Error (exception_diagnostic ~sequence:expected_sequence stage exception_)
  in
  let* response = response in
  let* response, response_json =
    Strategy_protocol.response_of_string ~expected_sequence response
    |> Result.map_error
         (Diagnostic.annotate ~sequence:expected_sequence ~json_path:"$")
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
      Error
        (diagnostic ~sequence ~code:Diagnostic.Strategy_protocol
           ("external strategy initialization failed: " ^ message))
  | Intents _ | Stopped ->
      Error
        (diagnostic ~sequence ~code:Diagnostic.Strategy_protocol
           "external strategy returned the wrong initialization response")

let on_event session context event =
  let* sequence = next_sequence session in
  let* response =
    exchange_at session ~stage:"strategy event" ~sequence (fun ~sequence ->
        Strategy_protocol.event_message ~sequence context event)
  in
  match response with
  | Strategy_protocol.Intents intents -> Ok intents
  | Failed message ->
      Error
        (diagnostic ~sequence ~code:Diagnostic.Strategy_protocol
           ("external strategy failed: " ^ message))
  | Ready _ | Stopped ->
      Error
        (diagnostic ~sequence ~code:Diagnostic.Strategy_protocol
           "external strategy returned the wrong event response")

let shutdown session =
  let* sequence = next_sequence session in
  let* response =
    exchange_at session ~stage:"strategy shutdown" ~sequence
      Strategy_protocol.shutdown_message
  in
  match response with
  | Strategy_protocol.Stopped -> Ok ()
  | Failed message ->
      Error
        (diagnostic ~sequence ~code:Diagnostic.Strategy_protocol
           ("external strategy shutdown failed: " ^ message))
  | Ready _ | Intents _ ->
      Error
        (diagnostic ~sequence ~code:Diagnostic.Strategy_protocol
           "external strategy returned the wrong shutdown response")

let await_exit session =
  session.close_input ();
  let status =
    try
      match
        Eio.Time.with_timeout session.clock session.timeout (fun () ->
            Ok (await_child session.child))
      with
      | Ok status -> Ok status
      | Error `Timeout ->
          Error
            (diagnostic ~code:Diagnostic.Strategy_timeout
               "external strategy did not exit after shutdown")
    with exception_ ->
      Error (exception_diagnostic "waiting for external strategy" exception_)
  in
  let* status = status in
  match status with
  | `Exited 0 -> (
      let* () = terminate_process_group session.child in
      let trailing_output =
        try
          match
            Eio.Time.with_timeout session.clock session.timeout (fun () ->
                Ok (Eio.Buf_read.peek_char session.output))
          with
          | Ok value -> Ok value
          | Error `Timeout ->
              Error
                (diagnostic ~code:Diagnostic.Strategy_timeout
                   "external strategy stdout did not close after exit")
        with exception_ ->
          Error
            (exception_diagnostic "reading final strategy output" exception_)
      in
      let* trailing_output = trailing_output in
      match trailing_output with
      | None -> Ok ()
      | Some _ ->
          Error
            (diagnostic ~code:Diagnostic.Strategy_protocol
               "external strategy wrote data after its stopped response"))
  | `Exited code ->
      Error
        (diagnostic ~code:Diagnostic.Strategy_exit
           (Printf.sprintf "external strategy exited with code %d" code))
  | `Signaled signal ->
      Error
        (diagnostic ~code:Diagnostic.Strategy_exit
           (Printf.sprintf "external strategy was killed by signal %d" signal))

let validate_configuration ~command ~timeout =
  if not (valid_timeout timeout) then
    Error
      (diagnostic ~code:Diagnostic.Strategy_invalid_configuration
         "strategy response timeout must be finite and positive")
  else
    match command with
    | [] ->
        Error
          (diagnostic ~code:Diagnostic.Strategy_invalid_configuration
             "external strategy command must not be empty")
    | executable :: _ when String.length executable = 0 ->
        Error
          (diagnostic ~code:Diagnostic.Strategy_invalid_configuration
             "external strategy executable must not be empty")
    | executable :: _ -> Ok executable

let run_session ~effects ~env ~command ~executable ~timeout ~transcript
    ~(initialization : Strategy_protocol.initialization) use =
  try
    Eio.Switch.run ~name:"external-strategy" @@ fun switch ->
    let process_manager = Eio.Stdenv.process_mgr env in
    if enable_child_subreaper () <> 0 then
      failwith "could not enable external strategy child reaping";
    let child_stdout, strategy_stdout = Eio_unix.pipe switch in
    let strategy_stdin, child_stdin = Eio_unix.pipe switch in
    let fds =
      [
        (0, Eio_unix.Resource.fd strategy_stdin, `Blocking);
        (1, Eio_unix.Resource.fd strategy_stdout, `Blocking);
        (2, Eio_unix.Resource.fd (Eio.Stdenv.stderr env), `Blocking);
      ]
    in
    let process =
      Boundary_effects.perform effects Boundary_effects.Spawn_process (fun () ->
          Eio_unix.Process.spawn_unix ~sw:switch process_manager ~pgid:0 ~fds
            ~executable command)
    in
    Eio.Flow.close strategy_stdin;
    Eio.Flow.close strategy_stdout;
    let child =
      {
        process;
        pgid = Eio.Process.pid process;
        clock = Eio.Stdenv.clock env;
        effects;
        status = None;
      }
    in
    let close_input () = Eio.Flow.close child_stdin in
    let session =
      {
        input = (child_stdin :> Eio.Flow.sink_ty Eio.Resource.t);
        close_input;
        output =
          Eio.Buf_read.of_flow
            ~max_size:(Strategy_protocol.max_message_bytes + 1)
            child_stdout;
        child;
        clock = Eio.Stdenv.clock env;
        transcript;
        effects;
        timeout;
        next_sequence = 1L;
      }
    in
    try
      let result =
        let* identity = initialize session initialization in
        let* value = use session in
        let* () = shutdown session in
        let* () = await_exit session in
        Ok (value, identity)
      in
      match result with
      | Ok _ -> result
      | Error _ -> append_cleanup_error result child
    with exception_ ->
      let backtrace = Printexc.get_raw_backtrace () in
      ignore (terminate_process_group child);
      Printexc.raise_with_backtrace exception_ backtrace
  with
  | Eio.Cancel.Cancelled _ as exception_ -> raise exception_
  | exception_ ->
      Error (exception_diagnostic "external strategy process" exception_)

let with_staged_session ?(effects = Boundary_effects.direct) ~env ~command
    ~timeout ~transcript ~initialization use =
  match validate_configuration ~command ~timeout with
  | Error _ as error -> error
  | Ok executable ->
      run_session ~effects ~env ~command ~executable ~timeout ~transcript
        ~initialization use

let with_session ?(effects = Boundary_effects.direct)
    ?(durability = Artifact_writer.Buffered) ~env ~command ~timeout
    ~transcript_path ~initialization use =
  match validate_configuration ~command ~timeout with
  | Error _ as error -> error
  | Ok executable -> (
      match Strategy_transcript.create ~effects ~durability transcript_path with
      | Error _ as error -> error
      | Ok transcript -> (
          let fail result =
            Strategy_transcript.close_preserving_partial transcript;
            result
          in
          try
            match
              run_session ~effects ~env ~command ~executable ~timeout
                ~transcript ~initialization use
            with
            | Error _ as error -> fail error
            | Ok value -> (
                match Strategy_transcript.commit transcript with
                | Ok () -> Ok value
                | Error _ as error -> error)
          with Eio.Cancel.Cancelled _ as exception_ ->
            Strategy_transcript.close_preserving_partial transcript;
            raise exception_))
