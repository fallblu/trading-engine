open Cmdliner

type diagnostic_format = Human | Json
type output_format = Human_output | Json_output

type counts = {
  instruments : int64;
  schedule_batches : int64;
  slices : int64;
  audits : int64;
  orders : int64;
  active_orders : int64;
  filled_orders : int64;
  rejected_orders : int64;
}

type success = {
  operation : string;
  run_id : Trading_engine.Id.Run.t;
  scenario_sha256 : string;
  journal_sha256 : string option;
  transcript_sha256 : string option;
  counts : counts;
  valuation : Trading_engine.Account.valuation;
  journal : string option;
  transcript : string option;
}

type journal_destination = {
  replay_path : string;
  public_path : string;
  writes_stdout : bool;
  cleanup : unit -> unit;
}

let cli_error message =
  Trading_engine.Diagnostic.make
    ~code:Trading_engine.Diagnostic.Cli_invalid_arguments
    ~phase:Trading_engine.Diagnostic.Cli message

let input_error ~message exception_ =
  Trading_engine.Diagnostic.of_exception
    ~code:Trading_engine.Diagnostic.Input_io
    ~phase:Trading_engine.Diagnostic.Input ~message exception_

let count predicate values =
  List.fold_left
    (fun total value -> total + Bool.to_int (predicate value))
    0 values

let int64 value = `Intlit (Int64.to_string value)
let option_string = function None -> `Null | Some value -> `String value

let success_to_yojson success =
  let counts = success.counts in
  `Assoc
    [
      ("result_version", `String "1");
      ("status", `String "success");
      ("operation", `String success.operation);
      ("run_id", `String (Trading_engine.Id.Run.to_string success.run_id));
      ( "hashes",
        `Assoc
          [
            ("scenario_sha256", `String success.scenario_sha256);
            ("journal_sha256", option_string success.journal_sha256);
            ( "strategy_transcript_sha256",
              option_string success.transcript_sha256 );
          ] );
      ( "counts",
        `Assoc
          [
            ("instruments", int64 counts.instruments);
            ("schedule_batches", int64 counts.schedule_batches);
            ("slices", int64 counts.slices);
            ("audits", int64 counts.audits);
            ("orders", int64 counts.orders);
            ("active_orders", int64 counts.active_orders);
            ("filled_orders", int64 counts.filled_orders);
            ("rejected_orders", int64 counts.rejected_orders);
          ] );
      ( "valuation",
        Trading_engine.Codec.account_valuation_to_yojson ~contract_version:"16"
          success.valuation );
      ( "artifacts",
        `Assoc
          [
            ("journal", option_string success.journal);
            ("strategy_transcript", option_string success.transcript);
          ] );
    ]

let order_counts orders =
  let active = count Trading_engine.Order.is_active orders in
  let filled =
    count
      (fun order ->
        order.Trading_engine.Order.status = Trading_engine.Order.Filled)
      orders
  in
  let rejected =
    count
      (fun order ->
        match order.Trading_engine.Order.status with
        | Trading_engine.Order.Rejected _ -> true
        | _ -> false)
      orders
  in
  (active, filled, rejected)

let emit_success format ~to_stderr success =
  let formatter =
    if to_stderr then Format.err_formatter else Format.std_formatter
  in
  match format with
  | Json_output ->
      Fmt.pf formatter "%s@."
        (Yojson.Safe.to_string (success_to_yojson success))
  | Human_output ->
      let counts = success.counts in
      Fmt.pf formatter
        "run=%a audits=%Ld orders=%Ld active=%Ld filled=%Ld rejected=%Ld@."
        Trading_engine.Id.Run.pp success.run_id counts.audits counts.orders
        counts.active_orders counts.filled_orders counts.rejected_orders;
      Fmt.pf formatter "%a@." Trading_engine.Account.pp_valuation
        success.valuation;
      Option.iter (Fmt.pf formatter "journal=%s@.") success.journal;
      Option.iter
        (Fmt.pf formatter "strategy_transcript=%s@.")
        success.transcript

let digest_file path = Trading_engine.Sha256.digest_file path

let remove_if_exists path =
  try if Sys.file_exists path then Sys.remove path with Sys_error _ -> ()

let temporary_journal_destination () =
  try
    let directory =
      Filename.temp_dir ~perms:0o700 "trading-engine-journal-" ""
    in
    let path = Filename.concat directory "journal.jsonl" in
    Ok
      {
        replay_path = path;
        public_path = "stdout";
        writes_stdout = true;
        cleanup =
          (fun () ->
            remove_if_exists path;
            remove_if_exists (path ^ ".partial");
            remove_if_exists (path ^ ".partial.cleanup");
            try Unix.rmdir directory with Unix.Unix_error _ -> ());
      }
  with exception_ ->
    Error
      (Trading_engine.Diagnostic.of_exception
         ~code:Trading_engine.Diagnostic.Artifact_io
         ~phase:Trading_engine.Diagnostic.Artifact
         ~message:"could not create temporary journal spool" exception_)

let journal_destination path =
  if String.equal path "-" then temporary_journal_destination ()
  else
    Ok
      {
        replay_path = path;
        public_path = path;
        writes_stdout = false;
        cleanup = Fun.id;
      }

let copy_file_to_stdout path =
  try
    In_channel.with_open_bin path (fun channel ->
        let buffer = Bytes.create 65_536 in
        let rec loop () =
          match input channel buffer 0 (Bytes.length buffer) with
          | 0 -> ()
          | length ->
              output stdout buffer 0 length;
              loop ()
        in
        loop ());
    flush stdout;
    Ok ()
  with exception_ ->
    Error
      (Trading_engine.Diagnostic.of_exception
         ~code:Trading_engine.Diagnostic.Artifact_io
         ~phase:Trading_engine.Diagnostic.Artifact
         ~message:"could not write journal to standard output" exception_)

let finish_success output_format destination success =
  let result =
    match digest_file destination.replay_path with
    | Error _ as error -> error
    | Ok journal_sha256 ->
        let success =
          {
            success with
            journal_sha256 = Some journal_sha256;
            journal = Some destination.public_path;
          }
        in
        if destination.writes_stdout then (
          match copy_file_to_stdout destination.replay_path with
          | Error _ as error -> error
          | Ok () ->
              emit_success output_format ~to_stderr:true success;
              Ok ())
        else (
          emit_success output_format ~to_stderr:false success;
          Ok ())
  in
  destination.cleanup ();
  result

let run_replay scenario_sha256 scenario destination durability output_format =
  match
    Trading_engine.Replay.run ~scenario_sha256
      ~journal_path:destination.replay_path ~durability scenario
  with
  | Error message ->
      destination.cleanup ();
      Error message
  | Ok result ->
      let active, filled, rejected = order_counts result.orders in
      let success =
        {
          operation = "replay";
          run_id = scenario.Trading_engine.Scenario.run_id;
          scenario_sha256;
          journal_sha256 = None;
          transcript_sha256 = None;
          counts =
            {
              instruments = Int64.of_int (List.length scenario.instruments);
              schedule_batches = Int64.of_int (List.length scenario.schedule);
              slices = Int64.of_int (List.length scenario.slices);
              audits = Int64.of_int (List.length result.audits);
              orders = Int64.of_int (List.length result.orders);
              active_orders = Int64.of_int active;
              filled_orders = Int64.of_int filled;
              rejected_orders = Int64.of_int rejected;
            };
          valuation = result.valuation;
          journal = None;
          transcript = None;
        }
      in
      finish_success output_format destination success

let run_stream input destination durability output_format =
  match
    Trading_engine.Replay.run_stream ~journal_path:destination.replay_path
      ~durability input
  with
  | Error message ->
      destination.cleanup ();
      Error message
  | Ok result ->
      let active, filled, rejected = order_counts result.orders in
      let success =
        {
          operation = "replay";
          run_id = result.run_id;
          scenario_sha256 = result.scenario_sha256;
          journal_sha256 = None;
          transcript_sha256 = None;
          counts =
            {
              instruments = Int64.of_int result.instrument_count;
              schedule_batches = result.schedule_count;
              slices = result.slice_count;
              audits = result.audit_count;
              orders = Int64.of_int (List.length result.orders);
              active_orders = Int64.of_int active;
              filled_orders = Int64.of_int filled;
              rejected_orders = Int64.of_int rejected;
            };
          valuation = result.valuation;
          journal = None;
          transcript = None;
        }
      in
      finish_success output_format destination success

type external_strategy = {
  command : string list;
  timeout : float;
  transcript : string;
}

let run_external_replay environment scenario_sha256 scenario destination
    strategy durability output_format =
  match
    Trading_engine.External_replay.run ~durability ~env:environment
      ~scenario_sha256 ~journal_path:destination.replay_path
      ~transcript_path:strategy.transcript ~strategy_command:strategy.command
      ~strategy_timeout:strategy.timeout scenario
  with
  | Error message ->
      destination.cleanup ();
      Error message
  | Ok result -> (
      let active, filled, rejected = order_counts result.orders in
      match digest_file strategy.transcript with
      | Error _ as error ->
          destination.cleanup ();
          error
      | Ok transcript_sha256 ->
          finish_success output_format destination
            {
              operation = "replay";
              run_id = scenario.Trading_engine.Scenario.run_id;
              scenario_sha256;
              journal_sha256 = None;
              transcript_sha256 = Some transcript_sha256;
              counts =
                {
                  instruments = Int64.of_int (List.length scenario.instruments);
                  schedule_batches = 0L;
                  slices = Int64.of_int (List.length scenario.slices);
                  audits = Int64.of_int (List.length result.audits);
                  orders = Int64.of_int (List.length result.orders);
                  active_orders = Int64.of_int active;
                  filled_orders = Int64.of_int filled;
                  rejected_orders = Int64.of_int rejected;
                };
              valuation = result.valuation;
              journal = None;
              transcript = Some strategy.transcript;
            })

let run_external_stream environment input destination strategy durability
    output_format =
  match
    Trading_engine.External_replay.run_stream ~durability ~env:environment
      ~journal_path:destination.replay_path ~transcript_path:strategy.transcript
      ~strategy_command:strategy.command ~strategy_timeout:strategy.timeout
      input
  with
  | Error message ->
      destination.cleanup ();
      Error message
  | Ok result -> (
      let active, filled, rejected = order_counts result.orders in
      match digest_file strategy.transcript with
      | Error _ as error ->
          destination.cleanup ();
          error
      | Ok transcript_sha256 ->
          finish_success output_format destination
            {
              operation = "replay";
              run_id = result.run_id;
              scenario_sha256 = result.scenario_sha256;
              journal_sha256 = None;
              transcript_sha256 = Some transcript_sha256;
              counts =
                {
                  instruments = Int64.of_int result.instrument_count;
                  schedule_batches = 0L;
                  slices = result.slice_count;
                  audits = result.audit_count;
                  orders = Int64.of_int (List.length result.orders);
                  active_orders = Int64.of_int active;
                  filled_orders = Int64.of_int filled;
                  rejected_orders = Int64.of_int rejected;
                };
              valuation = result.valuation;
              journal = None;
              transcript = Some strategy.transcript;
            })

let emit_validation output_format ~run_id ~scenario_sha256 ~instrument_count
    ~schedule_count ~slice_count ~orders ~valuation ~audit_count =
  let active, filled, rejected = order_counts orders in
  emit_success output_format ~to_stderr:false
    {
      operation = "validate";
      run_id;
      scenario_sha256;
      journal_sha256 = None;
      transcript_sha256 = None;
      counts =
        {
          instruments = instrument_count;
          schedule_batches = schedule_count;
          slices = slice_count;
          audits = audit_count;
          orders = Int64.of_int (List.length orders);
          active_orders = Int64.of_int active;
          filled_orders = Int64.of_int filled;
          rejected_orders = Int64.of_int rejected;
        };
      valuation;
      journal = None;
      transcript = None;
    };
  Ok ()

let execute_json environment input journal validate_only strategy durability
    output_format =
  let document =
    try Ok (In_channel.with_open_bin input In_channel.input_all)
    with Sys_error message as exception_ ->
      Error
        (input_error
           ~message:("could not read scenario: " ^ message)
           exception_)
  in
  match document with
  | Error _ as error -> error
  | Ok document -> (
      let scenario_sha256 = Trading_engine.Sha256.digest_string document in
      match Trading_engine.Scenario.of_string document with
      | Error message -> Error message
      | Ok scenario -> (
          if validate_only then
            match journal with
            | Some _ ->
                Error
                  (cli_error "--journal cannot be used with --validate-only")
            | None -> (
                match Trading_engine.Replay.run ~scenario_sha256 scenario with
                | Error message -> Error message
                | Ok result ->
                    if output_format = Human_output then (
                      Fmt.pr
                        "valid run=%a instruments=%d schedule=%d slices=%d \
                         scenario_sha256=%s@."
                        Trading_engine.Id.Run.pp scenario.run_id
                        (List.length scenario.instruments)
                        (List.length scenario.schedule)
                        (List.length scenario.slices)
                        scenario_sha256;
                      Ok ())
                    else
                      emit_validation output_format ~run_id:scenario.run_id
                        ~scenario_sha256
                        ~instrument_count:
                          (Int64.of_int (List.length scenario.instruments))
                        ~schedule_count:
                          (Int64.of_int (List.length scenario.schedule))
                        ~slice_count:
                          (Int64.of_int (List.length scenario.slices))
                        ~orders:result.orders ~valuation:result.valuation
                        ~audit_count:(Int64.of_int (List.length result.audits)))
          else
            match journal with
            | None ->
                Error
                  (cli_error
                     "--journal is required unless --validate-only is set")
            | Some path -> (
                match journal_destination path with
                | Error _ as error -> error
                | Ok destination -> (
                    match strategy with
                    | None ->
                        run_replay scenario_sha256 scenario destination
                          durability output_format
                    | Some strategy ->
                        run_external_replay environment scenario_sha256 scenario
                          destination strategy durability output_format))))

let execute_jsonl environment input journal validate_only strategy durability
    output_format =
  if validate_only then
    match journal with
    | Some _ ->
        Error (cli_error "--journal cannot be used with --validate-only")
    | None -> (
        match Trading_engine.Replay.run_stream input with
        | Error message -> Error message
        | Ok result ->
            if output_format = Human_output then (
              Fmt.pr
                "valid run=%a instruments=%d schedule=%Ld slices=%Ld \
                 scenario_sha256=%s@."
                Trading_engine.Id.Run.pp result.run_id result.instrument_count
                result.schedule_count result.slice_count result.scenario_sha256;
              Ok ())
            else
              emit_validation output_format ~run_id:result.run_id
                ~scenario_sha256:result.scenario_sha256
                ~instrument_count:(Int64.of_int result.instrument_count)
                ~schedule_count:result.schedule_count
                ~slice_count:result.slice_count ~orders:result.orders
                ~valuation:result.valuation ~audit_count:result.audit_count)
  else
    match journal with
    | None ->
        Error (cli_error "--journal is required unless --validate-only is set")
    | Some path -> (
        match journal_destination path with
        | Error _ as error -> error
        | Ok destination -> (
            match strategy with
            | None -> run_stream input destination durability output_format
            | Some strategy ->
                run_external_stream environment input destination strategy
                  durability output_format))

type input_format = Json | Jsonl

let execute_scenario environment input journal validate_only strategy durability
    output_format = function
  | Json ->
      execute_json environment input journal validate_only strategy durability
        output_format
  | Jsonl ->
      execute_jsonl environment input journal validate_only strategy durability
        output_format

let external_strategy executable arguments timeout transcript =
  match (executable, transcript, arguments, timeout) with
  | None, None, [], None -> Ok None
  | None, _, _, _ ->
      Error
        (cli_error
           "--strategy-arg, --strategy-timeout, and --strategy-transcript \
            require --strategy-executable")
  | Some _, None, _, _ ->
      Error
        (cli_error
           "--strategy-transcript is required with --strategy-executable")
  | Some executable, Some transcript, arguments, timeout ->
      let timeout = Option.value timeout ~default:30.0 in
      if (not (Float.is_finite timeout)) || Float.compare timeout 0.0 <= 0 then
        Error (cli_error "--strategy-timeout must be finite and positive")
      else Ok (Some { command = executable :: arguments; timeout; transcript })

let spool_standard_input () =
  let path, channel =
    Filename.open_temp_file ~mode:[ Open_binary ] "trading-engine-stdin-"
      ".jsonl"
  in
  let fail diagnostic =
    close_out_noerr channel;
    remove_if_exists path;
    Error diagnostic
  in
  try
    let buffer = Bytes.create 65_536 in
    let rec loop total =
      match input stdin buffer 0 (Bytes.length buffer) with
      | 0 -> Ok total
      | length ->
          if
            total
            > Trading_engine.Resource_limits.scenario_stream_bytes - length
          then
            Error
              (Trading_engine.Diagnostic.make
                 ~code:Trading_engine.Diagnostic.Resource_limit
                 ~phase:Trading_engine.Diagnostic.Input
                 (Printf.sprintf
                    "standard-input scenario stream exceeds %d bytes"
                    Trading_engine.Resource_limits.scenario_stream_bytes))
          else (
            output channel buffer 0 length;
            loop (total + length))
    in
    match loop 0 with
    | Error diagnostic -> fail diagnostic
    | Ok _ ->
        close_out channel;
        Ok path
  with exception_ ->
    fail
      (Trading_engine.Diagnostic.of_exception
         ~code:Trading_engine.Diagnostic.Input_io
         ~phase:Trading_engine.Diagnostic.Input
         ~message:"could not spool scenario stream from standard input"
         exception_)

let with_input_path input input_format function_ =
  if not (String.equal input "-") then function_ input
  else
    match input_format with
    | Json ->
        Error
          (cli_error
             "standard input requires --input-format jsonl; batch JSON is not \
              supported")
    | Jsonl -> (
        try
          match spool_standard_input () with
          | Error _ as error -> error
          | Ok path ->
              Fun.protect
                ~finally:(fun () -> remove_if_exists path)
                (fun () -> function_ path)
        with exception_ ->
          Error
            (Trading_engine.Diagnostic.of_exception
               ~code:Trading_engine.Diagnostic.Input_io
               ~phase:Trading_engine.Diagnostic.Input
               ~message:"could not prepare standard-input scenario stream"
               exception_))

let execute environment input journal validate_only capabilities input_format
    strategy_executable strategy_arguments strategy_timeout strategy_transcript
    durable_artifacts output_format =
  if capabilities then
    match
      ( input,
        journal,
        validate_only,
        strategy_executable,
        strategy_arguments,
        strategy_timeout,
        strategy_transcript,
        durable_artifacts )
    with
    | None, None, false, None, [], None, None, false ->
        Fmt.pr "%s@." (Trading_engine.Contract.capabilities_to_string ());
        Ok ()
    | _ ->
        Error
          (cli_error
             "--capabilities cannot be combined with replay or strategy options")
  else if validate_only && durable_artifacts then
    Error (cli_error "--durable-artifacts cannot be used with --validate-only")
  else if durable_artifacts && Option.equal String.equal journal (Some "-") then
    Error
      (cli_error
         "--durable-artifacts cannot be used when --journal writes to standard \
          output")
  else if Option.equal String.equal strategy_transcript (Some "-") then
    Error
      (cli_error
         "--strategy-transcript does not support standard output; choose a \
          file path")
  else
    match input with
    | None ->
        Error (cli_error "--input is required unless --capabilities is set")
    | Some path -> (
        match
          external_strategy strategy_executable strategy_arguments
            strategy_timeout strategy_transcript
        with
        | Error _ as error -> error
        | Ok (Some _) when validate_only ->
            Error
              (cli_error
                 "external strategy options cannot be used with --validate-only")
        | Ok strategy ->
            let durability =
              if durable_artifacts then Trading_engine.Artifact_writer.Durable
              else Trading_engine.Artifact_writer.Buffered
            in
            with_input_path path input_format (fun input_path ->
                execute_scenario environment input_path journal validate_only
                  strategy durability output_format input_format))

let input =
  let doc = "Read the replay scenario from $(docv)." in
  Arg.(
    value
    & opt (some string) None
    & info [ "input"; "i" ] ~docv:"SCENARIO|-" ~doc)

let input_format =
  let formats = Arg.enum [ ("json", Json); ("jsonl", Jsonl) ] in
  let doc = "Parse the scenario as $(docv)." in
  Arg.(value & opt formats Json & info [ "input-format" ] ~docv:"FORMAT" ~doc)

let journal =
  let doc =
    "Create the append-only JSON Lines audit journal at $(docv). Use '-' to \
     write a completed journal to standard output."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "journal"; "j" ] ~docv:"JOURNAL.jsonl" ~doc)

let durable_artifacts =
  let doc =
    "Synchronize staged artifact contents and directory metadata before \
     reporting success."
  in
  Arg.(value & flag & info [ "durable-artifacts" ] ~doc)

let validate_only =
  let doc = "Validate the scenario and exit without creating a journal." in
  Arg.(value & flag & info [ "validate-only" ] ~doc)

let capabilities =
  let doc = "Print machine-readable engine capabilities as JSON and exit." in
  Arg.(value & flag & info [ "capabilities" ] ~doc)

let diagnostic_format =
  let formats = Arg.enum [ ("human", Human); ("json", Json) ] in
  let doc = "Render runtime diagnostics as $(docv) (default: human)." in
  Arg.(
    value & opt formats Human & info [ "diagnostic-format" ] ~docv:"FORMAT" ~doc)

let output_format =
  let formats = Arg.enum [ ("human", Human_output); ("json", Json_output) ] in
  let doc =
    "Render successful validation and replay summaries as $(docv) (default: \
     human). JSON output also selects JSON diagnostics."
  in
  Arg.(
    value & opt formats Human_output
    & info [ "output-format" ] ~docv:"FORMAT" ~doc)

let strategy_executable =
  let doc =
    "Launch $(docv) as the external strategy process without using a shell."
  in
  Arg.(
    value
    & opt (some file) None
    & info [ "strategy-executable" ] ~docv:"PROGRAM" ~doc)

let strategy_argument =
  let doc =
    "Pass $(docv) to the external strategy. Repeat this option to preserve \
     argv boundaries."
  in
  Arg.(value & opt_all string [] & info [ "strategy-arg" ] ~docv:"ARG" ~doc)

let strategy_timeout =
  let doc =
    "Allow $(docv) seconds for each external strategy request and clean exit \
     (default: 30)."
  in
  Arg.(
    value
    & opt (some float) None
    & info [ "strategy-timeout" ] ~docv:"SECONDS" ~doc)

let strategy_transcript =
  let doc =
    "Create the append-only external strategy protocol transcript at $(docv)."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "strategy-transcript" ] ~docv:"TRANSCRIPT.jsonl" ~doc)

let command environment =
  let doc = "run a deterministic completed-bar trading replay" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Replays a scenario through strategy, risk, order management, \
         simulated execution, and exact accounting. The journal and its \
         partial path must not already exist. A successful replay atomically \
         finalizes the journal. Use $(b,--validate-only) for an in-memory dry \
         replay that does not create a journal.";
    ]
  in
  Cmd.v
    (Cmd.info "trading-engine" ~version:Trading_engine.Contract.engine_version
       ~doc ~man)
    Term.(
      const
        (fun
          input
          journal
          validate_only
          capabilities
          input_format
          strategy_executable
          strategy_arguments
          strategy_timeout
          strategy_transcript
          durable_artifacts
          diagnostic_format
          output_format
        ->
          ( diagnostic_format,
            output_format,
            execute environment input journal validate_only capabilities
              input_format strategy_executable strategy_arguments
              strategy_timeout strategy_transcript durable_artifacts
              output_format ))
      $ input $ journal $ validate_only $ capabilities $ input_format
      $ strategy_executable $ strategy_argument $ strategy_timeout
      $ strategy_transcript $ durable_artifacts $ diagnostic_format
      $ output_format)

let () =
  Fmt_tty.setup_std_outputs ();
  Eio_main.run @@ fun environment ->
  match Cmd.eval_value' (command environment) with
  | `Exit code -> exit code
  | `Ok (_, _, Ok ()) -> exit Cmd.Exit.ok
  | `Ok (diagnostic_format, output_format, Error diagnostic) ->
      let rendered =
        match (diagnostic_format, output_format) with
        | Human, Human_output ->
            "trading-engine: " ^ Trading_engine.Diagnostic.to_human diagnostic
        | Json, _ | _, Json_output ->
            Trading_engine.Diagnostic.to_json diagnostic
      in
      Fmt.epr "%s@." rendered;
      exit Cmd.Exit.some_error
