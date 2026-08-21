open Cmdliner

type diagnostic_format = Human | Json

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

let run_replay scenario_sha256 scenario journal =
  match
    Trading_engine.Replay.run ~scenario_sha256 ~journal_path:journal scenario
  with
  | Error message -> Error message
  | Ok result ->
      let active = count Trading_engine.Order.is_active result.orders in
      let filled =
        count
          (fun order ->
            order.Trading_engine.Order.status = Trading_engine.Order.Filled)
          result.orders
      in
      let rejected =
        count
          (fun order ->
            match order.Trading_engine.Order.status with
            | Trading_engine.Order.Rejected _ -> true
            | _ -> false)
          result.orders
      in
      Fmt.pr "run=%a audits=%d orders=%d active=%d filled=%d rejected=%d@."
        Trading_engine.Id.Run.pp scenario.Trading_engine.Scenario.run_id
        (List.length result.audits)
        (List.length result.orders)
        active filled rejected;
      Fmt.pr "%a@." Trading_engine.Account.pp_valuation result.valuation;
      Fmt.pr "journal=%s@." journal;
      Ok ()

let run_stream input journal =
  match Trading_engine.Replay.run_stream ~journal_path:journal input with
  | Error message -> Error message
  | Ok result ->
      let active = count Trading_engine.Order.is_active result.orders in
      let filled =
        count
          (fun order ->
            order.Trading_engine.Order.status = Trading_engine.Order.Filled)
          result.orders
      in
      let rejected =
        count
          (fun order ->
            match order.Trading_engine.Order.status with
            | Trading_engine.Order.Rejected _ -> true
            | _ -> false)
          result.orders
      in
      Fmt.pr "run=%a audits=%Ld orders=%d active=%d filled=%d rejected=%d@."
        Trading_engine.Id.Run.pp result.run_id result.audit_count
        (List.length result.orders)
        active filled rejected;
      Fmt.pr "%a@." Trading_engine.Account.pp_valuation result.valuation;
      Fmt.pr "journal=%s@." journal;
      Ok ()

type external_strategy = {
  command : string list;
  timeout : float;
  transcript : string;
}

let run_external_replay environment scenario_sha256 scenario journal strategy =
  match
    Trading_engine.External_replay.run ~env:environment ~scenario_sha256
      ~journal_path:journal ~transcript_path:strategy.transcript
      ~strategy_command:strategy.command ~strategy_timeout:strategy.timeout
      scenario
  with
  | Error message -> Error message
  | Ok result ->
      let active = count Trading_engine.Order.is_active result.orders in
      let filled =
        count
          (fun order ->
            order.Trading_engine.Order.status = Trading_engine.Order.Filled)
          result.orders
      in
      let rejected =
        count
          (fun order ->
            match order.Trading_engine.Order.status with
            | Trading_engine.Order.Rejected _ -> true
            | _ -> false)
          result.orders
      in
      Fmt.pr "run=%a audits=%d orders=%d active=%d filled=%d rejected=%d@."
        Trading_engine.Id.Run.pp scenario.Trading_engine.Scenario.run_id
        (List.length result.audits)
        (List.length result.orders)
        active filled rejected;
      Fmt.pr "%a@." Trading_engine.Account.pp_valuation result.valuation;
      Fmt.pr "journal=%s@." journal;
      Fmt.pr "strategy_transcript=%s@." strategy.transcript;
      Ok ()

let run_external_stream environment input journal strategy =
  match
    Trading_engine.External_replay.run_stream ~env:environment
      ~journal_path:journal ~transcript_path:strategy.transcript
      ~strategy_command:strategy.command ~strategy_timeout:strategy.timeout
      input
  with
  | Error message -> Error message
  | Ok result ->
      let active = count Trading_engine.Order.is_active result.orders in
      let filled =
        count
          (fun order ->
            order.Trading_engine.Order.status = Trading_engine.Order.Filled)
          result.orders
      in
      let rejected =
        count
          (fun order ->
            match order.Trading_engine.Order.status with
            | Trading_engine.Order.Rejected _ -> true
            | _ -> false)
          result.orders
      in
      Fmt.pr "run=%a audits=%Ld orders=%d active=%d filled=%d rejected=%d@."
        Trading_engine.Id.Run.pp result.run_id result.audit_count
        (List.length result.orders)
        active filled rejected;
      Fmt.pr "%a@." Trading_engine.Account.pp_valuation result.valuation;
      Fmt.pr "journal=%s@." journal;
      Fmt.pr "strategy_transcript=%s@." strategy.transcript;
      Ok ()

let execute_json environment input journal validate_only strategy =
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
                | Ok _ ->
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
            match journal with
            | None ->
                Error
                  (cli_error
                     "--journal is required unless --validate-only is set")
            | Some path -> (
                match strategy with
                | None -> run_replay scenario_sha256 scenario path
                | Some strategy ->
                    run_external_replay environment scenario_sha256 scenario
                      path strategy)))

let execute_jsonl environment input journal validate_only strategy =
  if validate_only then
    match journal with
    | Some _ ->
        Error (cli_error "--journal cannot be used with --validate-only")
    | None -> (
        match Trading_engine.Replay.run_stream input with
        | Error message -> Error message
        | Ok result ->
            Fmt.pr
              "valid run=%a instruments=%d schedule=%Ld slices=%Ld \
               scenario_sha256=%s@."
              Trading_engine.Id.Run.pp result.run_id result.instrument_count
              result.schedule_count result.slice_count result.scenario_sha256;
            Ok ())
  else
    match journal with
    | None ->
        Error (cli_error "--journal is required unless --validate-only is set")
    | Some path -> (
        match strategy with
        | None -> run_stream input path
        | Some strategy -> run_external_stream environment input path strategy)

type input_format = Json | Jsonl

let execute_scenario environment input journal validate_only strategy = function
  | Json -> execute_json environment input journal validate_only strategy
  | Jsonl -> execute_jsonl environment input journal validate_only strategy

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

let execute environment input journal validate_only capabilities input_format
    strategy_executable strategy_arguments strategy_timeout strategy_transcript
    =
  if capabilities then
    match
      ( input,
        journal,
        validate_only,
        strategy_executable,
        strategy_arguments,
        strategy_timeout,
        strategy_transcript )
    with
    | None, None, false, None, [], None, None ->
        Fmt.pr "%s@." (Trading_engine.Contract.capabilities_to_string ());
        Ok ()
    | _ ->
        Error
          (cli_error
             "--capabilities cannot be combined with replay or strategy options")
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
            execute_scenario environment path journal validate_only strategy
              input_format)

let input =
  let doc = "Read the replay scenario from $(docv)." in
  Arg.(
    value & opt (some file) None & info [ "input"; "i" ] ~docv:"SCENARIO" ~doc)

let input_format =
  let formats = Arg.enum [ ("json", Json); ("jsonl", Jsonl) ] in
  let doc = "Parse the scenario as $(docv)." in
  Arg.(value & opt formats Json & info [ "input-format" ] ~docv:"FORMAT" ~doc)

let journal =
  let doc = "Create the append-only JSON Lines audit journal at $(docv)." in
  Arg.(
    value
    & opt (some string) None
    & info [ "journal"; "j" ] ~docv:"JOURNAL.jsonl" ~doc)

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
          diagnostic_format
        ->
          ( diagnostic_format,
            execute environment input journal validate_only capabilities
              input_format strategy_executable strategy_arguments
              strategy_timeout strategy_transcript ))
      $ input $ journal $ validate_only $ capabilities $ input_format
      $ strategy_executable $ strategy_argument $ strategy_timeout
      $ strategy_transcript $ diagnostic_format)

let () =
  Fmt_tty.setup_std_outputs ();
  Eio_main.run @@ fun environment ->
  match Cmd.eval_value' (command environment) with
  | `Exit code -> exit code
  | `Ok (_, Ok ()) -> exit Cmd.Exit.ok
  | `Ok (format, Error diagnostic) ->
      let rendered =
        match format with
        | Human ->
            "trading-engine: " ^ Trading_engine.Diagnostic.to_human diagnostic
        | Json -> Trading_engine.Diagnostic.to_json diagnostic
      in
      Fmt.epr "%s@." rendered;
      exit Cmd.Exit.some_error
