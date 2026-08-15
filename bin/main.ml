open Cmdliner

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

let execute_scenario input journal validate_only =
  Eio_main.run @@ fun _environment ->
  let document =
    try Ok (In_channel.with_open_bin input In_channel.input_all)
    with Sys_error message -> Error ("could not read scenario: " ^ message)
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
            | Some _ -> Error "--journal cannot be used with --validate-only"
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
                Error "--journal is required unless --validate-only is set"
            | Some path -> run_replay scenario_sha256 scenario path))

let execute input journal validate_only capabilities =
  if capabilities then
    match (input, journal, validate_only) with
    | None, None, false ->
        Fmt.pr "%s@." (Trading_engine.Contract.capabilities_to_string ());
        Ok ()
    | _ ->
        Error
          "--capabilities cannot be combined with --input, --journal, or \
           --validate-only"
  else
    match input with
    | None -> Error "--input is required unless --capabilities is set"
    | Some path -> execute_scenario path journal validate_only

let input =
  let doc = "Read the replay scenario from $(docv)." in
  Arg.(
    value
    & opt (some file) None
    & info [ "input"; "i" ] ~docv:"SCENARIO.json" ~doc)

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

let command =
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
    Term.(const execute $ input $ journal $ validate_only $ capabilities)

let () =
  Fmt_tty.setup_std_outputs ();
  exit (Cmd.eval_result command)
