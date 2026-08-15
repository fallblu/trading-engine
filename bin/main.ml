open Cmdliner

let count predicate values =
  List.fold_left
    (fun total value -> total + Bool.to_int (predicate value))
    0 values

let run_replay scenario journal =
  match Trading_engine.Replay.run ~journal_path:journal scenario with
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

let execute input journal validate_only =
  Eio_main.run @@ fun _environment ->
  match Trading_engine.Scenario.read_file input with
  | Error message -> Error message
  | Ok scenario -> (
      if validate_only then
        match journal with
        | Some _ -> Error "--journal cannot be used with --validate-only"
        | None -> (
            match Trading_engine.Replay.run scenario with
            | Error message -> Error message
            | Ok _ ->
                Fmt.pr
                  "valid run=%a schema=%d instruments=%d schedule=%d bars=%d@."
                  Trading_engine.Id.Run.pp scenario.run_id
                  scenario.schema_version
                  (List.length scenario.instruments)
                  (List.length scenario.schedule)
                  (List.length scenario.bars);
                Ok ())
      else
        match journal with
        | None -> Error "--journal is required unless --validate-only is set"
        | Some path -> run_replay scenario path)

let input =
  let doc = "Read the version 1 replay scenario from $(docv)." in
  Arg.(
    required
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

let command =
  let doc = "run a deterministic completed-bar trading replay" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Replays a versioned scenario through strategy, risk, order \
         management, simulated execution, and exact accounting. The journal \
         path must not already exist. Use $(b,--validate-only) for an \
         in-memory dry replay that does not create a journal.";
    ]
  in
  Cmd.v
    (Cmd.info "trading-engine" ~version:"dev" ~doc ~man)
    Term.(const execute $ input $ journal $ validate_only)

let () =
  Fmt_tty.setup_std_outputs ();
  exit (Cmd.eval_result command)
