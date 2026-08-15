open Cmdliner

let count predicate values =
  List.fold_left
    (fun total value -> total + Bool.to_int (predicate value))
    0 values

let run input journal =
  Eio_main.run @@ fun _environment ->
  match Trading_engine.Scenario.read_file input with
  | Error message -> Error message
  | Ok scenario -> (
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
            Trading_engine.Id.Run.pp scenario.run_id
            (List.length result.audits)
            (List.length result.orders)
            active filled rejected;
          Fmt.pr "%a@." Trading_engine.Account.pp_valuation result.valuation;
          Fmt.pr "journal=%s@." journal;
          Ok ())

let input =
  let doc = "Read the version 1 replay scenario from $(docv)." in
  Arg.(
    required
    & opt (some file) None
    & info [ "input"; "i" ] ~docv:"SCENARIO.json" ~doc)

let journal =
  let doc = "Create the append-only JSON Lines audit journal at $(docv)." in
  Arg.(
    required
    & opt (some string) None
    & info [ "journal"; "j" ] ~docv:"JOURNAL.jsonl" ~doc)

let command =
  let doc = "run a deterministic completed-bar trading replay" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Replays a versioned scenario through strategy, risk, order \
         management, simulated execution, and exact accounting. The journal \
         path must not already exist.";
    ]
  in
  Cmd.v
    (Cmd.info "trading-engine" ~version:"dev" ~doc ~man)
    Term.(const run $ input $ journal)

let () =
  Fmt_tty.setup_std_outputs ();
  exit (Cmd.eval_result command)
