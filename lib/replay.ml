module Runner = Engine.Make (Scripted_strategy)

type result = {
  account : Account.t;
  orders : Order.t list;
  valuation : Account.valuation;
  audits : Audit.t list;
}

let append_events journal events =
  match journal with
  | None -> Ok ()
  | Some journal ->
      let step result event =
        match result with
        | Error _ as error -> error
        | Ok () -> Journal.append journal event
      in
      List.fold_left step (Ok ()) events

let run ?journal_path scenario =
  let journal_result =
    match journal_path with
    | None -> Ok None
    | Some path -> Journal.create path |> Result.map Option.some
  in
  match journal_result with
  | Error _ as error -> error
  | Ok journal -> (
      let finish result =
        Option.iter Journal.close journal;
        result
      in
      match Scripted_strategy.create scenario.Scenario.schedule with
      | Error _ as error -> finish error
      | Ok strategy_state -> (
          match
            Engine.config ~risk:scenario.risk ~execution:scenario.execution
              ~max_internal_events:scenario.max_internal_events
          with
          | Error _ as error -> finish error
          | Ok config -> (
              let initial =
                Runner.create ~run_id:scenario.run_id ~config
                  ~initial_cash:scenario.initial_cash ~strategy_state
              in
              let step result bar =
                match result with
                | Error _ as error -> error
                | Ok (state, audits_rev) -> (
                    match Runner.process_bar state bar with
                    | Error _ as error -> error
                    | Ok (state, events) -> (
                        match append_events journal events with
                        | Error _ as error -> error
                        | Ok () -> Ok (state, List.rev_append events audits_rev)
                        ))
              in
              match List.fold_left step (Ok (initial, [])) scenario.bars with
              | Error _ as error -> finish error
              | Ok (state, audits_rev) -> (
                  let marks =
                    List.filter_map
                      (fun instrument ->
                        match
                          Runner.latest_bar state instrument.Instrument.id
                        with
                        | None -> None
                        | Some bar -> Some (instrument.id, bar.Bar.close_price))
                      scenario.instruments
                  in
                  match Account.value (Runner.account state) ~marks with
                  | Error _ as error -> finish error
                  | Ok valuation ->
                      finish
                        (Ok
                           {
                             account = Runner.account state;
                             orders = Oms.orders (Runner.oms state);
                             valuation;
                             audits = List.rev audits_rev;
                           })))))
