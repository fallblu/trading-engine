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
      List.fold_left
        (fun result event ->
          match result with
          | Error _ as error -> error
          | Ok () -> Journal.append journal event)
        (Ok ()) events

let run ~scenario_sha256 ?journal_path scenario =
  let journal_result =
    match journal_path with
    | None -> Ok None
    | Some path -> Journal.create path |> Result.map Option.some
  in
  match journal_result with
  | Error _ as error -> error
  | Ok journal -> (
      let fail result =
        Option.iter Journal.close_preserving_partial journal;
        result
      in
      let succeed result =
        match journal with
        | None -> result
        | Some journal -> (
            match Journal.commit journal with
            | Ok () -> result
            | Error _ as error -> error)
      in
      match Scripted_strategy.create scenario.Scenario.schedule with
      | Error _ as error -> fail error
      | Ok strategy_state -> (
          match
            Engine.config ~risk:scenario.risk ~execution:scenario.execution
              ~max_internal_events:scenario.max_internal_events
          with
          | Error _ as error -> fail error
          | Ok config -> (
              match
                Runner.create ~run_id:scenario.run_id ~scenario_sha256 ~config
                  ~initial_cash:scenario.initial_cash ~strategy_state
              with
              | Error _ as error -> fail error
              | Ok initial -> (
                  let step result market_slice =
                    match result with
                    | Error _ as error -> error
                    | Ok (state, audits_rev) -> (
                        match Runner.process_slice state market_slice with
                        | Error _ as error -> error
                        | Ok (state, events) -> (
                            match append_events journal events with
                            | Error _ as error -> error
                            | Ok () ->
                                Ok (state, List.rev_append events audits_rev)))
                  in
                  match
                    List.fold_left step (Ok (initial, [])) scenario.slices
                  with
                  | Error _ as error -> fail error
                  | Ok (state, audits_rev) -> (
                      match Runner.complete state with
                      | Error _ as error -> fail error
                      | Ok (state, valuation, completion_events) -> (
                          match append_events journal completion_events with
                          | Error _ as error -> fail error
                          | Ok () ->
                              let audits_rev =
                                List.rev_append completion_events audits_rev
                              in
                              succeed
                                (Ok
                                   {
                                     account = Runner.account state;
                                     orders = Oms.orders (Runner.oms state);
                                     valuation;
                                     audits = List.rev audits_rev;
                                   })))))))
