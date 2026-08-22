module Runner = Engine.Make (Scripted_strategy)

type result = {
  account : Account.t;
  orders : Order.t list;
  valuation : Account.valuation;
  audits : Audit.t list;
}

type streamed_result = {
  run_id : Id.Run.t;
  scenario_sha256 : string;
  instrument_count : int;
  schedule_count : int64;
  account : Account.t;
  orders : Order.t list;
  valuation : Account.valuation;
  audit_count : int64;
  slice_count : int64;
}

type stream_state = {
  run_id : Id.Run.t;
  instrument_count : int;
  runner : Runner.t;
  journal : Journal.t option;
  audit_count : int64;
  schedule_count : int64;
}

let reducer ?sequence message =
  Diagnostic.make ?sequence ~code:Diagnostic.Reducer_failed
    ~phase:Diagnostic.Reducer message

let replay ?sequence message =
  Diagnostic.make ?sequence ~code:Diagnostic.Replay_failed
    ~phase:Diagnostic.Replay message

let reducer_result ?sequence result =
  Result.map_error
    (fun message ->
      if
        String.starts_with
          ~prefix:"internal event count exceeds configured limit" message
      then
        Diagnostic.make ?sequence ~code:Diagnostic.Resource_limit
          ~phase:Diagnostic.Reducer message
      else reducer ?sequence message)
    result

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

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

let add_audit_count count events =
  let added = Int64.of_int (List.length events) in
  if Int64.compare count (Int64.sub Int64.max_int added) > 0 then
    Error (replay "audit event count is exhausted")
  else Ok (Int64.add count added)

let engine_config ~contract_version ~risk ~venue_calendars ~execution_model
    ~execution ~financing ~settlement ~max_internal_events =
  match (financing, settlement) with
  | None, None ->
      Engine.config_v8 ~contract_version ~risk ~venue_calendars ~execution_model
        ~execution ~max_internal_events
  | Some financing, None ->
      Engine.config_v10 ~contract_version ~risk ~venue_calendars
        ~execution_model ~execution ~financing ~max_internal_events
  | Some financing, Some settlement ->
      if String.equal contract_version "15" then
        Engine.config_v15 ~contract_version ~risk ~venue_calendars
          ~execution_model ~execution ~financing ~settlement
          ~max_internal_events
      else if String.equal contract_version "14" then
        Engine.config_v14 ~contract_version ~risk ~venue_calendars
          ~execution_model ~execution ~financing ~settlement
          ~max_internal_events
      else if String.equal contract_version "13" then
        Engine.config_v13 ~contract_version ~risk ~venue_calendars
          ~execution_model ~execution ~financing ~settlement
          ~max_internal_events
      else if String.equal contract_version "12" then
        Engine.config_v12 ~contract_version ~risk ~venue_calendars
          ~execution_model ~execution ~financing ~settlement
          ~max_internal_events
      else
        Engine.config_v11 ~contract_version ~risk ~venue_calendars
          ~execution_model ~execution ~financing ~settlement
          ~max_internal_events
  | None, Some _ -> Error "settlement requires financing configuration"

let run ~scenario_sha256 ?journal_path ?(durability = Artifact_writer.Buffered)
    scenario =
  let* strategy_state =
    Scripted_strategy.create scenario.Scenario.schedule |> reducer_result
  in
  let* config =
    engine_config ~contract_version:scenario.contract_version
      ~risk:scenario.risk ~venue_calendars:scenario.venue_calendars
      ~execution_model:scenario.execution_model ~execution:scenario.execution
      ~financing:scenario.financing ~settlement:scenario.settlement
      ~max_internal_events:scenario.max_internal_events
    |> reducer_result
  in
  let* initial =
    (match scenario.initial_portfolio with
      | None ->
          Runner.create ~run_id:scenario.run_id ~scenario_sha256 ~config
            ~initial_cash:scenario.initial_cash ~strategy_state
      | Some initial_portfolio ->
          Runner.create_with_portfolio ~run_id:scenario.run_id ~scenario_sha256
            ~config ~initial_portfolio ~strategy_state)
    |> reducer_result
  in
  let journal_result =
    match journal_path with
    | None -> Ok None
    | Some path -> Journal.create ~durability path |> Result.map Option.some
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
      let step result market_slice =
        match result with
        | Error _ as error -> error
        | Ok (state, audits_rev) -> (
            match
              Runner.process_slice state market_slice
              |> reducer_result
                   ~sequence:market_slice.Market_slice.slice_sequence
            with
            | Error _ as error -> error
            | Ok (state, events) -> (
                match append_events journal events with
                | Error _ as error -> error
                | Ok () -> Ok (state, List.rev_append events audits_rev)))
      in
      match List.fold_left step (Ok (initial, [])) scenario.slices with
      | Error _ as error -> fail error
      | Ok (state, audits_rev) -> (
          match Runner.complete state |> reducer_result with
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
                       }))))

let run_stream_pass ~scenario_sha256 ~journal channel =
  Scenario_stream.fold_channel channel
    ~init:(fun header ->
      match Scripted_strategy.create [] |> reducer_result with
      | Error _ as error -> error
      | Ok strategy_state -> (
          match
            engine_config ~contract_version:header.contract_version
              ~risk:header.Scenario.risk ~venue_calendars:header.venue_calendars
              ~execution_model:header.execution_model
              ~execution:header.execution ~financing:header.financing
              ~settlement:header.settlement
              ~max_internal_events:header.max_internal_events
            |> reducer_result
          with
          | Error _ as error -> error
          | Ok config -> (
              match
                (match header.initial_portfolio with
                  | None ->
                      Runner.create ~run_id:header.run_id ~scenario_sha256
                        ~config ~initial_cash:header.initial_cash
                        ~strategy_state
                  | Some initial_portfolio ->
                      Runner.create_with_portfolio ~run_id:header.run_id
                        ~scenario_sha256 ~config ~initial_portfolio
                        ~strategy_state)
                |> reducer_result
              with
              | Error _ as error -> error
              | Ok runner ->
                  Ok
                    {
                      run_id = header.run_id;
                      instrument_count = List.length header.instruments;
                      runner;
                      journal;
                      audit_count = 0L;
                      schedule_count = 0L;
                    })))
    ~step:(fun state item ->
      match
        Scripted_strategy.create
          [ (item.Scenario.market_slice.slice_sequence, item.intents) ]
        |> reducer_result ~sequence:item.market_slice.slice_sequence
      with
      | Error _ as error -> error
      | Ok strategy_state -> (
          let runner = Runner.with_strategy_state state.runner strategy_state in
          match
            Runner.process_slice runner item.market_slice
            |> reducer_result ~sequence:item.market_slice.slice_sequence
          with
          | Error _ as error -> error
          | Ok (runner, events) -> (
              match append_events state.journal events with
              | Error _ as error -> error
              | Ok () ->
                  let schedule_count =
                    if item.intents = [] then state.schedule_count
                    else Int64.succ state.schedule_count
                  in
                  add_audit_count state.audit_count events
                  |> Result.map (fun audit_count ->
                      { state with runner; audit_count; schedule_count }))))
    ~finish:(fun state ~slice_count ->
      match Runner.complete state.runner |> reducer_result with
      | Error _ as error -> error
      | Ok (runner, valuation, events) -> (
          match append_events state.journal events with
          | Error _ as error -> error
          | Ok () ->
              add_audit_count state.audit_count events
              |> Result.map (fun audit_count ->
                  {
                    run_id = state.run_id;
                    scenario_sha256;
                    instrument_count = state.instrument_count;
                    schedule_count = state.schedule_count;
                    account = Runner.account runner;
                    orders = Oms.orders (Runner.oms runner);
                    valuation;
                    audit_count;
                    slice_count;
                  })))

let run_stream ?journal_path ?(durability = Artifact_writer.Buffered) path =
  let journal = ref None in
  let fail result =
    Option.iter Journal.close_preserving_partial !journal;
    result
  in
  try
    In_channel.with_open_bin path (fun channel ->
        let scenario_sha256 = Sha256.digest_channel channel in
        seek_in channel 0;
        match run_stream_pass ~scenario_sha256 ~journal:None channel with
        | Error _ as error -> error
        | Ok validated -> (
            seek_in channel 0;
            let validated_sha256 = Sha256.digest_channel channel in
            if not (String.equal scenario_sha256 validated_sha256) then
              Error
                (Diagnostic.make ~code:Diagnostic.Scenario_stream_changed
                   ~phase:Diagnostic.Input
                   "scenario stream changed during validation")
            else
              match journal_path with
              | None -> Ok validated
              | Some path -> (
                  match Journal.create ~durability path with
                  | Error _ as error -> error
                  | Ok created -> (
                      journal := Some created;
                      seek_in channel 0;
                      match
                        run_stream_pass ~scenario_sha256 ~journal:!journal
                          channel
                      with
                      | Error _ as error -> fail error
                      | Ok replayed -> (
                          seek_in channel 0;
                          let replayed_sha256 = Sha256.digest_channel channel in
                          if not (String.equal scenario_sha256 replayed_sha256)
                          then
                            fail
                              (Error
                                 (Diagnostic.make
                                    ~code:Diagnostic.Scenario_stream_changed
                                    ~phase:Diagnostic.Input
                                    "scenario stream changed during replay"))
                          else
                            match Journal.commit created with
                            | Error _ as error -> error
                            | Ok () -> Ok replayed)))))
  with Sys_error message as exception_ ->
    fail
      (Error
         (Diagnostic.of_exception ~code:Diagnostic.Input_io
            ~phase:Diagnostic.Input
            ~message:("could not read scenario stream: " ^ message)
            exception_))
