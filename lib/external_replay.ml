module Runner = Engine.Interactive

type result = {
  account : Account.t;
  orders : Order.t list;
  valuation : Account.valuation;
  audits : Audit.t list;
  strategy : Strategy_protocol.identity;
}

type streamed_result = {
  run_id : Id.Run.t;
  scenario_sha256 : string;
  instrument_count : int;
  account : Account.t;
  orders : Order.t list;
  valuation : Account.valuation;
  audit_count : int64;
  slice_count : int64;
  strategy : Strategy_protocol.identity;
}

type validated_stream = {
  initialization : Strategy_protocol.initialization;
  slice_count : int64;
}

type stream_state = {
  runner : Runner.t;
  journal : Journal.t option;
  audit_count : int64;
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

let initialization_of_scenario ~scenario_sha256 (scenario : Scenario.t) =
  Strategy_protocol.
    {
      scenario_contract_version = scenario.contract_version;
      scenario_sha256;
      metadata = scenario.metadata;
      run_id = scenario.run_id;
      base_currency = scenario.base_currency;
      initial_cash = scenario.initial_cash;
      initial_portfolio = scenario.initial_portfolio;
      instruments = scenario.instruments;
      venue_calendars = scenario.venue_calendars;
      risk = scenario.risk;
      execution_model = scenario.execution_model;
      execution = scenario.execution;
    }

let initialization_of_header ~scenario_sha256 (header : Scenario.stream_header)
    =
  Strategy_protocol.
    {
      scenario_contract_version = header.contract_version;
      scenario_sha256;
      metadata = header.metadata;
      run_id = header.run_id;
      base_currency = header.base_currency;
      initial_cash = header.initial_cash;
      initial_portfolio = header.initial_portfolio;
      instruments = header.instruments;
      venue_calendars = header.venue_calendars;
      risk = header.risk;
      execution_model = header.execution_model;
      execution = header.execution;
    }

let create_runner ~contract_version ~run_id ~scenario_sha256 ~risk
    ~venue_calendars ~execution_model ~execution ~max_internal_events
    ~initial_cash ~initial_portfolio =
  let* config =
    Engine.config_v8 ~contract_version ~risk ~venue_calendars ~execution_model
      ~execution ~max_internal_events
    |> reducer_result
  in
  match initial_portfolio with
  | None ->
      Runner.create ~run_id ~scenario_sha256 ~config ~initial_cash
      |> reducer_result
  | Some initial_portfolio ->
      Runner.create_with_portfolio ~run_id ~scenario_sha256 ~config
        ~initial_portfolio
      |> reducer_result

let append_events journal events =
  match journal with
  | None -> Ok ()
  | Some journal ->
      List.fold_left
        (fun result event ->
          let* () = result in
          Journal.append journal event)
        (Ok ()) events

let add_audit_count count events =
  let added = Int64.of_int (List.length events) in
  if Int64.compare count (Int64.sub Int64.max_int added) > 0 then
    Error (replay "audit event count is exhausted")
  else Ok (Int64.add count added)

let rec drive respond progress =
  match Runner.strategy_request progress with
  | Some (context, event) ->
      let* intents = respond context event in
      let* progress = Runner.resume progress intents |> reducer_result in
      drive respond progress
  | None -> (
      match Runner.slice_result progress with
      | Some result -> Ok result
      | None ->
          Error (replay "interactive engine reached an invalid progress state"))

let process_slice respond runner market_slice =
  let* progress =
    Runner.process_slice runner market_slice
    |> reducer_result ~sequence:market_slice.Market_slice.slice_sequence
  in
  drive respond progress

let create_artifacts ~durability ~journal_path ~transcript_path =
  let* journal = Journal.create ~durability journal_path in
  match Strategy_transcript.create ~durability transcript_path with
  | Ok transcript -> Ok (journal, transcript)
  | Error _ as error ->
      Journal.close_preserving_partial journal;
      error

let close_artifacts (journal, transcript) =
  Journal.close_preserving_partial journal;
  Strategy_transcript.close_preserving_partial transcript

let protect_artifacts artifacts run =
  try run ()
  with exception_ ->
    let backtrace = Printexc.get_raw_backtrace () in
    close_artifacts artifacts;
    Printexc.raise_with_backtrace exception_ backtrace

let commit_artifacts (journal, transcript) =
  Artifact_writer.commit
    [ Journal.artifact journal; Strategy_transcript.artifact transcript ]

let run ?(durability = Artifact_writer.Buffered) ~env ~scenario_sha256
    ~journal_path ~transcript_path ~strategy_command ~strategy_timeout
    (scenario : Scenario.t) =
  if scenario.schedule <> [] then
    Error
      (replay "external strategy replay requires an empty scenario schedule")
  else
    let initialization = initialization_of_scenario ~scenario_sha256 scenario in
    let* () =
      Strategy_process.validate_configuration ~command:strategy_command
        ~timeout:strategy_timeout ~initialization
    in
    let* initial =
      create_runner ~contract_version:scenario.contract_version
        ~run_id:scenario.run_id ~scenario_sha256 ~risk:scenario.risk
        ~venue_calendars:scenario.venue_calendars
        ~execution_model:scenario.execution_model ~execution:scenario.execution
        ~max_internal_events:scenario.max_internal_events
        ~initial_cash:scenario.initial_cash
        ~initial_portfolio:scenario.initial_portfolio
    in
    let* journal, transcript =
      create_artifacts ~durability ~journal_path ~transcript_path
    in
    let journal_ref = Some journal in
    let session_result =
      protect_artifacts (journal, transcript) @@ fun () ->
      Strategy_process.with_staged_session ~env ~command:strategy_command
        ~timeout:strategy_timeout ~transcript ~initialization (fun session ->
          let respond = Strategy_process.on_event session in
          let step result market_slice =
            let* state, audits_rev = result in
            let* state, events = process_slice respond state market_slice in
            let* () = append_events journal_ref events in
            Ok (state, List.rev_append events audits_rev)
          in
          let* state, audits_rev =
            List.fold_left step (Ok (initial, [])) scenario.slices
          in
          let* state, valuation, completion_events =
            Runner.complete state |> reducer_result
          in
          let* () = append_events journal_ref completion_events in
          Ok
            ( state,
              valuation,
              List.rev (List.rev_append completion_events audits_rev) ))
    in
    match session_result with
    | Error _ as error ->
        close_artifacts (journal, transcript);
        error
    | Ok ((state, valuation, audits), strategy) -> (
        match commit_artifacts (journal, transcript) with
        | Error _ as error -> error
        | Ok () ->
            Ok
              {
                account = Runner.account state;
                orders = Oms.orders (Runner.oms state);
                valuation;
                audits;
                strategy;
              })

let validate_stream_pass ~scenario_sha256 channel =
  Scenario_stream.fold_channel channel
    ~init:(fun header ->
      let* runner =
        create_runner ~contract_version:header.contract_version
          ~run_id:header.Scenario.run_id ~scenario_sha256 ~risk:header.risk
          ~venue_calendars:header.venue_calendars
          ~execution_model:header.execution_model ~execution:header.execution
          ~max_internal_events:header.max_internal_events
          ~initial_cash:header.initial_cash
          ~initial_portfolio:header.initial_portfolio
      in
      Ok (runner, initialization_of_header ~scenario_sha256 header, 0L))
    ~step:(fun (runner, initialization, slice_count) item ->
      if item.Scenario.intents <> [] then
        Error
          (replay ~sequence:item.market_slice.slice_sequence
             "external strategy replay requires empty streamed intents")
      else
        let* runner, _ =
          process_slice (fun _ _ -> Ok []) runner item.market_slice
        in
        Ok (runner, initialization, Int64.succ slice_count))
    ~finish:(fun (runner, initialization, counted_slices) ~slice_count ->
      if not (Int64.equal counted_slices slice_count) then
        Error (replay "scenario stream slice count changed during validation")
      else
        let* _, _, _ = Runner.complete runner |> reducer_result in
        Ok { initialization; slice_count })

let replay_stream_pass ~scenario_sha256 ~journal ~session channel =
  Scenario_stream.fold_channel channel
    ~init:(fun header ->
      let* runner =
        create_runner ~contract_version:header.contract_version
          ~run_id:header.Scenario.run_id ~scenario_sha256 ~risk:header.risk
          ~venue_calendars:header.venue_calendars
          ~execution_model:header.execution_model ~execution:header.execution
          ~max_internal_events:header.max_internal_events
          ~initial_cash:header.initial_cash
          ~initial_portfolio:header.initial_portfolio
      in
      Ok { runner; journal = Some journal; audit_count = 0L })
    ~step:(fun state item ->
      if item.Scenario.intents <> [] then
        Error
          (replay ~sequence:item.market_slice.slice_sequence
             "external strategy replay requires empty streamed intents")
      else
        let* runner, events =
          process_slice
            (Strategy_process.on_event session)
            state.runner item.market_slice
        in
        let* () = append_events state.journal events in
        let* audit_count = add_audit_count state.audit_count events in
        Ok { state with runner; audit_count })
    ~finish:(fun state ~slice_count ->
      let* runner, valuation, events =
        Runner.complete state.runner |> reducer_result
      in
      let* () = append_events state.journal events in
      let* audit_count = add_audit_count state.audit_count events in
      Ok (runner, valuation, audit_count, slice_count))

let run_stream ?(durability = Artifact_writer.Buffered) ~env ~journal_path
    ~transcript_path ~strategy_command ~strategy_timeout path =
  let artifacts_ref = ref None in
  let fail result =
    Option.iter close_artifacts !artifacts_ref;
    result
  in
  try
    In_channel.with_open_bin path (fun channel ->
        let scenario_sha256 = Sha256.digest_channel channel in
        seek_in channel 0;
        let* validated = validate_stream_pass ~scenario_sha256 channel in
        seek_in channel 0;
        let validated_sha256 = Sha256.digest_channel channel in
        if not (String.equal scenario_sha256 validated_sha256) then
          Error
            (Diagnostic.make ~code:Diagnostic.Scenario_stream_changed
               ~phase:Diagnostic.Input
               "scenario stream changed during validation")
        else
          let* () =
            Strategy_process.validate_configuration ~command:strategy_command
              ~timeout:strategy_timeout ~initialization:validated.initialization
          in
          let* journal, transcript =
            create_artifacts ~durability ~journal_path ~transcript_path
          in
          artifacts_ref := Some (journal, transcript);
          let session_result =
            protect_artifacts (journal, transcript) @@ fun () ->
            Strategy_process.with_staged_session ~env ~command:strategy_command
              ~timeout:strategy_timeout ~transcript
              ~initialization:validated.initialization (fun session ->
                seek_in channel 0;
                let* runner, valuation, audit_count, slice_count =
                  replay_stream_pass ~scenario_sha256 ~journal ~session channel
                in
                seek_in channel 0;
                let replayed_sha256 = Sha256.digest_channel channel in
                if not (String.equal scenario_sha256 replayed_sha256) then
                  Error
                    (Diagnostic.make ~code:Diagnostic.Scenario_stream_changed
                       ~phase:Diagnostic.Input
                       "scenario stream changed during replay")
                else Ok (runner, valuation, audit_count, slice_count))
          in
          match session_result with
          | Error _ as error -> fail error
          | Ok ((runner, valuation, audit_count, slice_count), strategy) -> (
              match commit_artifacts (journal, transcript) with
              | Error _ as error -> error
              | Ok () ->
                  Ok
                    {
                      run_id = validated.initialization.run_id;
                      scenario_sha256;
                      instrument_count =
                        List.length validated.initialization.instruments;
                      account = Runner.account runner;
                      orders = Oms.orders (Runner.oms runner);
                      valuation;
                      audit_count;
                      slice_count;
                      strategy;
                    }))
  with Sys_error message as exception_ ->
    fail
      (Error
         (Diagnostic.of_exception ~code:Diagnostic.Input_io
            ~phase:Diagnostic.Input
            ~message:("could not read scenario stream: " ^ message)
            exception_))
