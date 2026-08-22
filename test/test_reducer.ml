open Test_support
module T = Trading_engine
module Runner = T.Engine.Make (T.Scripted_strategy)

let runner ?contract_version ?(initial_cash = "10000") ?(risk = risk ())
    ?execution_model ?(execution = execution ()) schedule =
  let strategy_state = T.Scripted_strategy.create schedule |> ok in
  let config =
    engine_config ?contract_version ~risk ?execution_model ~execution ()
  in
  Runner.create ~run_id:(run_id "test-run") ~scenario_sha256 ~config
    ~initial_cash:[ ("USD", money initial_cash) ]
    ~strategy_state
  |> ok

let event_names events =
  List.map (fun event -> T.Audit.event_name event.T.Audit.event) events

let target quantity_value =
  T.Strategy.Target_quantities
    [
      T.Strategy.
        {
          instrument_id = instrument_id "test-equity";
          quantity = quantity quantity_value;
        };
    ]

let weight_target weight_value =
  T.Strategy.Target_weights
    [
      T.Strategy.
        {
          instrument_id = instrument_id "test-equity";
          weight = weight weight_value;
        };
    ]

let market_order_retries_after_partial_fill () =
  let state =
    runner
      ~execution:(execution ~participation_bps:5000 ())
      [ (1L, [ target "10" ]) ]
  in
  let state, first_events =
    Runner.process_slice state (market_slice 1L) |> ok
  in
  Alcotest.(check (list string))
    "first audit order"
    [
      "run_started";
      "market_slice_received";
      "target_portfolio_requested";
      "order_accepted";
      "valuation";
    ]
    (event_names first_events);
  let accepted = List.hd (T.Oms.active_orders (Runner.oms state)) in
  Alcotest.(check int64)
    "eligible after first slice" 1L accepted.eligible_after_slice_sequence;
  let state, second_events =
    Runner.process_slice state
      (market_slice
         ~bars:
           [ bar ~open_price:"103" ~close_price:"107" ~volume:(Some "12") 2L ]
         2L)
    |> ok
  in
  Alcotest.check quantity_testable "six shares filled" (quantity "6")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check (list string))
    "partial fill is cancelled and retried"
    [
      "market_slice_received";
      "fill_applied";
      "order_cancelled";
      "order_accepted";
      "valuation";
    ]
    (event_names second_events);
  let retry = List.hd (T.Oms.active_orders (Runner.oms state)) in
  Alcotest.check quantity_testable "retry preserves desired remainder"
    (quantity "4") retry.request.quantity

let partial_limit_persists () =
  let limit_request =
    request ~quantity_value:"10" ~kind:(T.Order.Limit (price "100")) ()
  in
  let state =
    runner
      ~execution:(execution ~participation_bps:10_000 ())
      [ (1L, [ T.Strategy.Submit_order limit_request ]) ]
  in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let state, _ =
    Runner.process_slice state
      (market_slice
         ~bars:
           [
             bar ~open_price:"105" ~low_price:"99" ~close_price:"101"
               ~volume:(Some "4") 2L;
           ]
         2L)
    |> ok
  in
  let partial = List.hd (T.Oms.active_orders (Runner.oms state)) in
  Alcotest.check quantity_testable "four filled" (quantity "4")
    partial.filled_quantity;
  Alcotest.(check string)
    "limit remains active" "partially_filled"
    (T.Order.status_to_string partial.status)

let weight_target_uses_current_equity_and_close () =
  let state = runner ~initial_cash:"1000" [ (1L, [ weight_target "0.5" ]) ] in
  let state, events =
    Runner.process_slice state
      (market_slice ~bars:[ bar ~close_price:"100" 1L ] 1L)
    |> ok
  in
  let order = List.hd (T.Oms.active_orders (Runner.oms state)) in
  Alcotest.check quantity_testable "half equity buys five" (quantity "5")
    order.request.quantity;
  let target_event =
    List.find
      (fun audit ->
        String.equal
          (T.Audit.event_name audit.T.Audit.event)
          "target_portfolio_requested")
      events
  in
  match target_event.event with
  | T.Audit.Target_portfolio_requested
      {
        basis = T.Audit.Weights;
        targets =
          [ { weight = Some target_weight; reference_price = Some mark; _ } ];
      } ->
      Alcotest.(check string)
        "weight retained" "0.5"
        (T.Scalar.Weight.to_decimal_string target_weight);
      Alcotest.check price_testable "reference close retained" (price "100")
        mark
  | _ -> Alcotest.fail "expected weight target audit"

let bounded_target_orders_make_progress () =
  let constrained = risk ~max_order:"10" ~max_long:"100" () in
  let state = runner ~risk:constrained [ (1L, [ target "25" ]) ] in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let state, _ = Runner.process_slice state (market_slice 2L) |> ok in
  let state, _ = Runner.process_slice state (market_slice 3L) |> ok in
  let state, _ = Runner.process_slice state (market_slice 4L) |> ok in
  Alcotest.check quantity_testable "bounded chunks reach target" (quantity "25")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  let quantities =
    T.Oms.orders (Runner.oms state)
    |> List.map (fun order ->
        T.Scalar.Quantity.to_decimal_string order.T.Order.request.quantity)
  in
  Alcotest.(check (list string)) "ten, ten, five" [ "10"; "10"; "5" ] quantities

let superseding_target_replaces_retry () =
  let constrained = risk ~max_order:"10" ~max_long:"100" () in
  let state =
    runner ~risk:constrained [ (1L, [ target "25" ]); (2L, [ target "5" ]) ]
  in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let state, events = Runner.process_slice state (market_slice 2L) |> ok in
  Alcotest.check quantity_testable "first chunk filled" (quantity "10")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  let active = T.Oms.active_orders (Runner.oms state) in
  match active with
  | [ order ] ->
      Alcotest.(check string)
        "replacement reverses side" "sell"
        (T.Order.side_to_string order.request.side);
      Alcotest.check quantity_testable "replacement quantity" (quantity "5")
        order.request.quantity;
      Alcotest.(check int)
        "one target request in superseding slice" 1
        (List.length
           (List.filter
              (fun name -> String.equal name "target_portfolio_requested")
              (event_names events)))
  | _ -> Alcotest.fail "expected one replacement order"

let fill_limit_clips_buy_to_lots ?(contract_version = T.Contract.version) () =
  let constrained = risk ~max_leverage:"1" () in
  let state =
    runner ~contract_version ~initial_cash:"550" ~risk:constrained
      ~execution:(execution ~fixed_fee:"10" ())
      [ (1L, [ target "10" ]) ]
  in
  let decision_bar =
    bar ~open_price:"50" ~high_price:"50" ~low_price:"50" ~close_price:"50" 1L
  in
  let state, _ =
    Runner.process_slice state (market_slice ~bars:[ decision_bar ] 1L) |> ok
  in
  let state, events =
    Runner.process_slice state
      (market_slice ~bars:[ bar ~open_price:"100" ~close_price:"100" 2L ] 2L)
    |> ok
  in
  Alcotest.check quantity_testable "five risk-permitted shares" (quantity "5")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.check money_testable "cash after clipped fill" (money "40")
    (account_cash (Runner.account state));
  let limited =
    List.find
      (fun audit ->
        String.equal (T.Audit.event_name audit.T.Audit.event) "fill_clipped")
      events
  in
  Alcotest.(check string)
    "fill-clipped contract version" contract_version limited.contract_version;
  match limited.event with
  | T.Audit.Fill_clipped
      {
        proposed_quantity;
        permitted_quantity;
        price = fill_price;
        limit = T.Risk.Maximum_leverage threshold;
        _;
      } ->
      Alcotest.check quantity_testable "ten proposed" (quantity "10")
        proposed_quantity;
      Alcotest.check quantity_testable "five permitted" (quantity "5")
        permitted_quantity;
      Alcotest.check price_testable "actual price" (price "100") fill_price;
      Alcotest.(check string)
        "leverage threshold" "1"
        (T.Scalar.Ratio.to_decimal_string threshold)
  | _ -> Alcotest.fail "expected leverage clipping audit"

let v4_replays_keep_the_fill_clipped_record () =
  fill_limit_clips_buy_to_lots ~contract_version:T.Contract.previous_version ()

let v3_replays_keep_the_legacy_clipping_record () =
  let constrained = risk ~max_leverage:"1" () in
  let state =
    runner ~contract_version:T.Contract.legacy_journal_version
      ~initial_cash:"550" ~risk:constrained
      ~execution:(execution ~fixed_fee:"10" ())
      [ (1L, [ target "10" ]) ]
  in
  let decision_bar =
    bar ~open_price:"50" ~high_price:"50" ~low_price:"50" ~close_price:"50" 1L
  in
  let state, _ =
    Runner.process_slice state (market_slice ~bars:[ decision_bar ] 1L) |> ok
  in
  let _, events =
    Runner.process_slice state
      (market_slice ~bars:[ bar ~open_price:"100" ~close_price:"100" 2L ] 2L)
    |> ok
  in
  let limited =
    List.find
      (fun audit ->
        String.equal (T.Audit.event_name audit.T.Audit.event) "margin_limited")
      events
  in
  Alcotest.(check string)
    "legacy journal version" T.Contract.legacy_journal_version
    limited.contract_version;
  match limited.event with
  | T.Audit.Margin_limited { requested_quantity; permitted_quantity; _ } ->
      Alcotest.check quantity_testable "legacy requested" (quantity "10")
        requested_quantity;
      Alcotest.check quantity_testable "legacy permitted" (quantity "5")
        permitted_quantity
  | _ -> Alcotest.fail "expected legacy margin_limited audit"

let invalid_fill_candidates_fail_instead_of_clipping () =
  let constrained = risk ~max_leverage:"1" () in
  let state =
    runner ~initial_cash:"9223372036854.775807" ~risk:constrained
      [
        ( 1L,
          [
            T.Strategy.Submit_order
              (request ~side:T.Order.Sell ~quantity_value:"1" ());
          ] );
      ]
  in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  Alcotest.(check string)
    "account overflow is not a clipping policy" "int64 addition overflow"
    (Runner.process_slice state (market_slice 2L) |> error)

let sells_precede_buys_in_the_same_slice () =
  let a = instrument ~id:"asset-a" ~symbol:"A" () in
  let b = instrument ~id:"asset-b" ~symbol:"B" () in
  let configured = risk ~instruments:[ a; b ] ~max_long:"100" () in
  let portfolio a_quantity b_quantity =
    T.Strategy.Target_quantities
      [
        T.Strategy.{ instrument_id = a.id; quantity = quantity a_quantity };
        T.Strategy.{ instrument_id = b.id; quantity = quantity b_quantity };
      ]
  in
  let bars sequence =
    [
      bar ~instrument:a.id ~open_price:"100" ~close_price:"100" sequence;
      bar ~instrument:b.id ~open_price:"100" ~close_price:"100" sequence;
    ]
  in
  let state =
    runner ~initial_cash:"1000" ~risk:configured
      [ (1L, [ portfolio "10" "0" ]); (2L, [ portfolio "0" "10" ]) ]
  in
  let state, _ =
    Runner.process_slice state (market_slice ~bars:(bars 1L) 1L) |> ok
  in
  let state, _ =
    Runner.process_slice state (market_slice ~bars:(bars 2L) 2L) |> ok
  in
  Alcotest.check money_testable "cash fully invested" (money "0")
    (account_cash (Runner.account state));
  let state, events =
    Runner.process_slice state (market_slice ~bars:(bars 3L) 3L) |> ok
  in
  Alcotest.check quantity_testable "sold first asset" (quantity "0")
    (T.Account.position_quantity (Runner.account state) a.id);
  Alcotest.check quantity_testable "bought second asset" (quantity "10")
    (T.Account.position_quantity (Runner.account state) b.id);
  Alcotest.(check (list string))
    "sell fill precedes buy fill" [ "sell"; "buy" ]
    (List.filter_map
       (fun audit ->
         match audit.T.Audit.event with
         | T.Audit.Fill_applied fill -> Some (T.Order.side_to_string fill.side)
         | _ -> None)
       events)

let external_ordering_is_validated () =
  let state = runner [] in
  let state, _ = Runner.process_slice state (market_slice 2L) |> ok in
  Alcotest.(check bool)
    "slice sequence cannot repeat" true
    (Result.is_error (Runner.process_slice state (market_slice 2L)))

let interactive_market_slice_timeline_is_non_overlapping () =
  let config = engine_config () in
  let initial =
    T.Engine.Interactive.create ~run_id:(run_id "timeline-test")
      ~scenario_sha256 ~config
      ~initial_cash:[ ("USD", money "10000") ]
    |> ok
  in
  let rec finish progress =
    match T.Engine.Interactive.strategy_request progress with
    | Some _ -> T.Engine.Interactive.resume progress [] |> ok |> finish
    | None -> (
        match T.Engine.Interactive.slice_result progress with
        | Some (state, _) -> state
        | None -> Alcotest.fail "expected completed interactive slice")
  in
  let state =
    T.Engine.Interactive.process_slice initial (market_slice 1L) |> ok |> finish
  in
  List.iter
    (fun (label, start_at) ->
      Alcotest.(check string)
        label "market slice start must not precede previous end"
        (T.Engine.Interactive.process_slice state
           (market_slice ~start_at:(timestamp start_at) 2L)
        |> error))
    [
      ("backward start rejected", "2026-01-01T14:30:00Z");
      ("overlapping start rejected", "2026-01-02T20:00:00Z");
    ];
  Alcotest.(check bool)
    "equal boundary accepted" true
    (Result.is_ok
       (T.Engine.Interactive.process_slice state
          (market_slice ~start_at:(timestamp "2026-01-02T21:00:00Z") 2L)))

module Looping_strategy = struct
  type state = T.Order.request

  let name = "looping"

  let on_event request _context = function
    | T.Strategy.Market_slice_closed _ ->
        (request, [ T.Strategy.Submit_order request ])
    | T.Strategy.Order_updated _ ->
        (request, [ T.Strategy.Submit_order request ])
    | T.Strategy.Fill_received _ | T.Strategy.Intent_rejected _ -> (request, [])
end

module Looping_runner = T.Engine.Make (Looping_strategy)

let internal_feedback_is_capped () =
  let strategy_state = request ~quantity_value:"1" () in
  let config = engine_config ~max_internal_events:3 () in
  let state =
    Looping_runner.create ~run_id:(run_id "loop") ~scenario_sha256 ~config
      ~initial_cash:[ ("USD", money "10000") ]
      ~strategy_state
    |> ok
  in
  Alcotest.(check bool)
    "feedback loop rejected" true
    (Result.is_error (Looping_runner.process_slice state (market_slice 1L)))

let exact_internal_event_limit_succeeds () =
  let strategy_state = T.Scripted_strategy.create [] |> ok in
  let config = engine_config ~max_internal_events:1 () in
  let state =
    Runner.create ~run_id:(run_id "one-event") ~scenario_sha256 ~config
      ~initial_cash:[ ("USD", money "10000") ]
      ~strategy_state
    |> ok
  in
  Alcotest.(check bool)
    "one callback fits a limit of one" true
    (Result.is_ok (Runner.process_slice state (market_slice 1L)))

let reducer_feedback_queue_handles_large_batches () =
  let batch_size = T.Resource_limits.intents_per_batch in
  let invalid_intent =
    T.Strategy.Submit_order
      (request ~quantity_value:"1" ~origin:T.Order.Target_rebalance ())
  in
  let intents = List.init batch_size (fun _ -> invalid_intent) in
  let strategy_state = T.Scripted_strategy.create [ (1L, intents) ] |> ok in
  let max_internal_events = (2 * batch_size) + 1 in
  let config = engine_config ~max_internal_events () in
  let state =
    Runner.create ~run_id:(run_id "large-feedback") ~scenario_sha256 ~config
      ~initial_cash:[ ("USD", money "10000") ]
      ~strategy_state
    |> ok
  in
  let _, events = Runner.process_slice state (market_slice 1L) |> ok in
  let rejection_count =
    List.fold_left
      (fun count audit ->
        match audit.T.Audit.event with
        | T.Audit.Intent_rejected _ -> count + 1
        | _ -> count)
      0 events
  in
  Alcotest.(check int) "every intent rejected" batch_size rejection_count;
  Alcotest.(check int)
    "batch completes at the exact feedback limit" (batch_size + 3)
    (List.length events)

let completed_run_is_terminal_and_hash_bound () =
  let state = runner [] in
  let state, valuation, events = Runner.complete state |> ok in
  Alcotest.(check (list string))
    "start and completion events"
    [ "run_started"; "run_completed" ]
    (event_names events);
  Alcotest.check money_testable "initial equity" (money "10000")
    valuation.equity;
  (match ((List.hd events).event, (List.rev events |> List.hd).event) with
  | ( T.Audit.Run_started { scenario_sha256 = started },
      T.Audit.Run_completed { scenario_sha256 = completed; _ } ) ->
      Alcotest.(check string) "start hash" scenario_sha256 started;
      Alcotest.(check string) "completion hash" scenario_sha256 completed
  | _ -> Alcotest.fail "expected hash-bound terminal records");
  Alcotest.(check bool)
    "later slice rejected" true
    (Result.is_error (Runner.process_slice state (market_slice 1L)));
  Alcotest.(check bool)
    "second completion rejected" true
    (Result.is_error (Runner.complete state))

let invalid_initial_state_is_rejected () =
  let strategy_state = T.Scripted_strategy.create [] |> ok in
  let config = engine_config () in
  Alcotest.(check bool)
    "negative cash rejected" true
    (Result.is_error
       (Runner.create ~run_id:(run_id "bad-cash") ~scenario_sha256 ~config
          ~initial_cash:[ ("USD", money "-1") ]
          ~strategy_state));
  Alcotest.(check bool)
    "noncanonical hash rejected" true
    (Result.is_error
       (Runner.create ~run_id:(run_id "bad-hash")
          ~scenario_sha256:(String.make 64 'A') ~config
          ~initial_cash:[ ("USD", money "1") ]
          ~strategy_state))

let one_valuation_per_slice () =
  let state = runner [] in
  let state, first = Runner.process_slice state (market_slice 1L) |> ok in
  let _, second = Runner.process_slice state (market_slice 2L) |> ok in
  let count events =
    List.length
      (List.filter
         (fun name -> String.equal name "valuation")
         (event_names events))
  in
  Alcotest.(check int) "first slice" 1 (count first);
  Alcotest.(check int) "second slice" 1 (count second)

module No_fill_execution = struct
  let name = "test_no_fill"

  let start_slice _execution ~instruments:_ ~oms:_ _slice =
    Ok (T.Execution.finished [])
end

let configured_execution_model_is_dispatched () =
  let execution_model =
    T.Execution_model.of_module (module No_fill_execution)
  in
  let direct = T.Strategy.Submit_order (request ~quantity_value:"3" ()) in
  let state = runner ~execution_model [ (1L, [ direct ]) ] in
  let state, first = Runner.process_slice state (market_slice 1L) |> ok in
  let state, second = Runner.process_slice state (market_slice 2L) |> ok in
  Alcotest.check quantity_testable "custom model applies no fills"
    T.Scalar.Quantity.zero
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check int)
    "order remains working" 1
    (List.length (T.Oms.active_orders (Runner.oms state)));
  Alcotest.(check bool)
    "no fill audit" false
    (List.exists
       (fun name -> String.equal name "fill_applied")
       (event_names (first @ second)));
  match (List.hd first).event with
  | T.Audit.Run_started { execution_model = actual; _ } ->
      Alcotest.(check string)
        "selected model is audited" No_fill_execution.name actual
  | _ -> Alcotest.fail "expected run start"

let execution_model_configuration_must_match () =
  let completed = T.Execution_model.find "completed_bar_v1" |> ok in
  let next_open = T.Execution_model.find "completed_bar_next_open_v1" |> ok in
  let conservative =
    T.Execution.create_conservative ~participation_bps:10_000 ~fee_schedules:[]
      ~half_spread_bps:0 ~impact_coefficient_bps:0
      ~missing_volume_policy:T.Execution.Reject_missing_volume
    |> ok
  in
  let configure contract_version execution_model execution =
    T.Engine.config ~contract_version ~risk:(risk ()) ~execution_model
      ~execution ~max_internal_events:1000
  in
  Alcotest.(check bool)
    "conservative model requires pricing configuration" true
    (Result.is_error (configure "13" next_open (execution ())));
  Alcotest.(check bool)
    "legacy model rejects conservative pricing" true
    (Result.is_error (configure "13" completed conservative));
  Alcotest.(check bool)
    "conservative model is v13-only" true
    (Result.is_error (configure "12" next_open conservative));
  Alcotest.(check bool)
    "matching conservative configuration accepted" true
    (Result.is_ok (configure "13" next_open conservative))

module Cancel_next_strategy = struct
  type state = { submitted : bool; cancelled : bool }

  let name = "cancel-next"

  let orders =
    [
      T.Strategy.Submit_order (request ~quantity_value:"1" ());
      T.Strategy.Submit_order (request ~quantity_value:"1" ());
    ]

  let on_event state context = function
    | T.Strategy.Market_slice_closed slice
      when Int64.equal slice.T.Market_slice.slice_sequence 1L
           && not state.submitted ->
        ({ state with submitted = true }, orders)
    | T.Strategy.Fill_received _ when not state.cancelled -> (
        match T.Strategy.working_orders context with
        | [ order ] ->
            ( { state with cancelled = true },
              [ T.Strategy.Cancel_order order.T.Order.id ] )
        | _ -> Alcotest.fail "first fill must expose one remaining order")
    | _ -> (state, [])
end

module Cancel_next_runner = T.Engine.Make (Cancel_next_strategy)

let callbacks_use_current_slice_and_apply_responses_before_matching () =
  let configured = instrument ~currency:"EUR" () in
  let configured_risk = risk ~instruments:[ configured ] () in
  let config = engine_config ~risk:configured_risk () in
  let initial_cash = [ ("USD", money "10000"); ("EUR", money "0") ] in
  let first_slice =
    market_slice
      ~bars:[ bar ~close_price:"104" 1L ]
      ~fx_rates:[ fx_mark (); fx_mark ~currency:"EUR" ~rate:"1" () ]
      1L
  in
  let second_slice =
    market_slice
      ~bars:
        [
          bar ~open_price:"103" ~high_price:"108" ~low_price:"102"
            ~close_price:"107" 2L;
        ]
      ~fx_rates:[ fx_mark (); fx_mark ~currency:"EUR" ~rate:"2" () ]
      2L
  in
  let strategy_state =
    Cancel_next_strategy.{ submitted = false; cancelled = false }
  in
  let scripted =
    Cancel_next_runner.create
      ~run_id:(run_id "callback-consistency")
      ~scenario_sha256 ~config ~initial_cash ~strategy_state
    |> ok
  in
  let scripted, _ =
    Cancel_next_runner.process_slice scripted first_slice |> ok
  in
  let scripted, scripted_events =
    Cancel_next_runner.process_slice scripted second_slice |> ok
  in
  let interactive =
    T.Engine.Interactive.create
      ~run_id:(run_id "callback-consistency")
      ~scenario_sha256 ~config ~initial_cash
    |> ok
  in
  let rec finish_first submitted progress =
    match T.Engine.Interactive.strategy_request progress with
    | Some (_, T.Strategy.Market_slice_closed _) when not submitted ->
        T.Engine.Interactive.resume progress Cancel_next_strategy.orders
        |> ok |> finish_first true
    | Some _ ->
        T.Engine.Interactive.resume progress [] |> ok |> finish_first submitted
    | None -> (
        match T.Engine.Interactive.slice_result progress with
        | Some (state, _) -> state
        | None -> Alcotest.fail "expected completed first slice")
  in
  let interactive =
    T.Engine.Interactive.process_slice interactive first_slice
    |> ok |> finish_first false
  in
  let contexts_rev = ref [] in
  let rec finish_second cancelled progress =
    match T.Engine.Interactive.strategy_request progress with
    | Some (context, event) ->
        contexts_rev := (context, event) :: !contexts_rev;
        let intents, cancelled =
          match (event, cancelled, T.Strategy.working_orders context) with
          | T.Strategy.Fill_received _, false, [ order ] ->
              ([ T.Strategy.Cancel_order order.T.Order.id ], true)
          | _ -> ([], cancelled)
        in
        T.Engine.Interactive.resume progress intents
        |> ok |> finish_second cancelled
    | None -> (
        match T.Engine.Interactive.slice_result progress with
        | Some result -> result
        | None -> Alcotest.fail "expected completed second slice")
  in
  let interactive, interactive_events =
    T.Engine.Interactive.process_slice interactive second_slice
    |> ok |> finish_second false
  in
  Alcotest.(check (list string))
    "scripted and interactive audit bytes"
    (List.map T.Codec.audit_to_string scripted_events)
    (List.map T.Codec.audit_to_string interactive_events);
  Alcotest.check quantity_testable "only the first order fills" (quantity "1")
    (T.Account.position_quantity
       (T.Engine.Interactive.account interactive)
       configured.id);
  Alcotest.check quantity_testable "scripted result matches" (quantity "1")
    (T.Account.position_quantity
       (Cancel_next_runner.account scripted)
       configured.id);
  Alcotest.(check int)
    "one fill audit" 1
    (List.length
       (List.filter
          (fun audit ->
            match audit.T.Audit.event with
            | T.Audit.Fill_applied _ -> true
            | _ -> false)
          interactive_events));
  let contexts = List.rev !contexts_rev in
  Alcotest.(check int) "four slice callbacks" 4 (List.length contexts);
  List.iter
    (fun (context, _) ->
      Alcotest.(check string)
        "callback clock"
        (T.Codec.ptime_to_string second_slice.received_at)
        (T.Codec.ptime_to_string (T.Strategy.now context));
      let latest_bar =
        T.Strategy.latest_bar context configured.id |> Option.get
      in
      Alcotest.check price_testable "current close" (price "107")
        latest_bar.close_price;
      let portfolio = T.Strategy.portfolio context in
      let position =
        List.find
          (fun (position : T.Strategy.marked_position) ->
            T.Id.Instrument.equal position.T.Strategy.instrument_id
              configured.id)
          portfolio.positions
      in
      Alcotest.check price_testable "current position mark" (price "107")
        position.mark;
      let euro =
        List.find
          (fun (balance : T.Account.cash_attribution) ->
            String.equal balance.T.Account.currency "EUR")
          portfolio.cash_balances
      in
      Alcotest.check price_testable "current FX mark" (price "2") euro.fx_rate)
    contexts;
  match contexts with
  | (fill_context, T.Strategy.Fill_received _)
    :: (order_context, T.Strategy.Order_updated _)
    :: (_, T.Strategy.Order_updated cancelled_order)
    :: [ (_, T.Strategy.Market_slice_closed _) ] ->
      Alcotest.(check int)
        "second order is working at first fill" 1
        (List.length (T.Strategy.working_orders fill_context));
      Alcotest.(check int)
        "cancellation is visible to the next callback" 0
        (List.length (T.Strategy.working_orders order_context));
      Alcotest.(check string)
        "second order is cancelled" "cancelled"
        (T.Order.status_to_string cancelled_order.status)
  | _ -> Alcotest.fail "unexpected callback order"

let interactive_reducer_matches_scripted_strategy () =
  let scripted = runner [ (1L, [ target "7" ]) ] in
  let scripted, scripted_events =
    Runner.process_slice scripted (market_slice 1L) |> ok
  in
  let config = engine_config () in
  let interactive =
    T.Engine.Interactive.create ~run_id:(run_id "test-run") ~scenario_sha256
      ~config
      ~initial_cash:[ ("USD", money "10000") ]
    |> ok
  in
  let progress =
    T.Engine.Interactive.process_slice interactive (market_slice 1L) |> ok
  in
  let rec drive sent_target progress =
    match T.Engine.Interactive.strategy_request progress with
    | Some (_, T.Strategy.Market_slice_closed _) when not sent_target ->
        T.Engine.Interactive.resume progress [ target "7" ] |> ok |> drive true
    | Some _ ->
        T.Engine.Interactive.resume progress [] |> ok |> drive sent_target
    | None -> (
        match T.Engine.Interactive.slice_result progress with
        | Some result -> result
        | None -> Alcotest.fail "expected completed interactive slice")
  in
  let interactive, interactive_events = drive false progress in
  Alcotest.(check (list string))
    "audit bytes"
    (List.map T.Codec.audit_to_string scripted_events)
    (List.map T.Codec.audit_to_string interactive_events);
  Alcotest.check money_testable "account cash"
    (account_cash (Runner.account scripted))
    (account_cash (T.Engine.Interactive.account interactive));
  let completed_progress =
    match T.Engine.Interactive.process_slice interactive (market_slice 2L) with
    | Error message -> Alcotest.fail message
    | Ok progress ->
        let rec finish progress =
          match T.Engine.Interactive.strategy_request progress with
          | Some _ -> T.Engine.Interactive.resume progress [] |> ok |> finish
          | None -> progress
        in
        finish progress
  in
  Alcotest.(check string)
    "completed progress rejects a response"
    "completed slice cannot accept strategy intents"
    (T.Engine.Interactive.resume completed_progress [] |> error)

let explicit_phase_order_is_stable () =
  let dividend =
    T.Corporate_action.cash_dividend
      ~id:(T.Id.Corporate_action.of_string_exn "phase-dividend")
      ~instrument_id:(instrument_id "test-equity")
      ~amount_per_unit:(money "1")
    |> ok
  in
  let metric =
    T.Metric.create ~name:"phase.boundary" ~value:(T.Metric.String "reached") ()
    |> ok
    |> fun metric -> T.Strategy.Emit_metric metric
  in
  let state = runner [ (1L, [ target "2" ]); (2L, [ metric; target "0" ]) ] in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let _, events =
    Runner.process_slice state (market_slice ~corporate_actions:[ dividend ] 2L)
    |> ok
  in
  Alcotest.(check (list string))
    "actions, matching, notifications, targets, and valuation stay ordered"
    [
      "market_slice_received";
      "cash_dividend_applied";
      "fill_applied";
      "metric_emitted";
      "target_portfolio_requested";
      "order_accepted";
      "valuation";
    ]
    (event_names events)

let tests =
  [
    Alcotest.test_case "market target retries after partial fill" `Quick
      market_order_retries_after_partial_fill;
    Alcotest.test_case "partial limit persists" `Quick partial_limit_persists;
    Alcotest.test_case "weight sizing uses current equity" `Quick
      weight_target_uses_current_equity_and_close;
    Alcotest.test_case "bounded target progress" `Quick
      bounded_target_orders_make_progress;
    Alcotest.test_case "superseding target replaces retry" `Quick
      superseding_target_replaces_retry;
    Alcotest.test_case "fill clipping identifies leverage" `Quick
      fill_limit_clips_buy_to_lots;
    Alcotest.test_case "v4 keeps fill clipping records" `Quick
      v4_replays_keep_the_fill_clipped_record;
    Alcotest.test_case "v3 keeps legacy clipping records" `Quick
      v3_replays_keep_the_legacy_clipping_record;
    Alcotest.test_case "invalid fill candidates fail" `Quick
      invalid_fill_candidates_fail_instead_of_clipping;
    Alcotest.test_case "same-slice sells precede buys" `Quick
      sells_precede_buys_in_the_same_slice;
    Alcotest.test_case "external ordering validation" `Quick
      external_ordering_is_validated;
    Alcotest.test_case "interactive market slice timeline is non-overlapping"
      `Quick interactive_market_slice_timeline_is_non_overlapping;
    Alcotest.test_case "internal feedback cap" `Quick
      internal_feedback_is_capped;
    Alcotest.test_case "exact internal event limit" `Quick
      exact_internal_event_limit_succeeds;
    Alcotest.test_case "large reducer feedback batch" `Slow
      reducer_feedback_queue_handles_large_batches;
    Alcotest.test_case "completed run is terminal and hash-bound" `Quick
      completed_run_is_terminal_and_hash_bound;
    Alcotest.test_case "invalid initial state rejected" `Quick
      invalid_initial_state_is_rejected;
    Alcotest.test_case "one valuation per slice" `Quick one_valuation_per_slice;
    Alcotest.test_case "configured execution model is dispatched" `Quick
      configured_execution_model_is_dispatched;
    Alcotest.test_case "execution model configuration matches" `Quick
      execution_model_configuration_must_match;
    Alcotest.test_case "callbacks use current slice and synchronous responses"
      `Quick callbacks_use_current_slice_and_apply_responses_before_matching;
    Alcotest.test_case "interactive reducer matches scripted strategy" `Quick
      interactive_reducer_matches_scripted_strategy;
    Alcotest.test_case "explicit phase order is stable" `Quick
      explicit_phase_order_is_stable;
  ]
