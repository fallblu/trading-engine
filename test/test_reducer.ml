open Test_support
module T = Trading_engine
module Runner = T.Engine.Make (T.Scripted_strategy)

let runner ?(initial_cash = "10000") ?(risk = risk ())
    ?(execution = execution ()) schedule =
  let strategy_state = T.Scripted_strategy.create schedule |> ok in
  let config = engine_config ~risk ~execution () in
  Runner.create ~run_id:(run_id "test-run") ~scenario_sha256 ~config
    ~initial_cash:(money initial_cash) ~strategy_state
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
  let constrained = risk ~max_order:"10" ~max_position:"100" () in
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
        T.Scalar.Quantity.to_string order.T.Order.request.quantity)
  in
  Alcotest.(check (list string)) "ten, ten, five" [ "10"; "10"; "5" ] quantities

let superseding_target_replaces_retry () =
  let constrained = risk ~max_order:"10" ~max_position:"100" () in
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

let cash_limit_clips_buy_to_whole_lots () =
  let state =
    runner ~initial_cash:"550"
      ~execution:(execution ~fixed_fee:"10" ())
      [ (1L, [ target "10" ]) ]
  in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let state, events =
    Runner.process_slice state
      (market_slice ~bars:[ bar ~open_price:"100" ~close_price:"100" 2L ] 2L)
    |> ok
  in
  Alcotest.check quantity_testable "five affordable shares" (quantity "5")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.check money_testable "cash stays nonnegative" (money "40")
    (T.Account.cash (Runner.account state));
  let limited =
    List.find
      (fun audit ->
        String.equal (T.Audit.event_name audit.T.Audit.event) "cash_limited")
      events
  in
  match limited.event with
  | T.Audit.Cash_limited
      { requested_quantity; affordable_quantity; price = fill_price; _ } ->
      Alcotest.check quantity_testable "ten requested" (quantity "10")
        requested_quantity;
      Alcotest.check quantity_testable "five affordable" (quantity "5")
        affordable_quantity;
      Alcotest.check price_testable "actual price" (price "100") fill_price
  | _ -> Alcotest.fail "expected cash limit audit"

let sells_fund_buys_in_the_same_slice () =
  let a = instrument ~id:"asset-a" ~symbol:"A" () in
  let b = instrument ~id:"asset-b" ~symbol:"B" () in
  let configured = risk ~instruments:[ a; b ] ~max_position:"100" () in
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
    (T.Account.cash (Runner.account state));
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
      ~initial_cash:(money "10000") ~strategy_state
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
      ~initial_cash:(money "10000") ~strategy_state
    |> ok
  in
  Alcotest.(check bool)
    "one callback fits a limit of one" true
    (Result.is_ok (Runner.process_slice state (market_slice 1L)))

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
          ~initial_cash:(money "-1") ~strategy_state));
  Alcotest.(check bool)
    "noncanonical hash rejected" true
    (Result.is_error
       (Runner.create ~run_id:(run_id "bad-hash")
          ~scenario_sha256:(String.make 64 'A') ~config
          ~initial_cash:(money "1") ~strategy_state))

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
    Alcotest.test_case "cash limit clips buys" `Quick
      cash_limit_clips_buy_to_whole_lots;
    Alcotest.test_case "same-slice sells fund buys" `Quick
      sells_fund_buys_in_the_same_slice;
    Alcotest.test_case "external ordering validation" `Quick
      external_ordering_is_validated;
    Alcotest.test_case "internal feedback cap" `Quick
      internal_feedback_is_capped;
    Alcotest.test_case "exact internal event limit" `Quick
      exact_internal_event_limit_succeeds;
    Alcotest.test_case "completed run is terminal and hash-bound" `Quick
      completed_run_is_terminal_and_hash_bound;
    Alcotest.test_case "invalid initial state rejected" `Quick
      invalid_initial_state_is_rejected;
    Alcotest.test_case "one valuation per slice" `Quick one_valuation_per_slice;
  ]
