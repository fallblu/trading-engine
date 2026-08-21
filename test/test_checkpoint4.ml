open Test_support
module T = Trading_engine
module Runner = T.Engine.Make (T.Scripted_strategy)

module Margin_observing_strategy = struct
  type state = { saw_liquidation_update : bool }

  let name = "margin-observer"
  let initial = { saw_liquidation_update = false }

  let on_event state _context event =
    match event with
    | T.Strategy.Market_slice_closed market_slice
      when Int64.equal market_slice.T.Market_slice.slice_sequence 1L ->
        ( state,
          [
            T.Strategy.Target_quantities
              [
                T.Strategy.
                  {
                    instrument_id = instrument_id "test-equity";
                    quantity = quantity "15";
                  };
              ];
          ] )
    | T.Strategy.Order_updated order
      when order.T.Order.request.origin = T.Order.Margin_liquidation ->
        ({ saw_liquidation_update = true }, [])
    | T.Strategy.Market_slice_closed _ | T.Strategy.Fill_received _
    | T.Strategy.Order_updated _ | T.Strategy.Intent_rejected _ ->
        (state, [])
end

module Margin_runner = T.Engine.Make (Margin_observing_strategy)

let euro_instrument () =
  instrument ~id:"euro-equity" ~symbol:"EURO" ~currency:"EUR" ~lot_size:"0.001"
    ()

let multi_currency_fractional_accounting () =
  let euro = euro_instrument () in
  let account =
    test_account
      ~initial_cash:[ ("EUR", money "100"); ("USD", money "1000") ]
      ()
  in
  let order =
    request ~instrument:euro.id ~quantity_value:"1.5" () |> accepted_order
  in
  let execution =
    T.Fill.create ~id:(fill_id "euro-fill") ~order_id:order.id
      ~instrument_id:euro.id ~quote_currency:"EUR" ~side:T.Order.Buy
      ~quantity:(quantity "1.5") ~price:(price "20") ~fee:(money "0.5")
      ~executed_at:(timestamp "2026-01-03T14:30:00Z")
      ~slice_sequence:2L
    |> ok
  in
  let account = T.Account.apply_fill account execution |> ok in
  Alcotest.check money_testable "EUR settlement" (money "69.5")
    (account_cash ~currency:"EUR" account);
  let valuation =
    account_value ~instruments:[ euro ]
      ~fx_rates:[ ("EUR", price "1.2"); ("USD", price "1") ]
      account
      ~marks:[ (euro.id, price "22") ]
  in
  Alcotest.check money_testable "base cash" (money "1083.4") valuation.cash;
  Alcotest.check money_testable "base market value" (money "39.6")
    valuation.net_market_value;
  Alcotest.check money_testable "base equity" (money "1123") valuation.equity

let split_and_dividend_accounting () =
  let account = test_account () in
  let order = request ~quantity_value:"1.5" () |> accepted_order in
  let account =
    T.Account.apply_fill account
      (fill ~quantity_value:"1.5" ~price_value:"100" order)
    |> ok
  in
  let account =
    T.Account.apply_split account
      ~instrument_id:(instrument_id "test-equity")
      ~numerator:3L ~denominator:2L
    |> ok
  in
  let position = T.Account.position account (instrument_id "test-equity") in
  Alcotest.check quantity_testable "split quantity" (quantity "2.25")
    position.quantity;
  Alcotest.check money_testable "split preserves basis" (money "150")
    position.cost_basis;
  let account =
    T.Account.apply_cash_dividend account
      ~instrument_id:(instrument_id "test-equity")
      ~quote_currency:"USD" ~amount_per_unit:(money "2")
    |> ok
  in
  Alcotest.check money_testable "long dividend cash" (money "9854.5")
    (account_cash account);
  let position = T.Account.position account (instrument_id "test-equity") in
  Alcotest.check money_testable "dividend attribution" (money "4.5")
    position.dividend_pnl;
  let short_account = test_account ~initial_cash:[ ("USD", money "1000") ] () in
  let short_order =
    request ~side:T.Order.Sell ~quantity_value:"2" () |> accepted_order
  in
  let short_account =
    T.Account.apply_fill short_account
      (fill ~quantity_value:"2" ~price_value:"100" short_order)
    |> ok
  in
  let short_account =
    T.Account.apply_cash_dividend short_account
      ~instrument_id:(instrument_id "test-equity")
      ~quote_currency:"USD" ~amount_per_unit:(money "1.5")
    |> ok
  in
  Alcotest.check money_testable "short dividend cash debit" (money "1197")
    (account_cash short_account);
  let short_position =
    T.Account.position short_account (instrument_id "test-equity")
  in
  Alcotest.check money_testable "short dividend attribution" (money "-3")
    short_position.dividend_pnl

let split_adjusts_working_order () =
  let action =
    T.Corporate_action.split
      ~id:(T.Id.Corporate_action.of_string_exn "split-2-for-1")
      ~instrument_id:(instrument_id "test-equity")
      ~numerator:2L ~denominator:1L
    |> ok
  in
  let strategy_state =
    T.Scripted_strategy.create
      [
        ( 1L,
          [
            T.Strategy.Submit_order
              (request ~quantity_value:"10"
                 ~kind:(T.Order.Limit (price "50"))
                 ());
          ] );
      ]
    |> ok
  in
  let config = engine_config () in
  let state =
    Runner.create ~run_id:(run_id "split-order") ~scenario_sha256 ~config
      ~initial_cash:[ ("USD", money "10000") ]
      ~strategy_state
    |> ok
  in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let split_slice = market_slice ~corporate_actions:[ action ] 2L in
  let state, events = Runner.process_slice state split_slice |> ok in
  let order = T.Oms.active_orders (Runner.oms state) |> List.hd in
  Alcotest.check quantity_testable "working quantity doubles" (quantity "20")
    order.request.quantity;
  (match order.request.kind with
  | T.Order.Limit limit ->
      Alcotest.check price_testable "limit price halves" (price "25") limit
  | T.Order.Market -> Alcotest.fail "expected adjusted limit order");
  Alcotest.(check (list string))
    "causal adjustment events"
    [ "market_slice_received"; "split_applied"; "order_adjusted"; "valuation" ]
    (List.map (fun event -> T.Audit.event_name event.T.Audit.event) events)

let split_action id ~numerator ~denominator =
  T.Corporate_action.split
    ~id:(T.Id.Corporate_action.of_string_exn id)
    ~instrument_id:(instrument_id "test-equity")
    ~numerator ~denominator
  |> ok

let split_caps_adjusted_market_fill () =
  let strategy_state =
    T.Scripted_strategy.create
      [
        ( 1L,
          [
            T.Strategy.Submit_order
              (request ~quantity_value:"10" ~kind:T.Order.Market ());
          ] );
      ]
    |> ok
  in
  let config = engine_config ~risk:(risk ~max_order:"10" ()) () in
  let state =
    Runner.create
      ~run_id:(run_id "split-market-cap")
      ~scenario_sha256 ~config
      ~initial_cash:[ ("USD", money "10000") ]
      ~strategy_state
    |> ok
  in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let action =
    split_action "market-forward-split" ~numerator:2L ~denominator:1L
  in
  let split_bar =
    bar ~open_price:"50" ~high_price:"50" ~low_price:"50" ~close_price:"50" 2L
  in
  let state, events =
    Runner.process_slice state
      (market_slice ~bars:[ split_bar ] ~corporate_actions:[ action ] 2L)
    |> ok
  in
  Alcotest.check quantity_testable "bounded market fill" (quantity "10")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  let order = T.Oms.orders (Runner.oms state) |> List.hd in
  Alcotest.check quantity_testable "adjusted market quantity" (quantity "20")
    order.request.quantity;
  Alcotest.check quantity_testable "market filled quantity" (quantity "10")
    order.filled_quantity;
  Alcotest.(check string)
    "market remainder cancelled" "cancelled"
    (T.Order.status_to_string order.status);
  let limited =
    List.find_map
      (fun event ->
        match event.T.Audit.event with
        | T.Audit.Margin_limited { requested_quantity; permitted_quantity; _ }
          ->
            Some (requested_quantity, permitted_quantity)
        | _ -> None)
      events
    |> Option.get
  in
  Alcotest.check quantity_testable "adjusted proposal" (quantity "20")
    (fst limited);
  Alcotest.check quantity_testable "maximum permitted fill" (quantity "10")
    (snd limited)

let split_caps_partially_filled_limit_remainder () =
  let strategy_state =
    T.Scripted_strategy.create
      [
        ( 1L,
          [
            T.Strategy.Submit_order
              (request ~quantity_value:"10"
                 ~kind:(T.Order.Limit (price "100"))
                 ());
          ] );
      ]
    |> ok
  in
  let config = engine_config ~risk:(risk ~max_order:"10" ()) () in
  let state =
    Runner.create
      ~run_id:(run_id "split-partial-limit")
      ~scenario_sha256 ~config
      ~initial_cash:[ ("USD", money "10000") ]
      ~strategy_state
    |> ok
  in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let partial_bar =
    bar ~open_price:"90" ~high_price:"100" ~low_price:"80" ~close_price:"90"
      ~volume:(Some "4") 2L
  in
  let state, _ =
    Runner.process_slice state (market_slice ~bars:[ partial_bar ] 2L) |> ok
  in
  let action =
    split_action "partial-forward-split" ~numerator:2L ~denominator:1L
  in
  let adjusted_bar =
    bar ~open_price:"50" ~high_price:"50" ~low_price:"50" ~close_price:"50" 3L
  in
  let state, events =
    Runner.process_slice state
      (market_slice ~bars:[ adjusted_bar ] ~corporate_actions:[ action ] 3L)
    |> ok
  in
  let order = T.Oms.active_orders (Runner.oms state) |> List.hd in
  Alcotest.check quantity_testable "split-adjusted total" (quantity "20")
    order.request.quantity;
  Alcotest.check quantity_testable "split-adjusted prior fill plus bounded fill"
    (quantity "18") order.filled_quantity;
  Alcotest.check quantity_testable "GTC remainder" (quantity "2")
    (T.Order.remaining_quantity order);
  let limited =
    List.find_map
      (fun event ->
        match event.T.Audit.event with
        | T.Audit.Margin_limited { requested_quantity; permitted_quantity; _ }
          ->
            Some (requested_quantity, permitted_quantity)
        | _ -> None)
      events
    |> Option.get
  in
  Alcotest.check quantity_testable "adjusted partial proposal" (quantity "12")
    (fst limited);
  Alcotest.check quantity_testable "bounded partial fill" (quantity "10")
    (snd limited);
  let state, _ =
    Runner.process_slice state (market_slice ~bars:[ adjusted_bar ] 4L) |> ok
  in
  let order = T.Oms.orders (Runner.oms state) |> List.hd in
  Alcotest.(check string)
    "limit completes on later slice" "filled"
    (T.Order.status_to_string order.status);
  Alcotest.check quantity_testable "final split-adjusted position"
    (quantity "20")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"))

let reverse_split_restores_order_below_maximum () =
  let strategy_state =
    T.Scripted_strategy.create
      [
        ( 1L,
          [
            T.Strategy.Submit_order
              (request ~quantity_value:"10"
                 ~kind:(T.Order.Limit (price "100"))
                 ());
          ] );
      ]
    |> ok
  in
  let config = engine_config ~risk:(risk ~max_order:"10" ()) () in
  let state =
    Runner.create
      ~run_id:(run_id "reverse-split-order")
      ~scenario_sha256 ~config
      ~initial_cash:[ ("USD", money "10000") ]
      ~strategy_state
    |> ok
  in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let forward = split_action "a-forward-split" ~numerator:2L ~denominator:1L in
  let no_fill_bar =
    bar ~open_price:"75" ~high_price:"75" ~low_price:"75" ~close_price:"75" 2L
  in
  let state, _ =
    Runner.process_slice state
      (market_slice ~bars:[ no_fill_bar ] ~corporate_actions:[ forward ] 2L)
    |> ok
  in
  let oversized = T.Oms.active_orders (Runner.oms state) |> List.hd in
  Alcotest.check quantity_testable "forward-adjusted above maximum"
    (quantity "20") oversized.request.quantity;
  let reverse = split_action "b-reverse-split" ~numerator:1L ~denominator:2L in
  let fill_bar =
    bar ~open_price:"100" ~high_price:"100" ~low_price:"100" ~close_price:"100"
      3L
  in
  let state, events =
    Runner.process_slice state
      (market_slice ~bars:[ fill_bar ] ~corporate_actions:[ reverse ] 3L)
    |> ok
  in
  let order = T.Oms.orders (Runner.oms state) |> List.hd in
  Alcotest.check quantity_testable "reverse-adjusted at maximum" (quantity "10")
    order.request.quantity;
  Alcotest.(check string)
    "reverse-adjusted order fills" "filled"
    (T.Order.status_to_string order.status);
  Alcotest.(check bool)
    "fill at maximum is not clipped" false
    (List.exists
       (fun event ->
         match event.T.Audit.event with
         | T.Audit.Margin_limited _ -> true
         | _ -> false)
       events)

let margin_call_forces_deterministic_liquidation () =
  let config = engine_config () in
  let state =
    Margin_runner.create ~run_id:(run_id "margin-call") ~scenario_sha256 ~config
      ~initial_cash:[ ("USD", money "1000") ]
      ~strategy_state:Margin_observing_strategy.initial
    |> ok
  in
  let state, _ = Margin_runner.process_slice state (market_slice 1L) |> ok in
  let stressed_bar =
    bar ~open_price:"100" ~high_price:"100" ~low_price:"40" ~close_price:"40" 2L
  in
  let state, call_events =
    Margin_runner.process_slice state (market_slice ~bars:[ stressed_bar ] 2L)
    |> ok
  in
  Alcotest.(check bool)
    "margin call emitted" true
    (List.exists
       (fun event ->
         String.equal (T.Audit.event_name event.T.Audit.event) "margin_call")
       call_events);
  let liquidation =
    T.Oms.active_orders (Margin_runner.oms state)
    |> List.find (fun order ->
        order.T.Order.request.origin = T.Order.Margin_liquidation)
  in
  Alcotest.(check string)
    "forced sell" "sell"
    (T.Order.side_to_string liquidation.request.side);
  Alcotest.(check bool)
    "strategy observes liquidation order updates" true
    (Margin_runner.strategy_state state).saw_liquidation_update;
  let liquidation_bar =
    bar ~open_price:"40" ~high_price:"40" ~low_price:"40" ~close_price:"40" 3L
  in
  let state, restored_events =
    Margin_runner.process_slice state
      (market_slice ~bars:[ liquidation_bar ] 3L)
    |> ok
  in
  Alcotest.check quantity_testable "position flattened" T.Scalar.Quantity.zero
    (T.Account.position_quantity
       (Margin_runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check bool)
    "margin restored emitted" true
    (List.exists
       (fun event ->
         String.equal (T.Audit.event_name event.T.Audit.event) "margin_restored")
       restored_events)

let short_borrow_accrues_before_matching () =
  let target =
    T.Strategy.Target_quantities
      [
        T.Strategy.
          {
            instrument_id = instrument_id "test-equity";
            quantity = quantity "-10";
          };
      ]
  in
  let strategy_state = T.Scripted_strategy.create [ (1L, [ target ]) ] |> ok in
  let config = engine_config ~risk:(risk ~short_borrow_bps:3650 ()) () in
  let state =
    Runner.create ~run_id:(run_id "short-borrow") ~scenario_sha256 ~config
      ~initial_cash:[ ("USD", money "1000") ]
      ~strategy_state
    |> ok
  in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let state, _ = Runner.process_slice state (market_slice 2L) |> ok in
  let state, events = Runner.process_slice state (market_slice 3L) |> ok in
  let borrow =
    List.find_map
      (fun event ->
        match event.T.Audit.event with
        | T.Audit.Borrow_fee_applied { fee; _ } -> Some fee
        | _ -> None)
      events
    |> Option.get
  in
  Alcotest.(check bool)
    "positive borrow fee" true
    (T.Scalar.Money.compare borrow (money "0") > 0);
  let position =
    T.Account.position (Runner.account state) (instrument_id "test-equity")
  in
  Alcotest.check money_testable "borrow fee attributed" borrow
    position.borrow_fees

let risk_allows_reducing_an_out_of_limit_position () =
  let configured_risk = risk ~max_long:"10" () in
  let account = test_account ~initial_cash:[ ("USD", money "10000") ] () in
  let oversized =
    request ~quantity_value:"20" ()
    |> accepted_order |> fill ~quantity_value:"20"
  in
  let account = T.Account.apply_fill account oversized |> ok in
  let reduction = request ~side:T.Order.Sell ~quantity_value:"1" () in
  Alcotest.(check (result unit string))
    "reduction accepted above cap" (Ok ())
    (risk_check configured_risk ~account ~oms:T.Oms.empty reduction);
  let increase = request ~side:T.Order.Buy ~quantity_value:"1" () in
  Alcotest.(check bool)
    "increase rejected above cap" true
    (Result.is_error
       (risk_check configured_risk ~account ~oms:T.Oms.empty increase))

let engine_requires_complete_currency_ledgers () =
  let instruments = [ instrument (); euro_instrument () ] in
  let config = engine_config ~risk:(risk ~instruments ()) () in
  let strategy_state = T.Scripted_strategy.create [] |> ok in
  Alcotest.(check bool)
    "missing EUR ledger rejected" true
    (Result.is_error
       (Runner.create ~run_id:(run_id "missing-ledger") ~scenario_sha256 ~config
          ~initial_cash:[ ("USD", money "1000") ]
          ~strategy_state))

let tests =
  [
    Alcotest.test_case "fractional multi-currency accounting" `Quick
      multi_currency_fractional_accounting;
    Alcotest.test_case "split and dividend accounting" `Quick
      split_and_dividend_accounting;
    Alcotest.test_case "split adjusts working order" `Quick
      split_adjusts_working_order;
    Alcotest.test_case "split caps adjusted market fill" `Quick
      split_caps_adjusted_market_fill;
    Alcotest.test_case "split caps partially filled limit remainder" `Quick
      split_caps_partially_filled_limit_remainder;
    Alcotest.test_case "reverse split restores order below maximum" `Quick
      reverse_split_restores_order_below_maximum;
    Alcotest.test_case "margin call forces liquidation" `Quick
      margin_call_forces_deterministic_liquidation;
    Alcotest.test_case "short borrow accrues" `Quick
      short_borrow_accrues_before_matching;
    Alcotest.test_case "risk allows reduction above position cap" `Quick
      risk_allows_reducing_an_out_of_limit_position;
    Alcotest.test_case "engine requires complete currency ledgers" `Quick
      engine_requires_complete_currency_ledgers;
  ]
