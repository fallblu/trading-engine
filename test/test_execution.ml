open Test_support
module T = Trading_engine

let proposal_price result =
  match result.T.Execution.fills with
  | [ fill ] -> fill.price
  | _ -> Alcotest.fail "expected one proposed fill"

let match_orders ?(configured = instrument ()) ?(engine = execution ()) ~oms
    market_slice =
  T.Execution.match_slice engine ~instruments:[ configured ] ~oms market_slice
  |> ok

let conservative_execution ?(half_spread_bps = 0) ?(impact_coefficient_bps = 0)
    ?(missing_volume_policy = T.Execution.Reject_missing_volume) () =
  let component =
    T.Fee_schedule.create_component ~name:"broker" ~currency:"USD"
      ~basis:(T.Fee_schedule.Fixed (money "0.1"))
      ~rounding:T.Fee_schedule.Up ~applicability:T.Fee_schedule.Any
    |> ok
  in
  let schedule =
    T.Fee_schedule.create ~schedule_id:"test-fees-v1"
      ~instrument_id:(instrument_id "test-equity")
      ~settlement_currency:"USD" ~minimum:None ~maximum:None
      ~components:[ component ]
    |> ok
  in
  T.Execution.create_conservative ~participation_bps:10_000
    ~fee_schedules:[ schedule ] ~half_spread_bps ~impact_coefficient_bps
    ~missing_volume_policy
  |> ok

let conservative_step start ?(kind = T.Order.Market) ?(side = T.Order.Buy)
    ?(slice = market_slice 2L) engine =
  let oms, _ = oms_with_order (request ~kind ~side ()) in
  let cursor = start engine ~instruments:[ instrument () ] ~oms slice |> ok in
  T.Execution.next cursor ~oms |> ok

let market_event_time second =
  timestamp (Printf.sprintf "2026-01-03T14:30:%02dZ" second)

let quote_event ?(sequence = 1L) ?(second = 1) ?(bid = "99")
    ?(bid_quantity = "5") ?(ask = "101") ?(ask_quantity = "5") () =
  let event_at = market_event_time second in
  T.Market_event.quote
    ~instrument_id:(instrument_id "test-equity")
    ~event_at ~available_at:event_at ~received_at:event_at
    ~ingest_sequence:sequence ~bid_price:(price bid)
    ~bid_quantity:(quantity bid_quantity) ~ask_price:(price ask)
    ~ask_quantity:(quantity ask_quantity)
  |> ok

let trade_event ?(sequence = 2L) ?(second = 2) ?(price_value = "100")
    ?(quantity_value = "5") ?(aggressor_side = T.Market_event.Unknown) () =
  let event_at = market_event_time second in
  T.Market_event.trade
    ~instrument_id:(instrument_id "test-equity")
    ~event_at ~available_at:event_at ~received_at:event_at
    ~ingest_sequence:sequence ~price:(price price_value)
    ~quantity:(quantity quantity_value) ~aggressor_side
  |> ok

let quote_trade_slice events =
  let base = market_slice 2L in
  T.Market_slice.create_v14 ~slice_sequence:base.slice_sequence
    ~start_at:base.start_at ~end_at:base.end_at ~available_at:base.available_at
    ~received_at:base.received_at ~bars:base.bars ~fx_rates:base.fx_rates
    ~corporate_actions:base.corporate_actions
    ~borrow_observations:base.borrow_observations
    ~cash_rate_observations:base.cash_rate_observations
    ~settlement_failures:base.settlement_failures
    ~lifecycle_events:base.lifecycle_events ~market_events:events
  |> ok

let quote_trade_execution ?(participation_bps = 10_000) () =
  let fees = conservative_execution () |> T.Execution.fee_schedules in
  T.Execution.create_v2 ~participation_bps ~fee_schedules:fees |> ok

let liquidity_name = function
  | T.Fee_schedule.Maker -> "maker"
  | Taker -> "taker"

let quote_trade_step ?(kind = T.Order.Market) ?(side = T.Order.Buy) events =
  let oms, _ = oms_with_order (request ~kind ~side ()) in
  let cursor =
    T.Execution.start_slice_quote_trade (quote_trade_execution ())
      ~instruments:[ instrument () ]
      ~oms (quote_trade_slice events)
    |> ok
  in
  T.Execution.next cursor ~oms |> ok

let quote_trade_consumes_displayed_liquidity () =
  match quote_trade_step [ quote_event ~ask_quantity:"3" () ] with
  | T.Execution.Proposed (proposal, _) ->
      Alcotest.check price_testable "buy executes at displayed ask"
        (price "101") proposal.price;
      Alcotest.check quantity_testable "displayed size caps fill" (quantity "3")
        proposal.quantity;
      Alcotest.(check string)
        "quote fill is taker" "taker"
        (liquidity_name proposal.liquidity);
      Alcotest.(check string)
        "economic event time" "2026-01-03T14:30:01.000000Z"
        (T.Codec.ptime_to_string proposal.executed_at)
  | _ -> Alcotest.fail "marketable quote did not produce a fill"

let quote_trade_passive_fills_require_aggressor_evidence () =
  let limit = T.Order.Limit (price "100") in
  let events =
    [
      quote_event ();
      trade_event ~price_value:"99" ();
      trade_event ~sequence:3L ~second:3 ~price_value:"99"
        ~aggressor_side:T.Market_event.Sell ();
    ]
  in
  match quote_trade_step ~kind:limit events with
  | T.Execution.Proposed (proposal, _) ->
      Alcotest.check price_testable "passive fill uses observed trade"
        (price "99") proposal.price;
      Alcotest.(check string)
        "trade fill is maker" "maker"
        (liquidity_name proposal.liquidity);
      Alcotest.(check string)
        "unknown aggressor was skipped" "2026-01-03T14:30:03.000000Z"
        (T.Codec.ptime_to_string proposal.executed_at)
  | _ -> Alcotest.fail "qualified passive trade did not produce a fill"

let quote_trade_sell_paths_use_bid_and_buy_aggressors () =
  (match quote_trade_step ~side:T.Order.Sell [ quote_event ~bid:"99" () ] with
  | T.Execution.Proposed (proposal, continue) -> (
      Alcotest.check price_testable "sell executes at displayed bid"
        (price "99") proposal.price;
      let cursor = continue proposal.quantity |> ok in
      match T.Execution.next cursor ~oms:T.Oms.empty |> ok with
      | T.Execution.Finished _ -> ()
      | _ -> Alcotest.fail "consumed quote should finish")
  | _ -> Alcotest.fail "sell quote did not produce a fill");
  let passive = T.Order.Limit (price "100") in
  match
    quote_trade_step ~kind:passive ~side:T.Order.Sell
      [ trade_event ~price_value:"101" ~aggressor_side:T.Market_event.Buy () ]
  with
  | T.Execution.Proposed (proposal, continue) ->
      Alcotest.check price_testable "passive sell uses trade price"
        (price "101") proposal.price;
      Alcotest.(check string)
        "passive sell is maker" "maker"
        (liquidity_name proposal.liquidity);
      ignore (continue proposal.quantity |> ok)
  | _ -> Alcotest.fail "buy-aggressor trade did not fill passive sell"

let quote_trade_limits_fok_and_continuations () =
  let marketable = T.Order.Limit (price "102") in
  (match
     quote_trade_step ~kind:marketable [ quote_event ~ask_quantity:"3" () ]
   with
  | T.Execution.Proposed (proposal, continue) ->
      Alcotest.check quantity_testable "marketable limit uses displayed size"
        (quantity "3") proposal.quantity;
      Alcotest.(check bool)
        "over-consumption rejected" true
        (Result.is_error (continue (quantity "4")));
      Alcotest.(check bool)
        "negative application rejected" true
        (Result.is_error (continue (quantity "-1")))
  | _ -> Alcotest.fail "marketable limit did not execute");
  let oms, _ =
    oms_with_order
      (request_v8 ~kind:T.Order.Market ~time_in_force:T.Order.Fok ())
  in
  let cursor =
    T.Execution.start_slice_quote_trade (quote_trade_execution ())
      ~instruments:[ instrument () ]
      ~oms
      (quote_trade_slice [ quote_event ~ask_quantity:"3" () ])
    |> ok
  in
  match T.Execution.next cursor ~oms |> ok with
  | T.Execution.Finished _ -> ()
  | _ -> Alcotest.fail "FOK order filled partial displayed liquidity"

let quote_trade_stop_and_event_boundaries () =
  let oms, order =
    oms_with_order
      (request_v8
         ~kind:(T.Order.Stop (price "100"))
         ~time_in_force:T.Order.Gtc ())
  in
  let cursor =
    T.Execution.start_slice_quote_trade (quote_trade_execution ())
      ~instruments:[ instrument () ]
      ~oms
      (quote_trade_slice [ quote_event ~ask:"101" () ])
    |> ok
  in
  (match T.Execution.next cursor ~oms |> ok with
  | T.Execution.Triggered (order_id, triggered_at, 2L, _) ->
      Alcotest.(check string)
        "triggered order"
        (T.Id.Order.to_string order.id)
        (T.Id.Order.to_string order_id);
      Alcotest.(check string)
        "quote trigger uses event time" "2026-01-03T14:30:01.000000Z"
        (T.Codec.ptime_to_string triggered_at)
  | _ -> Alcotest.fail "stop was not triggered by observable quote");
  let other_event =
    let event_at = market_event_time 1 in
    T.Market_event.quote ~instrument_id:(instrument_id "other") ~event_at
      ~available_at:event_at ~received_at:event_at ~ingest_sequence:1L
      ~bid_price:(price "99") ~bid_quantity:(quantity "1")
      ~ask_price:(price "101") ~ask_quantity:(quantity "1")
    |> ok
  in
  Alcotest.(check bool)
    "unknown event instrument rejected" true
    (Result.is_error
       (T.Execution.start_slice_quote_trade (quote_trade_execution ())
          ~instruments:[ instrument () ]
          ~oms
          (quote_trade_slice [ other_event ])));
  let old_at = timestamp "2026-01-02T14:30:00Z" in
  let old_event =
    T.Market_event.trade
      ~instrument_id:(instrument_id "test-equity")
      ~event_at:old_at ~available_at:old_at ~received_at:old_at
      ~ingest_sequence:1L ~price:(price "100") ~quantity:(quantity "1")
      ~aggressor_side:T.Market_event.Unknown
    |> ok
  in
  Alcotest.(check bool)
    "event outside slice rejected" true
    (Result.is_error
       (T.Execution.start_slice_quote_trade (quote_trade_execution ())
          ~instruments:[ instrument () ]
          ~oms
          (quote_trade_slice [ old_event ])))

let conservative_limit_models_diverge () =
  let engine = conservative_execution () in
  let limit = T.Order.Limit (price "100") in
  let touch =
    market_slice
      ~bars:[ bar ~open_price:"105" ~high_price:"110" ~low_price:"100" 2L ]
      2L
  in
  (match
     conservative_step T.Execution.start_slice_next_open ~kind:limit
       ~slice:touch engine
   with
  | T.Execution.Finished _ -> ()
  | _ -> Alcotest.fail "next-open model filled an intrabar touch");
  (match
     conservative_step T.Execution.start_slice_adverse_touch ~kind:limit
       ~slice:touch engine
   with
  | T.Execution.Finished _ -> ()
  | _ -> Alcotest.fail "adverse-touch model filled without trade-through");
  let traded_through =
    market_slice
      ~bars:[ bar ~open_price:"105" ~high_price:"110" ~low_price:"99.99" 2L ]
      2L
  in
  match
    conservative_step T.Execution.start_slice_adverse_touch ~kind:limit
      ~slice:traded_through engine
  with
  | T.Execution.Proposed (proposal, _) ->
      Alcotest.check price_testable "one-tick adverse reference" (price "99.99")
        proposal.price
  | _ -> Alcotest.fail "adverse trade-through did not fill"

let conservative_costs_are_tick_aligned_and_attributed () =
  let engine =
    conservative_execution ~half_spread_bps:10 ~impact_coefficient_bps:100 ()
  in
  match
    conservative_step T.Execution.start_slice_next_open
      ~slice:
        (market_slice
           ~bars:[ bar ~open_price:"100" ~volume:(Some "100") 2L ]
           2L)
      engine
  with
  | T.Execution.Proposed (proposal, _) ->
      Alcotest.check price_testable "spread and impact final price"
        (price "100.2") proposal.price;
      let attribution = Option.get proposal.price_attribution in
      Alcotest.check price_testable "reference" (price "100")
        attribution.reference_price;
      Alcotest.check money_testable "spread" (money "0.1")
        attribution.spread_adjustment;
      Alcotest.check money_testable "impact" (money "0.1")
        attribution.impact_adjustment;
      Alcotest.check price_testable "attributed final" proposal.price
        attribution.final_price
  | _ -> Alcotest.fail "expected conservative market fill"

let conservative_missing_volume_policy_is_explicit () =
  let missing = market_slice ~bars:[ bar ~volume:None 2L ] 2L in
  let rejecting = conservative_execution ~impact_coefficient_bps:100 () in
  let oms, _ = oms_with_order (request ()) in
  let cursor =
    T.Execution.start_slice_next_open rejecting
      ~instruments:[ instrument () ]
      ~oms missing
    |> ok
  in
  Alcotest.(check bool)
    "missing volume rejected" true
    (Result.is_error (T.Execution.next cursor ~oms));
  let zero =
    conservative_execution ~half_spread_bps:10 ~impact_coefficient_bps:100
      ~missing_volume_policy:T.Execution.Zero_impact ()
  in
  match
    conservative_step T.Execution.start_slice_next_open ~slice:missing zero
  with
  | T.Execution.Proposed (proposal, _) ->
      Alcotest.check price_testable "zero-impact fallback keeps spread"
        (price "100.1") proposal.price
  | _ -> Alcotest.fail "zero-impact fallback did not fill"

let conservative_configuration_is_bounded () =
  let valid = conservative_execution () in
  let schedules = T.Execution.fee_schedules valid in
  let create half_spread_bps impact_coefficient_bps =
    T.Execution.create_conservative ~participation_bps:10_000
      ~fee_schedules:schedules ~half_spread_bps ~impact_coefficient_bps
      ~missing_volume_policy:T.Execution.Reject_missing_volume
  in
  List.iter
    (fun (spread, impact) ->
      Alcotest.(check bool)
        "out-of-range cost rejected" true
        (Result.is_error (create spread impact)))
    [ (-1, 0); (10_001, 0); (0, -1); (0, 10_001) ];
  Alcotest.(check bool)
    "v2 participation bound enforced" true
    (Result.is_error
       (T.Execution.create_v2 ~participation_bps:(-1) ~fee_schedules:schedules));
  let schedule = List.hd schedules in
  Alcotest.(check bool)
    "duplicate fee schedules rejected" true
    (Result.is_error
       (T.Execution.create_v2 ~participation_bps:10_000
          ~fee_schedules:[ schedule; schedule ]));
  Alcotest.(check bool)
    "missing instrument fee schedule rejected" true
    (Result.is_error
       (T.Execution.calculate_fee valid
          ~instrument:(instrument ~id:"other-equity" ~symbol:"OTHER" ())
          ~notional:(money "100") ~quantity:(quantity "1")
          ~liquidity:T.Fee_schedule.Taker
          ~fx_rates:[ ("USD", price "1") ]))

let conservative_sell_costs_and_limit_protection () =
  let engine =
    conservative_execution ~half_spread_bps:10 ~impact_coefficient_bps:100 ()
  in
  (match
     conservative_step T.Execution.start_slice_next_open ~side:T.Order.Sell
       ~slice:
         (market_slice
            ~bars:[ bar ~open_price:"100" ~volume:(Some "100") 2L ]
            2L)
       engine
   with
  | T.Execution.Proposed (proposal, _) ->
      Alcotest.check price_testable "sell costs reduce execution price"
        (price "99.8") proposal.price;
      let attribution = Option.get proposal.price_attribution in
      Alcotest.check money_testable "sell spread attribution" (money "0.1")
        attribution.spread_adjustment;
      Alcotest.check money_testable "sell impact attribution" (money "0.1")
        attribution.impact_adjustment
  | _ -> Alcotest.fail "expected conservative sell fill");
  let buy_limit = T.Order.Limit (price "100") in
  match
    conservative_step T.Execution.start_slice_next_open ~kind:buy_limit
      ~slice:
        (market_slice
           ~bars:[ bar ~open_price:"100" ~volume:(Some "100") 2L ]
           2L)
      engine
  with
  | T.Execution.Finished _ -> (
      let buy_with_room = T.Order.Limit (price "101") in
      (match
         conservative_step T.Execution.start_slice_next_open ~kind:buy_with_room
           ~slice:
             (market_slice
                ~bars:[ bar ~open_price:"100" ~volume:(Some "100") 2L ]
                2L)
           engine
       with
      | T.Execution.Proposed (proposal, _) ->
          Alcotest.check price_testable "cost-adjusted buy respects limit"
            (price "100.2") proposal.price
      | _ -> Alcotest.fail "buy with limit room did not fill");
      let sell_limit = T.Order.Limit (price "100") in
      (match
         conservative_step T.Execution.start_slice_next_open ~kind:sell_limit
           ~side:T.Order.Sell
           ~slice:
             (market_slice
                ~bars:[ bar ~open_price:"100" ~volume:(Some "100") 2L ]
                2L)
           engine
       with
      | T.Execution.Finished _ -> ()
      | _ -> Alcotest.fail "cost-adjusted fill violated sell limit");
      let sell_with_room = T.Order.Limit (price "99") in
      match
        conservative_step T.Execution.start_slice_next_open ~kind:sell_with_room
          ~side:T.Order.Sell
          ~slice:
            (market_slice
               ~bars:[ bar ~open_price:"100" ~volume:(Some "100") 2L ]
               2L)
          engine
      with
      | T.Execution.Proposed (proposal, _) ->
          Alcotest.check price_testable "cost-adjusted sell respects limit"
            (price "99.8") proposal.price
      | _ -> Alcotest.fail "sell with limit room did not fill")
  | _ -> Alcotest.fail "cost-adjusted fill violated buy limit"

let conservative_adverse_sell_requires_trade_through () =
  let engine = conservative_execution () in
  let limit = T.Order.Limit (price "100") in
  let touch =
    market_slice
      ~bars:
        [
          bar ~open_price:"95" ~high_price:"100" ~low_price:"90"
            ~close_price:"95" 2L;
        ]
      2L
  in
  (match
     conservative_step T.Execution.start_slice_adverse_touch ~kind:limit
       ~side:T.Order.Sell ~slice:touch engine
   with
  | T.Execution.Finished _ -> ()
  | _ -> Alcotest.fail "sell filled without one-tick trade-through");
  let traded_through =
    market_slice
      ~bars:
        [
          bar ~open_price:"95" ~high_price:"100.01" ~low_price:"90"
            ~close_price:"95" 2L;
        ]
      2L
  in
  match
    conservative_step T.Execution.start_slice_adverse_touch ~kind:limit
      ~side:T.Order.Sell ~slice:traded_through engine
  with
  | T.Execution.Proposed (proposal, _) ->
      Alcotest.check price_testable "sell adverse reference" (price "100.01")
        proposal.price
  | _ -> Alcotest.fail "sell trade-through did not fill"

let single_order_match ?(side = T.Order.Buy) ?(kind = T.Order.Market)
    ?(quantity_value = "10") ?(slice = market_slice 2L) () =
  let request = request ~side ~kind ~quantity_value () in
  let oms, _ = oms_with_order request in
  match_orders ~oms slice

let order_waits_for_later_slice () =
  let request = request () in
  let oms, order = oms_with_order ~eligible_after_slice_sequence:1L request in
  let same = match_orders ~oms (market_slice 1L) in
  Alcotest.(check int) "no same-slice fill" 0 (List.length same.fills);
  Alcotest.(check int) "not yet IOC" 0 (List.length same.market_ioc_orders);
  let later =
    match_orders ~oms (market_slice ~bars:[ bar ~open_price:"101" 2L ] 2L)
  in
  Alcotest.(check int) "next slice fills" 1 (List.length later.fills);
  Alcotest.check price_testable "next open" (price "101") (proposal_price later);
  Alcotest.check order_id_testable "market expires after eligible slice"
    order.id
    (List.hd later.market_ioc_orders)

let order_waits_for_a_slice_that_starts_after_creation () =
  let created_at = timestamp "2026-01-02T09:35:02Z" in
  let oms, _ = oms_with_order ~created_at (request ()) in
  let overlapping =
    market_slice
      ~start_at:(timestamp "2026-01-02T09:35:00Z")
      ~end_at:(timestamp "2026-01-02T09:40:00Z")
      ~available_at:(timestamp "2026-01-02T09:40:01Z")
      ~received_at:(timestamp "2026-01-02T09:40:02Z")
      2L
  in
  let skipped = match_orders ~oms overlapping in
  Alcotest.(check int)
    "overlapping slice does not fill" 0
    (List.length skipped.fills);
  Alcotest.(check int)
    "order remains live" 0
    (List.length skipped.market_ioc_orders);
  let causal =
    market_slice
      ~start_at:(timestamp "2026-01-02T09:40:00Z")
      ~end_at:(timestamp "2026-01-02T09:45:00Z")
      ~available_at:(timestamp "2026-01-02T09:45:01Z")
      ~received_at:(timestamp "2026-01-02T09:45:02Z")
      3L
  in
  let matched = match_orders ~oms causal in
  match matched.fills with
  | [ fill ] ->
      Alcotest.(check string)
        "fill uses causal slice open" "2026-01-02T09:40:00.000000Z"
        (T.Codec.ptime_to_string fill.executed_at)
  | _ -> Alcotest.fail "expected one causally eligible fill"

let buy_limit_gap_and_touch () =
  let limit = T.Order.Limit (price "100") in
  let gap =
    single_order_match ~kind:limit
      ~slice:
        (market_slice
           ~bars:[ bar ~open_price:"95" ~high_price:"105" ~low_price:"90" 2L ]
           2L)
      ()
  in
  Alcotest.check price_testable "gap gets better open" (price "95")
    (proposal_price gap);
  let touch =
    single_order_match ~kind:limit
      ~slice:
        (market_slice
           ~bars:[ bar ~open_price:"105" ~high_price:"110" ~low_price:"99" 2L ]
           2L)
      ()
  in
  Alcotest.check price_testable "intrabar touch gets limit" (price "100")
    (proposal_price touch);
  let missed =
    single_order_match ~kind:limit
      ~slice:
        (market_slice
           ~bars:[ bar ~open_price:"105" ~high_price:"110" ~low_price:"101" 2L ]
           2L)
      ()
  in
  Alcotest.(check int) "no touch" 0 (List.length missed.fills)

let sell_limit_gap_and_touch () =
  let limit = T.Order.Limit (price "100") in
  let gap =
    single_order_match ~side:T.Order.Sell ~kind:limit
      ~slice:
        (market_slice
           ~bars:[ bar ~open_price:"105" ~high_price:"110" ~low_price:"95" 2L ]
           2L)
      ()
  in
  Alcotest.check price_testable "sell gap gets better open" (price "105")
    (proposal_price gap);
  let touch =
    single_order_match ~side:T.Order.Sell ~kind:limit
      ~slice:
        (market_slice
           ~bars:
             [
               bar ~open_price:"95" ~high_price:"101" ~low_price:"90"
                 ~close_price:"98" 2L;
             ]
           2L)
      ()
  in
  Alcotest.check price_testable "sell touch gets limit" (price "100")
    (proposal_price touch);
  let missed =
    single_order_match ~side:T.Order.Sell ~kind:limit
      ~slice:
        (market_slice
           ~bars:
             [
               bar ~open_price:"95" ~high_price:"99" ~low_price:"90"
                 ~close_price:"98" 2L;
             ]
           2L)
      ()
  in
  Alcotest.(check int) "sell no touch" 0 (List.length missed.fills)

let volume_is_allocated_fifo () =
  let first_request = request ~quantity_value:"5" () in
  let oms, first = oms_with_order ~id:"order-first" first_request in
  let second_request = request ~quantity_value:"5" () in
  let oms, second =
    T.Oms.accept oms ~id:(order_id "order-second") ~accepted_sequence:2L
      ~created_event_id:(event_id "order-second-event")
      ~created_at:(timestamp "2026-01-02T21:00:02Z")
      ~eligible_after_slice_sequence:1L second_request
    |> ok
  in
  let matched =
    match_orders ~oms (market_slice ~bars:[ bar ~volume:(Some "6") 2L ] 2L)
  in
  match matched.fills with
  | [ first_fill; second_fill ] ->
      Alcotest.check order_id_testable "first order first" first.id
        first_fill.order_id;
      Alcotest.check quantity_testable "first takes five" (quantity "5")
        first_fill.quantity;
      Alcotest.check order_id_testable "second order second" second.id
        second_fill.order_id;
      Alcotest.check quantity_testable "second takes remainder" (quantity "1")
        second_fill.quantity
  | _ -> Alcotest.fail "expected two FIFO proposals"

let sells_have_capacity_priority () =
  let buy_request = request ~quantity_value:"5" () in
  let oms, buy = oms_with_order ~id:"order-buy" buy_request in
  let sell_request = request ~side:T.Order.Sell ~quantity_value:"5" () in
  let oms, sell =
    T.Oms.accept oms ~id:(order_id "order-sell") ~accepted_sequence:2L
      ~created_event_id:(event_id "order-sell-event")
      ~created_at:(timestamp "2026-01-02T21:00:02Z")
      ~eligible_after_slice_sequence:1L sell_request
    |> ok
  in
  let matched =
    match_orders ~oms (market_slice ~bars:[ bar ~volume:(Some "6") 2L ] 2L)
  in
  match matched.fills with
  | [ sell_fill; buy_fill ] ->
      Alcotest.check order_id_testable "sell matched first" sell.id
        sell_fill.order_id;
      Alcotest.check quantity_testable "sell takes five" (quantity "5")
        sell_fill.quantity;
      Alcotest.check order_id_testable "buy matched second" buy.id
        buy_fill.order_id;
      Alcotest.check quantity_testable "buy gets remainder" (quantity "1")
        buy_fill.quantity
  | _ -> Alcotest.fail "expected sell-first capacity proposals"

let liquidations_have_capacity_priority () =
  let sell_request = request ~side:T.Order.Sell ~quantity_value:"5" () in
  let oms, sell = oms_with_order ~id:"order-sell" sell_request in
  let liquidation_request =
    request ~quantity_value:"5" ~origin:T.Order.Margin_liquidation ()
  in
  let oms, liquidation =
    T.Oms.accept oms
      ~id:(order_id "order-liquidation")
      ~accepted_sequence:2L
      ~created_event_id:(event_id "order-liquidation-event")
      ~created_at:(timestamp "2026-01-02T21:00:02Z")
      ~eligible_after_slice_sequence:1L liquidation_request
    |> ok
  in
  let matched =
    match_orders ~oms (market_slice ~bars:[ bar ~volume:(Some "6") 2L ] 2L)
  in
  match matched.fills with
  | [ liquidation_fill; sell_fill ] ->
      Alcotest.check order_id_testable "liquidation matched first"
        liquidation.id liquidation_fill.order_id;
      Alcotest.check quantity_testable "liquidation takes five" (quantity "5")
        liquidation_fill.quantity;
      Alcotest.check order_id_testable "ordinary sell matched second" sell.id
        sell_fill.order_id;
      Alcotest.check quantity_testable "ordinary sell gets remainder"
        (quantity "1") sell_fill.quantity
  | _ -> Alcotest.fail "expected liquidation-first capacity proposals"

let participation_cap_is_shared () =
  let first_request =
    request ~quantity_value:"10" ~kind:(T.Order.Limit (price "110")) ()
  in
  let oms, _ = oms_with_order ~id:"order-a" first_request in
  let second_request =
    request ~quantity_value:"10" ~kind:(T.Order.Limit (price "110")) ()
  in
  let oms, _ =
    T.Oms.accept oms ~id:(order_id "order-b") ~accepted_sequence:2L
      ~created_event_id:(event_id "order-b-event")
      ~created_at:(timestamp "2026-01-02T21:00:02Z")
      ~eligible_after_slice_sequence:1L second_request
    |> ok
  in
  let matched =
    match_orders
      ~engine:(execution ~participation_bps:2500 ())
      ~oms
      (market_slice ~bars:[ bar ~volume:(Some "20") 2L ] 2L)
  in
  let total =
    List.fold_left
      (fun total fill ->
        T.Scalar.Quantity.add total fill.T.Execution.quantity |> ok)
      T.Scalar.Quantity.zero matched.fills
  in
  Alcotest.check quantity_testable "25 percent of twenty" (quantity "5") total

let applied_quantity_controls_shared_capacity () =
  let first_request = request ~quantity_value:"5" () in
  let oms, first = oms_with_order ~id:"order-first" first_request in
  let second_request = request ~quantity_value:"5" () in
  let oms, second =
    T.Oms.accept oms ~id:(order_id "order-second") ~accepted_sequence:2L
      ~created_event_id:(event_id "order-second-event")
      ~created_at:(timestamp "2026-01-02T21:00:02Z")
      ~eligible_after_slice_sequence:1L second_request
    |> ok
  in
  let apply proposals proposed =
    let applied =
      if proposals = [] then quantity "1" else proposed.T.Execution.quantity
    in
    Ok ((proposed.order_id, proposed.quantity) :: proposals, applied)
  in
  let proposals, _ =
    T.Execution.fold_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms
      (market_slice ~bars:[ bar ~volume:(Some "5") 2L ] 2L)
      ~init:[] ~apply
    |> ok
  in
  match List.rev proposals with
  | [ (first_id, first_quantity); (second_id, second_quantity) ] ->
      Alcotest.check order_id_testable "first order remains FIFO" first.id
        first_id;
      Alcotest.check quantity_testable "first sees full capacity" (quantity "5")
        first_quantity;
      Alcotest.check order_id_testable "second order follows" second.id
        second_id;
      Alcotest.check quantity_testable "second sees unused capacity"
        (quantity "4") second_quantity
  | _ -> Alcotest.fail "expected two capacity-aware proposals"

let applied_quantity_is_validated () =
  let oms, _ = oms_with_order (request ~quantity_value:"2" ()) in
  let slice = market_slice ~bars:[ bar ~volume:(Some "2") 2L ] 2L in
  let negative =
    T.Execution.fold_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms slice ~init:()
      ~apply:(fun () _ -> Ok ((), quantity "-1"))
  in
  Alcotest.(check bool)
    "quantity cannot be negative" true (Result.is_error negative);
  let excessive =
    T.Execution.fold_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms slice ~init:()
      ~apply:(fun () _ -> Ok ((), quantity "3"))
  in
  Alcotest.(check bool)
    "quantity cannot exceed proposal" true
    (Result.is_error excessive);
  let configured = instrument ~lot_size:"2" () in
  let misaligned =
    T.Execution.fold_slice (execution ()) ~instruments:[ configured ] ~oms slice
      ~init:() ~apply:(fun () _ -> Ok ((), quantity "1"))
  in
  Alcotest.(check bool)
    "quantity must remain lot aligned" true
    (Result.is_error misaligned)

let cursor_reads_current_oms_and_preserves_capacity () =
  let oms, first =
    oms_with_order ~id:"order-first" (request ~quantity_value:"1" ())
  in
  let oms, second =
    T.Oms.accept oms ~id:(order_id "order-second") ~accepted_sequence:2L
      ~created_event_id:(event_id "order-second-event")
      ~created_at:(timestamp "2026-01-02T21:00:02Z")
      ~eligible_after_slice_sequence:1L
      (request ~quantity_value:"1" ())
    |> ok
  in
  let oms, third =
    T.Oms.accept oms ~id:(order_id "order-third") ~accepted_sequence:3L
      ~created_event_id:(event_id "order-third-event")
      ~created_at:(timestamp "2026-01-02T21:00:02Z")
      ~eligible_after_slice_sequence:1L
      (request ~quantity_value:"1" ())
    |> ok
  in
  let cursor =
    T.Execution.start_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms
      (market_slice ~bars:[ bar ~volume:(Some "2") 2L ] 2L)
    |> ok
  in
  let cursor =
    match T.Execution.next cursor ~oms |> ok with
    | T.Execution.Proposed (proposed, advance) ->
        Alcotest.check order_id_testable "first proposal" first.id
          proposed.order_id;
        advance proposed.quantity |> ok
    | T.Execution.Finished _ | T.Execution.Triggered _ ->
        Alcotest.fail "expected first proposal"
  in
  let oms, _ = T.Oms.cancel oms second.id |> ok in
  let cursor =
    match T.Execution.next cursor ~oms |> ok with
    | T.Execution.Proposed (proposed, advance) ->
        Alcotest.check order_id_testable "cancelled order is skipped" third.id
          proposed.order_id;
        Alcotest.check quantity_testable "unused capacity reaches third order"
          (quantity "1") proposed.quantity;
        advance proposed.quantity |> ok
    | T.Execution.Finished _ | T.Execution.Triggered _ ->
        Alcotest.fail "expected third-order proposal"
  in
  match T.Execution.next cursor ~oms |> ok with
  | T.Execution.Finished _ -> ()
  | T.Execution.Proposed _ | T.Execution.Triggered _ ->
      Alcotest.fail "expected completed cursor"

let fills_respect_lot_size () =
  let configured = instrument ~lot_size:"10" () in
  let oms, _ = oms_with_order (request ~quantity_value:"20" ()) in
  let matched =
    match_orders ~configured
      ~engine:(execution ~participation_bps:5000 ())
      ~oms
      (market_slice ~bars:[ bar ~volume:(Some "25") 2L ] 2L)
  in
  match matched.fills with
  | [ fill ] ->
      Alcotest.check quantity_testable "capacity rounded to one lot"
        (quantity "10") fill.quantity
  | _ -> Alcotest.fail "expected one lot-aligned fill"

let off_tick_market_slice_is_rejected () =
  let configured = instrument ~tick_size:"0.05" () in
  let oms, _ = oms_with_order (request ()) in
  let result =
    T.Execution.match_slice (execution ()) ~instruments:[ configured ] ~oms
      (market_slice ~bars:[ bar ~open_price:"100.03" 2L ] 2L)
  in
  Alcotest.(check bool)
    "off-tick executable slice rejected" true (Result.is_error result)

let incomplete_market_slice_returns_error () =
  let primary = instrument () in
  let other = instrument ~id:"other-equity" ~symbol:"OTHER" () in
  let oms, _ = oms_with_order (request ()) in
  let incomplete = market_slice ~bars:[ bar ~instrument:other.id 2L ] 2L in
  Alcotest.(check bool)
    "missing eligible instrument is an error" true
    (Result.is_error
       (T.Execution.match_slice (execution ()) ~instruments:[ primary; other ]
          ~oms incomplete))

let tests =
  [
    Alcotest.test_case "quote replay consumes displayed liquidity" `Quick
      quote_trade_consumes_displayed_liquidity;
    Alcotest.test_case "passive trade requires aggressor evidence" `Quick
      quote_trade_passive_fills_require_aggressor_evidence;
    Alcotest.test_case "quote replay sell paths" `Quick
      quote_trade_sell_paths_use_bid_and_buy_aggressors;
    Alcotest.test_case "quote replay limits, FOK, and continuations" `Quick
      quote_trade_limits_fok_and_continuations;
    Alcotest.test_case "quote replay stops and boundaries" `Quick
      quote_trade_stop_and_event_boundaries;
    Alcotest.test_case "conservative limit models diverge" `Quick
      conservative_limit_models_diverge;
    Alcotest.test_case "conservative costs are attributed" `Quick
      conservative_costs_are_tick_aligned_and_attributed;
    Alcotest.test_case "conservative missing-volume policy" `Quick
      conservative_missing_volume_policy_is_explicit;
    Alcotest.test_case "conservative configuration bounds" `Quick
      conservative_configuration_is_bounded;
    Alcotest.test_case "conservative sell costs and limits" `Quick
      conservative_sell_costs_and_limit_protection;
    Alcotest.test_case "conservative adverse sell" `Quick
      conservative_adverse_sell_requires_trade_through;
    Alcotest.test_case "order waits for later slice" `Quick
      order_waits_for_later_slice;
    Alcotest.test_case "order waits for causal slice time" `Quick
      order_waits_for_a_slice_that_starts_after_creation;
    Alcotest.test_case "buy limit gap and touch" `Quick buy_limit_gap_and_touch;
    Alcotest.test_case "sell limit gap and touch" `Quick
      sell_limit_gap_and_touch;
    Alcotest.test_case "volume FIFO" `Quick volume_is_allocated_fifo;
    Alcotest.test_case "sells have capacity priority" `Quick
      sells_have_capacity_priority;
    Alcotest.test_case "liquidations have capacity priority" `Quick
      liquidations_have_capacity_priority;
    Alcotest.test_case "participation cap shared" `Quick
      participation_cap_is_shared;
    Alcotest.test_case "applied quantity controls shared capacity" `Quick
      applied_quantity_controls_shared_capacity;
    Alcotest.test_case "applied quantity validation" `Quick
      applied_quantity_is_validated;
    Alcotest.test_case "cursor reads current OMS" `Quick
      cursor_reads_current_oms_and_preserves_capacity;
    Alcotest.test_case "fills respect lot size" `Quick fills_respect_lot_size;
    Alcotest.test_case "off-tick market slice rejected" `Quick
      off_tick_market_slice_is_rejected;
    Alcotest.test_case "incomplete market slice returns error" `Quick
      incomplete_market_slice_returns_error;
  ]
