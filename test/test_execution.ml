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
    Alcotest.test_case "participation cap shared" `Quick
      participation_cap_is_shared;
    Alcotest.test_case "applied quantity controls shared capacity" `Quick
      applied_quantity_controls_shared_capacity;
    Alcotest.test_case "applied quantity validation" `Quick
      applied_quantity_is_validated;
    Alcotest.test_case "fills respect lot size" `Quick fills_respect_lot_size;
    Alcotest.test_case "off-tick market slice rejected" `Quick
      off_tick_market_slice_is_rejected;
    Alcotest.test_case "incomplete market slice returns error" `Quick
      incomplete_market_slice_returns_error;
  ]
