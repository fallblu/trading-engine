open Test_support
module T = Trading_engine

let proposal_price result =
  match result.T.Execution.fills with
  | [ fill ] -> fill.price
  | _ -> Alcotest.fail "expected one proposed fill"

let single_order_match ?(side = T.Order.Buy) ?(kind = T.Order.Market)
    ?(quantity_value = "10") ?(bar = bar 2L) () =
  let request = request ~side ~kind ~quantity_value () in
  let oms, _ = oms_with_order request in
  T.Execution.match_bar (execution ()) ~instrument:(instrument ()) ~oms bar
  |> ok

let order_waits_for_later_bar () =
  let request = request () in
  let oms, order = oms_with_order ~eligible_after_bar_sequence:1L request in
  let same =
    T.Execution.match_bar (execution ()) ~instrument:(instrument ()) ~oms
      (bar 1L)
    |> ok
  in
  Alcotest.(check int) "no same-bar fill" 0 (List.length same.fills);
  Alcotest.(check int) "not yet IOC" 0 (List.length same.market_ioc_orders);
  let later =
    T.Execution.match_bar (execution ()) ~instrument:(instrument ()) ~oms
      (bar ~open_price:"101" 2L)
    |> ok
  in
  Alcotest.(check int) "next bar fills" 1 (List.length later.fills);
  Alcotest.check price_testable "next open" (price "101") (proposal_price later);
  Alcotest.check order_id_testable "market expires after eligible bar" order.id
    (List.hd later.market_ioc_orders)

let buy_limit_gap_and_touch () =
  let limit = T.Order.Limit (price "100") in
  let gap =
    single_order_match ~kind:limit
      ~bar:(bar ~open_price:"95" ~high_price:"105" ~low_price:"90" 2L)
      ()
  in
  Alcotest.check price_testable "gap gets better open" (price "95")
    (proposal_price gap);
  let touch =
    single_order_match ~kind:limit
      ~bar:(bar ~open_price:"105" ~high_price:"110" ~low_price:"99" 2L)
      ()
  in
  Alcotest.check price_testable "intrabar touch gets limit" (price "100")
    (proposal_price touch);
  let missed =
    single_order_match ~kind:limit
      ~bar:(bar ~open_price:"105" ~high_price:"110" ~low_price:"101" 2L)
      ()
  in
  Alcotest.(check int) "no touch" 0 (List.length missed.fills)

let sell_limit_gap_and_touch () =
  let limit = T.Order.Limit (price "100") in
  let gap =
    single_order_match ~side:T.Order.Sell ~kind:limit
      ~bar:(bar ~open_price:"105" ~high_price:"110" ~low_price:"95" 2L)
      ()
  in
  Alcotest.check price_testable "sell gap gets better open" (price "105")
    (proposal_price gap);
  let touch =
    single_order_match ~side:T.Order.Sell ~kind:limit
      ~bar:
        (bar ~open_price:"95" ~high_price:"101" ~low_price:"90"
           ~close_price:"98" 2L)
      ()
  in
  Alcotest.check price_testable "sell touch gets limit" (price "100")
    (proposal_price touch);
  let missed =
    single_order_match ~side:T.Order.Sell ~kind:limit
      ~bar:
        (bar ~open_price:"95" ~high_price:"99" ~low_price:"90" ~close_price:"98"
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
      ~eligible_after_bar_sequence:1L second_request
    |> ok
  in
  let matched =
    T.Execution.match_bar (execution ()) ~instrument:(instrument ()) ~oms
      (bar ~volume:(Some "6") 2L)
    |> ok
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
      ~eligible_after_bar_sequence:1L second_request
    |> ok
  in
  let matched =
    T.Execution.match_bar
      (execution ~participation_bps:2500 ())
      ~instrument:(instrument ()) ~oms
      (bar ~volume:(Some "20") 2L)
    |> ok
  in
  let total =
    List.fold_left
      (fun total fill ->
        T.Scalar.Quantity.add total fill.T.Execution.quantity |> ok)
      T.Scalar.Quantity.zero matched.fills
  in
  Alcotest.check quantity_testable "25 percent of twenty" (quantity "5") total

let fills_respect_lot_size () =
  let configured = instrument ~lot_size:"10" () in
  let request = request ~quantity_value:"20" () in
  let oms, _ = oms_with_order request in
  let matched =
    T.Execution.match_bar
      (execution ~participation_bps:5000 ())
      ~instrument:configured ~oms
      (bar ~volume:(Some "25") 2L)
    |> ok
  in
  match matched.fills with
  | [ fill ] ->
      Alcotest.check quantity_testable "capacity rounded to one lot"
        (quantity "10") fill.quantity
  | _ -> Alcotest.fail "expected one lot-aligned fill"

let off_tick_market_bar_is_rejected () =
  let configured = instrument ~tick_size:"0.05" () in
  let oms, _ = oms_with_order (request ()) in
  let result =
    T.Execution.match_bar (execution ()) ~instrument:configured ~oms
      (bar ~open_price:"100.03" 2L)
  in
  Alcotest.(check bool)
    "off-tick executable bar rejected" true (Result.is_error result)

let tests =
  [
    Alcotest.test_case "order waits for later bar" `Quick
      order_waits_for_later_bar;
    Alcotest.test_case "buy limit gap and touch" `Quick buy_limit_gap_and_touch;
    Alcotest.test_case "sell limit gap and touch" `Quick
      sell_limit_gap_and_touch;
    Alcotest.test_case "volume FIFO" `Quick volume_is_allocated_fifo;
    Alcotest.test_case "participation cap shared" `Quick
      participation_cap_is_shared;
    Alcotest.test_case "fills respect lot size" `Quick fills_respect_lot_size;
    Alcotest.test_case "off-tick market bar rejected" `Quick
      off_tick_market_bar_is_rejected;
  ]
