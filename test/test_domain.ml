open Test_support
module T = Trading_engine

let identifier_validation () =
  Alcotest.(check bool)
    "empty rejected" true
    (Result.is_error (T.Id.Order.of_string ""));
  Alcotest.(check bool)
    "whitespace rejected" true
    (Result.is_error (T.Id.Order.of_string " order"));
  Alcotest.(check string)
    "round trip" "order-1"
    (T.Id.Order.of_string_exn "order-1" |> T.Id.Order.to_string)

let scalar_decimal_round_trip () =
  let values = [ "0"; "1"; "1.25"; "-0.5"; "999999.000001" ] in
  List.iter
    (fun value ->
      let parsed = money value in
      Alcotest.(check string)
        value value
        (T.Scalar.Money.to_decimal_string parsed))
    values;
  Alcotest.(check bool)
    "price zero rejected" true
    (Result.is_error (T.Scalar.Price.of_decimal_string "0"));
  Alcotest.(check bool)
    "excess precision rejected" true
    (Result.is_error (T.Scalar.Money.of_decimal_string "1.0000001"));
  List.iter
    (fun noncanonical ->
      Alcotest.(check bool)
        (noncanonical ^ " rejected")
        true
        (Result.is_error (T.Scalar.Money.of_decimal_string noncanonical)))
    [ "01"; "1.0"; "-0" ];
  Alcotest.(check bool)
    "leading-zero quantity rejected" true
    (Result.is_error (T.Scalar.Quantity.of_string "01"))

let scalar_overflow_is_rejected () =
  let maximum = T.Scalar.Money.of_micros Int64.max_int in
  Alcotest.(check bool)
    "add overflow" true
    (Result.is_error (T.Scalar.Money.add maximum (money "0.000001")));
  let huge_price = T.Scalar.Price.of_micros Int64.max_int |> ok in
  Alcotest.(check bool)
    "notional overflow" true
    (Result.is_error (T.Scalar.Money.notional huge_price (quantity "2")));
  let near_maximum = T.Scalar.Money.of_micros (Int64.sub Int64.max_int 1L) in
  let ratio =
    T.Scalar.Money.proportion_floor near_maximum
      ~numerator:(T.Scalar.Quantity.of_int64 (Int64.sub Int64.max_int 1L) |> ok)
      ~denominator:(T.Scalar.Quantity.of_int64 Int64.max_int |> ok)
    |> ok
  in
  Alcotest.(check int64)
    "extreme proportion uses wide intermediate"
    (Int64.sub Int64.max_int 2L)
    (T.Scalar.Money.to_micros ratio)

let fee_rounds_up_to_one_micro () =
  let fee =
    T.Scalar.Money.fee ~fixed:(money "0.25") ~bps:1 ~notional:(money "0.000001")
    |> ok
  in
  Alcotest.check money_testable "fixed plus rounded variable fee"
    (money "0.250001") fee

let market_slice_validation () =
  let start_at = timestamp "2026-01-02T14:30:00Z" in
  let end_at = timestamp "2026-01-02T21:00:00Z" in
  let available_at = timestamp "2026-01-02T20:59:59Z" in
  let received_at = timestamp "2026-01-02T21:00:01Z" in
  let result =
    T.Market_slice.create ~slice_sequence:1L ~start_at ~end_at ~available_at
      ~received_at
      ~bars:[ bar 1L ]
  in
  Alcotest.(check bool)
    "premature availability rejected" true (Result.is_error result)

let sha256_vectors () =
  Alcotest.(check string)
    "empty" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (T.Sha256.digest_string "");
  Alcotest.(check string)
    "abc" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (T.Sha256.digest_string "abc")

let oms_partial_fill_and_duplicate () =
  let request = request ~quantity_value:"10" () in
  let oms, order = oms_with_order request in
  let first = fill ~quantity_value:"4" order in
  let oms, outcome = T.Oms.apply_fill oms first |> ok in
  let updated =
    match outcome with
    | T.Oms.Applied order -> order
    | T.Oms.Duplicate -> Alcotest.fail "fill lost"
  in
  Alcotest.(check string)
    "partial status" "partially_filled"
    (T.Order.status_to_string updated.status);
  Alcotest.check quantity_testable "partial remainder" (quantity "6")
    (T.Order.remaining_quantity updated);
  let duplicate_oms, duplicate = T.Oms.apply_fill oms first |> ok in
  (match duplicate with
  | T.Oms.Duplicate -> ()
  | T.Oms.Applied _ -> Alcotest.fail "duplicate applied");
  Alcotest.check quantity_testable "duplicate did not change fill"
    updated.filled_quantity
    (T.Oms.find duplicate_oms order.id |> Option.get).filled_quantity;
  let conflicting = fill ~id:"fill-1" ~quantity_value:"1" updated in
  Alcotest.(check bool)
    "conflicting duplicate rejected" true
    (Result.is_error (T.Oms.apply_fill duplicate_oms conflicting));
  let second = fill ~id:"fill-2" ~quantity_value:"6" updated in
  let oms, outcome = T.Oms.apply_fill duplicate_oms second |> ok in
  let filled =
    match outcome with
    | T.Oms.Applied order -> order
    | T.Oms.Duplicate -> Alcotest.fail "fill lost"
  in
  Alcotest.(check string)
    "filled status" "filled"
    (T.Order.status_to_string filled.status);
  Alcotest.(check bool)
    "terminal cancel rejected" true
    (Result.is_error (T.Oms.cancel oms filled.id))

let oms_rejects_overfill () =
  let oms, order = oms_with_order (request ~quantity_value:"2" ()) in
  let excessive = fill ~quantity_value:"3" order in
  Alcotest.(check bool)
    "overfill rejected" true
    (Result.is_error (T.Oms.apply_fill oms excessive))

let oms_rejects_fill_before_order () =
  let created_at = timestamp "2026-01-03T14:30:00Z" in
  let oms, order = oms_with_order ~created_at (request ()) in
  let early = fill ~executed_at:(timestamp "2026-01-03T14:29:59Z") order in
  Alcotest.(check bool)
    "time-travel fill rejected" true
    (Result.is_error (T.Oms.apply_fill oms early))

let risk_checks_lot_and_tick_alignment () =
  let configured = instrument ~tick_size:"0.05" ~lot_size:"10" () in
  let risk = risk ~instruments:[ configured ] () in
  let account = T.Account.create ~initial_cash:(money "10000") in
  let odd_lot = request ~quantity_value:"5" () in
  Alcotest.(check bool)
    "odd lot rejected" true
    (Result.is_error (T.Risk.check risk ~account ~oms:T.Oms.empty odd_lot));
  let off_tick =
    request ~quantity_value:"10" ~kind:(T.Order.Limit (price "100.03")) ()
  in
  Alcotest.(check bool)
    "off tick rejected" true
    (Result.is_error (T.Risk.check risk ~account ~oms:T.Oms.empty off_tick));
  let aligned =
    request ~quantity_value:"10" ~kind:(T.Order.Limit (price "100.05")) ()
  in
  Alcotest.(check (result unit string))
    "aligned accepted" (Ok ())
    (T.Risk.check risk ~account ~oms:T.Oms.empty aligned)

let risk_enforces_one_currency () =
  let instruments =
    [ instrument ~id:"usd" (); instrument ~id:"eur" ~currency:"EUR" () ]
  in
  Alcotest.(check bool)
    "mixed currencies rejected" true
    (Result.is_error
       (T.Risk.create ~base_currency:"USD" ~instruments
          ~max_order_quantity:(quantity "1000") ~max_position:(quantity "1000")))

let risk_limits_cover_lots () =
  let configured = instrument ~lot_size:"10" () in
  Alcotest.(check bool)
    "order limit smaller than lot rejected" true
    (Result.is_error
       (T.Risk.create ~base_currency:"USD" ~instruments:[ configured ]
          ~max_order_quantity:(quantity "5") ~max_position:(quantity "100")));
  Alcotest.(check bool)
    "position limit smaller than lot rejected" true
    (Result.is_error
       (T.Risk.create ~base_currency:"USD" ~instruments:[ configured ]
          ~max_order_quantity:(quantity "100") ~max_position:(quantity "5")))

let tests =
  [
    Alcotest.test_case "identifier validation" `Quick identifier_validation;
    Alcotest.test_case "fixed-point decimal round trip" `Quick
      scalar_decimal_round_trip;
    Alcotest.test_case "checked overflow" `Quick scalar_overflow_is_rejected;
    Alcotest.test_case "fee rounds up" `Quick fee_rounds_up_to_one_micro;
    Alcotest.test_case "market slice validation" `Quick market_slice_validation;
    Alcotest.test_case "SHA-256 vectors" `Quick sha256_vectors;
    Alcotest.test_case "OMS partial and duplicate fills" `Quick
      oms_partial_fill_and_duplicate;
    Alcotest.test_case "OMS rejects overfill" `Quick oms_rejects_overfill;
    Alcotest.test_case "OMS rejects time-travel fill" `Quick
      oms_rejects_fill_before_order;
    Alcotest.test_case "risk lot and tick alignment" `Quick
      risk_checks_lot_and_tick_alignment;
    Alcotest.test_case "risk enforces one currency" `Quick
      risk_enforces_one_currency;
    Alcotest.test_case "risk limits cover lots" `Quick risk_limits_cover_lots;
  ]
