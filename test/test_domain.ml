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
    (Result.is_error (T.Scalar.Quantity.of_decimal_string "01"))

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
    T.Scalar.Money.proportion_toward_zero near_maximum
      ~numerator:(T.Scalar.Quantity.of_micros (Int64.sub Int64.max_int 1L))
      ~denominator:(T.Scalar.Quantity.of_micros Int64.max_int)
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

let portfolio_weight_rounds_toward_zero () =
  let positive =
    T.Scalar.Money.weight_toward_zero (money "2") ~equity:(money "3") |> ok
  in
  let negative =
    T.Scalar.Money.weight_toward_zero (money "-1") ~equity:(money "3") |> ok
  in
  Alcotest.(check string)
    "positive truncated" "0.666666"
    (T.Scalar.Weight.to_decimal_string positive);
  Alcotest.(check string)
    "negative truncated" "-0.333333"
    (T.Scalar.Weight.to_decimal_string negative);
  Alcotest.(check bool)
    "zero equity rejected" true
    (Result.is_error
       (T.Scalar.Money.weight_toward_zero (money "1") ~equity:(money "0")))

let market_slice_validation () =
  let start_at = timestamp "2026-01-02T14:30:00Z" in
  let end_at = timestamp "2026-01-02T21:00:00Z" in
  let available_at = timestamp "2026-01-02T20:59:59Z" in
  let received_at = timestamp "2026-01-02T21:00:01Z" in
  let result =
    T.Market_slice.create ~slice_sequence:1L ~start_at ~end_at ~available_at
      ~received_at
      ~bars:[ bar 1L ]
      ~fx_rates:[ fx_mark () ]
      ~corporate_actions:[]
  in
  Alcotest.(check bool)
    "premature availability rejected" true (Result.is_error result)

let bar_validation_boundaries () =
  let instrument_id = instrument_id "bar-validation" in
  let create ?(open_price = "100") ?(high_price = "110") ?(low_price = "90")
      ?(close_price = "105") ?(volume = Some "10") () =
    T.Bar.create ~instrument_id ~open_price:(price open_price)
      ~high_price:(price high_price) ~low_price:(price low_price)
      ~close_price:(price close_price)
      ~volume:(Option.map quantity volume)
  in
  let rejects label expected result =
    Alcotest.(check string) label expected (error result)
  in
  rejects "inverted range" "bar low must not exceed its high"
    (create ~high_price:"90" ~low_price:"100" ());
  rejects "open below range" "bar open must lie inside its low-high range"
    (create ~open_price:"89" ());
  rejects "open above range" "bar open must lie inside its low-high range"
    (create ~open_price:"111" ());
  rejects "close below range" "bar close must lie inside its low-high range"
    (create ~close_price:"89" ());
  rejects "close above range" "bar close must lie inside its low-high range"
    (create ~close_price:"111" ());
  rejects "negative volume" "bar volume must be nonnegative"
    (create ~volume:(Some "-1") ());
  let valid = create ~volume:None () |> ok in
  Alcotest.(check string)
    "rendered close" "bar[bar-validation] close=105"
    (Format.asprintf "%a" T.Bar.pp valid)

let corporate_action_validation_boundaries () =
  let id value = T.Id.Corporate_action.of_string_exn value in
  let instrument_id = instrument_id "action-validation" in
  let split ?(numerator = 2L) ?(denominator = 1L) action_id =
    T.Corporate_action.split ~id:(id action_id) ~instrument_id ~numerator
      ~denominator
  in
  Alcotest.(check string)
    "zero numerator" "split numerator and denominator must be positive"
    (error (split ~numerator:0L "zero-numerator"));
  Alcotest.(check string)
    "zero denominator" "split numerator and denominator must be positive"
    (error (split ~denominator:0L "zero-denominator"));
  Alcotest.(check string)
    "unchanged units" "split ratio must change the instrument units"
    (error (split ~numerator:1L "identity-split"));
  let split_action = split "split" |> ok in
  let invalid_dividend =
    T.Corporate_action.cash_dividend ~id:(id "invalid-dividend") ~instrument_id
      ~amount_per_unit:(money "0")
  in
  Alcotest.(check string)
    "zero dividend" "cash dividend amount per unit must be positive"
    (error invalid_dividend);
  let dividend =
    T.Corporate_action.cash_dividend ~id:(id "dividend") ~instrument_id
      ~amount_per_unit:(money "0.25")
    |> ok
  in
  Alcotest.(check bool)
    "actions compare by ID" true
    (T.Corporate_action.compare dividend split_action < 0);
  Alcotest.(check string)
    "split rendering" "split split 2:1 action-validation"
    (Format.asprintf "%a" T.Corporate_action.pp split_action);
  Alcotest.(check string)
    "dividend rendering" "dividend dividend 0.25 action-validation"
    (Format.asprintf "%a" T.Corporate_action.pp dividend)

let sha256_vectors () =
  Alcotest.(check string)
    "empty" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (T.Sha256.digest_string "");
  Alcotest.(check string)
    "abc" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (T.Sha256.digest_string "abc");
  Alcotest.(check string)
    "padding boundary"
    "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a"
    (T.Sha256.digest_string (String.make 56 'a'))

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
  let account = test_account () in
  let odd_lot = request ~quantity_value:"5" () in
  Alcotest.(check bool)
    "odd lot rejected" true
    (Result.is_error (risk_check risk ~account ~oms:T.Oms.empty odd_lot));
  let off_tick =
    request ~quantity_value:"10" ~kind:(T.Order.Limit (price "100.03")) ()
  in
  Alcotest.(check bool)
    "off tick rejected" true
    (Result.is_error (risk_check risk ~account ~oms:T.Oms.empty off_tick));
  let aligned =
    request ~quantity_value:"10" ~kind:(T.Order.Limit (price "100.05")) ()
  in
  Alcotest.(check (result unit string))
    "aligned accepted" (Ok ())
    (risk_check risk ~account ~oms:T.Oms.empty aligned)

let risk_accepts_multiple_currencies () =
  let instruments =
    [ instrument ~id:"usd" (); instrument ~id:"eur" ~currency:"EUR" () ]
  in
  let configured = risk ~instruments () in
  Alcotest.(check int)
    "mixed currencies accepted" 2
    (List.length (T.Risk.instruments configured))

let risk_limits_cover_lots () =
  let configured = instrument ~lot_size:"10" () in
  Alcotest.(check bool)
    "order limit smaller than lot rejected" true
    (Result.is_error
       (T.Risk.create ~base_currency:"USD" ~instruments:[ configured ]
          ~max_order_quantity:(quantity "5") ~max_long_position:(quantity "100")
          ~max_short_position:(quantity "100")
          ~max_gross_exposure:(money "1000000")
          ~max_leverage:(T.Scalar.Ratio.of_decimal_string "2" |> ok)
          ~initial_margin_bps:5000 ~maintenance_margin_bps:2500
          ~short_borrow_bps:100));
  Alcotest.(check bool)
    "position limit smaller than lot rejected" true
    (Result.is_error
       (T.Risk.create ~base_currency:"USD" ~instruments:[ configured ]
          ~max_order_quantity:(quantity "100") ~max_long_position:(quantity "5")
          ~max_short_position:(quantity "100")
          ~max_gross_exposure:(money "1000000")
          ~max_leverage:(T.Scalar.Ratio.of_decimal_string "2" |> ok)
          ~initial_margin_bps:5000 ~maintenance_margin_bps:2500
          ~short_borrow_bps:100))

let tests =
  [
    Alcotest.test_case "identifier validation" `Quick identifier_validation;
    Alcotest.test_case "fixed-point decimal round trip" `Quick
      scalar_decimal_round_trip;
    Alcotest.test_case "checked overflow" `Quick scalar_overflow_is_rejected;
    Alcotest.test_case "fee rounds up" `Quick fee_rounds_up_to_one_micro;
    Alcotest.test_case "portfolio weight rounds toward zero" `Quick
      portfolio_weight_rounds_toward_zero;
    Alcotest.test_case "market slice validation" `Quick market_slice_validation;
    Alcotest.test_case "bar validation boundaries" `Quick
      bar_validation_boundaries;
    Alcotest.test_case "corporate action validation boundaries" `Quick
      corporate_action_validation_boundaries;
    Alcotest.test_case "SHA-256 vectors" `Quick sha256_vectors;
    Alcotest.test_case "OMS partial and duplicate fills" `Quick
      oms_partial_fill_and_duplicate;
    Alcotest.test_case "OMS rejects overfill" `Quick oms_rejects_overfill;
    Alcotest.test_case "OMS rejects time-travel fill" `Quick
      oms_rejects_fill_before_order;
    Alcotest.test_case "risk lot and tick alignment" `Quick
      risk_checks_lot_and_tick_alignment;
    Alcotest.test_case "risk accepts multiple currencies" `Quick
      risk_accepts_multiple_currencies;
    Alcotest.test_case "risk limits cover lots" `Quick risk_limits_cover_lots;
  ]
