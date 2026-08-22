open Test_support
module T = Trading_engine

let component ?(currency = "USD") ?(rounding = T.Fee_schedule.Up)
    ?(applies_to = T.Fee_schedule.Any) name basis =
  T.Fee_schedule.create_component ~name ~currency ~basis ~rounding
    ~applicability:applies_to
  |> ok

let schedule ?(minimum = None) ?(maximum = None) components =
  T.Fee_schedule.create ~schedule_id:"test-fees-v1"
    ~instrument_id:(instrument_id "test-equity")
    ~settlement_currency:"USD" ~minimum ~maximum ~components
  |> ok

let calculate schedule ~liquidity ~notional_value ~quantity_value =
  T.Fee_schedule.calculate schedule ~quote_currency:"USD"
    ~notional:(money notional_value) ~quantity:(quantity quantity_value)
    ~liquidity
    ~fx_rates:[ ("USD", price "1"); ("EUR", price "1.2") ]
  |> ok

let components_minimums_caps_and_fx () =
  let fees =
    schedule
      ~minimum:(Some (money "0.5"))
      ~maximum:(Some (money "2"))
      [
        component "broker" (T.Fee_schedule.Fixed (money "0.1"));
        component ~currency:"EUR" "exchange" (T.Fee_schedule.Notional_bps 10);
        component ~rounding:T.Fee_schedule.Nearest
          ~applies_to:T.Fee_schedule.Maker_only "maker_rebate"
          (T.Fee_schedule.Notional_bps (-5));
        component "regulatory" (T.Fee_schedule.Per_unit (money "0.01"));
      ]
  in
  let taker_components, taker =
    calculate fees ~liquidity:T.Fee_schedule.Taker ~notional_value:"100"
      ~quantity_value:"10"
  in
  Alcotest.check money_testable "minimum applied after FX" (money "0.5") taker;
  Alcotest.(check int)
    "three charges and minimum adjustment" 4
    (List.length taker_components);
  let maker_components, maker =
    calculate fees ~liquidity:T.Fee_schedule.Maker ~notional_value:"100"
      ~quantity_value:"10"
  in
  Alcotest.check money_testable "rebate still observes minimum" (money "0.5")
    maker;
  Alcotest.(check bool)
    "maker attribution includes rebate" true
    (List.exists
       (fun component ->
         String.equal component.T.Fee_schedule.name "maker_rebate"
         && T.Scalar.Money.compare component.quote_amount T.Scalar.Money.zero
            < 0)
       maker_components);
  let capped =
    schedule
      ~maximum:(Some (money "2"))
      [ component "broker" (T.Fee_schedule.Fixed (money "3")) ]
  in
  let capped_components, capped_total =
    calculate capped ~liquidity:T.Fee_schedule.Taker ~notional_value:"100"
      ~quantity_value:"1"
  in
  Alcotest.check money_testable "cap" (money "2") capped_total;
  Alcotest.(check bool)
    "cap is attributed" true
    (List.exists
       (fun component ->
         String.equal component.T.Fee_schedule.kind "maximum_adjustment")
       capped_components)

let fragmented_fills_pay_per_fill_minimum () =
  let fees =
    schedule
      ~minimum:(Some (money "0.5"))
      [ component "exchange" (T.Fee_schedule.Notional_bps 1) ]
  in
  let _, whole =
    calculate fees ~liquidity:T.Fee_schedule.Taker ~notional_value:"100"
      ~quantity_value:"10"
  in
  let _, fragment =
    calculate fees ~liquidity:T.Fee_schedule.Taker ~notional_value:"50"
      ~quantity_value:"5"
  in
  let fragmented = T.Scalar.Money.add fragment fragment |> ok in
  Alcotest.check money_testable "whole minimum" (money "0.5") whole;
  Alcotest.check money_testable "two fill minimums" (money "1") fragmented

let rebate_settles_and_is_attributed () =
  let fees =
    schedule
      [
        component ~rounding:T.Fee_schedule.Nearest "maker_rebate"
          (T.Fee_schedule.Notional_bps (-10));
      ]
  in
  let fee_components, fee =
    calculate fees ~liquidity:T.Fee_schedule.Maker ~notional_value:"100"
      ~quantity_value:"1"
  in
  Alcotest.check money_testable "negative rebate" (money "-0.1") fee;
  let request = request ~quantity_value:"1" () in
  let fill =
    T.Fill.create_v9 ~id:(fill_id "rebate-fill")
      ~order_id:(order_id "rebate-order") ~instrument_id:request.instrument_id
      ~quote_currency:"USD" ~side:T.Order.Buy ~quantity:(quantity "1")
      ~price:(price "100") ~fee ~fee_components
      ~executed_at:(timestamp "2026-01-03T14:30:00Z")
      ~slice_sequence:1L
    |> ok
  in
  let account = T.Account.apply_fill (test_account ()) fill |> ok in
  Alcotest.check money_testable "rebate increases cash" (money "9900.1")
    (T.Account.cash account "USD" |> Option.get);
  let position = T.Account.position account request.instrument_id in
  Alcotest.check money_testable "signed execution fee" (money "-0.1")
    position.execution_fees;
  Alcotest.(check int)
    "position component attribution" 1
    (List.length position.execution_fee_components)

let tests =
  [
    Alcotest.test_case "components, FX, minimums, and caps" `Quick
      components_minimums_caps_and_fx;
    Alcotest.test_case "fragmented minimums" `Quick
      fragmented_fills_pay_per_fill_minimum;
    Alcotest.test_case "rebate accounting attribution" `Quick
      rebate_settles_and_is_attributed;
  ]
