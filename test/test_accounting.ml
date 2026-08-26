open Test_support
module T = Trading_engine

let apply_trade account ~id ~side ~quantity_value ~price_value ~fee_value =
  let order =
    request ~side ~quantity_value () |> accepted_order ~id:("order-" ^ id)
  in
  let fill =
    fill ~id:("fill-" ^ id) ~quantity_value ~price_value ~fee_value order
  in
  T.Account.apply_fill account fill |> ok

let exact_cost_basis_and_pnl () =
  let account = test_account () in
  let account =
    apply_trade account ~id:"buy" ~side:T.Order.Buy ~quantity_value:"10"
      ~price_value:"100" ~fee_value:"1"
  in
  Alcotest.check money_testable "cash after buy" (money "8999")
    (account_cash account);
  let position = T.Account.position account (instrument_id "test-equity") in
  Alcotest.check quantity_testable "ten shares" (quantity "10")
    position.quantity;
  Alcotest.check money_testable "fee included in basis" (money "1001")
    position.cost_basis;
  let marked =
    account_value account ~marks:[ (instrument_id "test-equity", price "110") ]
  in
  Alcotest.check money_testable "open equity" (money "10099") marked.equity;
  Alcotest.check money_testable "open unrealized" (money "99")
    marked.unrealized_pnl;
  (match marked.positions with
  | [ attribution ] ->
      Alcotest.check quantity_testable "attributed open quantity"
        (quantity "10") attribution.quantity;
      Alcotest.check money_testable "attributed open basis" (money "1001")
        attribution.cost_basis;
      Alcotest.check money_testable "attributed buy fee" (money "1")
        attribution.total_fees
  | _ -> Alcotest.fail "expected one attributed position");
  let account =
    apply_trade account ~id:"sell-one" ~side:T.Order.Sell ~quantity_value:"4"
      ~price_value:"120" ~fee_value:"0.5"
  in
  Alcotest.check money_testable "cash after partial sell" (money "9478.5")
    (account_cash account);
  let remaining = T.Account.position account (instrument_id "test-equity") in
  Alcotest.check quantity_testable "six shares remain" (quantity "6")
    remaining.quantity;
  Alcotest.check money_testable "proportional basis" (money "600.6")
    remaining.cost_basis;
  Alcotest.check money_testable "partial realized" (money "79.1")
    remaining.realized_pnl;
  let account =
    apply_trade account ~id:"sell-two" ~side:T.Order.Sell ~quantity_value:"6"
      ~price_value:"90" ~fee_value:"0.5"
  in
  Alcotest.check money_testable "final cash" (money "10018")
    (account_cash account);
  Alcotest.check quantity_testable "position closed" T.Scalar.Quantity.zero
    (T.Account.position_quantity account (instrument_id "test-equity"));
  let closed =
    account_value account ~marks:[ (instrument_id "test-equity", price "95") ]
  in
  match closed.positions with
  | [ attribution ] ->
      Alcotest.check quantity_testable "closed attributed quantity"
        T.Scalar.Quantity.zero attribution.quantity;
      Alcotest.check money_testable "closed attributed basis"
        T.Scalar.Money.zero attribution.cost_basis;
      Alcotest.check money_testable "instrument realized P&L" (money "18")
        attribution.realized_pnl;
      Alcotest.check money_testable "instrument cumulative fees" (money "2")
        attribution.total_fees;
      Alcotest.check money_testable "closed attribution reconciles realized"
        closed.realized_pnl attribution.realized_pnl;
      Alcotest.check money_testable "closed attribution reconciles fees"
        closed.total_fees attribution.total_fees
  | _ -> Alcotest.fail "expected the closed position attribution to persist"

let flat_attribution_does_not_require_a_mark () =
  let account = test_account () in
  let account =
    apply_trade account ~id:"buy" ~side:T.Order.Buy ~quantity_value:"2"
      ~price_value:"100" ~fee_value:"1"
  in
  let account =
    T.Account.apply_cash_dividend account
      ~instrument_id:(instrument_id "test-equity")
      ~quote_currency:"USD" ~amount_per_unit:(money "3")
    |> ok
  in
  let account =
    apply_trade account ~id:"sell-long" ~side:T.Order.Sell ~quantity_value:"2"
      ~price_value:"110" ~fee_value:"1"
  in
  let account =
    apply_trade account ~id:"sell-short" ~side:T.Order.Sell ~quantity_value:"1"
      ~price_value:"100" ~fee_value:"1"
  in
  let account =
    T.Account.apply_borrow_fee account
      ~instrument_id:(instrument_id "test-equity")
      ~quote_currency:"USD" ~fee:(money "2")
    |> ok
  in
  let account =
    apply_trade account ~id:"cover" ~side:T.Order.Buy ~quantity_value:"1"
      ~price_value:"90" ~fee_value:"1"
  in
  let valuation = account_value account ~marks:[] in
  let attribution =
    match valuation.positions with
    | [ value ] -> value
    | _ -> Alcotest.fail "expected one flat position attribution"
  in
  Alcotest.check quantity_testable "flat quantity" T.Scalar.Quantity.zero
    attribution.quantity;
  Alcotest.check price_testable "canonical flat mark" (price "1")
    attribution.mark;
  Alcotest.check money_testable "attributed realized P&L" (money "30")
    attribution.realized_pnl;
  Alcotest.check money_testable "attributed dividend P&L" (money "6")
    attribution.dividend_pnl;
  Alcotest.check money_testable "attributed execution fees" (money "4")
    attribution.execution_fees;
  Alcotest.check money_testable "attributed borrow fees" (money "2")
    attribution.borrow_fees;
  Alcotest.check money_testable "attributed total fees" (money "6")
    attribution.total_fees;
  Alcotest.check money_testable "aggregate realized P&L"
    attribution.base_realized_pnl valuation.realized_pnl;
  Alcotest.check money_testable "aggregate dividend P&L"
    attribution.base_dividend_pnl valuation.dividend_pnl;
  Alcotest.check money_testable "aggregate execution fees"
    attribution.base_execution_fees valuation.execution_fees;
  Alcotest.check money_testable "aggregate borrow fees"
    attribution.base_borrow_fees valuation.borrow_fees;
  Alcotest.check money_testable "aggregate total fees"
    attribution.base_total_fees valuation.total_fees

let sell_opens_short_position () =
  let account = test_account ~initial_cash:[ ("USD", money "1000") ] () in
  let sell =
    request ~side:T.Order.Sell ~quantity_value:"1" ()
    |> accepted_order |> fill ~quantity_value:"1"
  in
  let account = T.Account.apply_fill account sell |> ok in
  let position = T.Account.position account (instrument_id "test-equity") in
  Alcotest.check quantity_testable "one unit short" (quantity "-1")
    position.quantity;
  Alcotest.check money_testable "short proceeds settle to cash" (money "1100")
    (account_cash account);
  Alcotest.check money_testable "short basis is signed" (money "-100")
    position.cost_basis;
  let marked =
    account_value account ~marks:[ (instrument_id "test-equity", price "90") ]
  in
  Alcotest.check money_testable "short mark profit" (money "10")
    marked.unrealized_pnl

let fills_settle_to_explicit_margin_cash () =
  let account = test_account ~initial_cash:[ ("USD", money "50") ] () in
  let buy =
    request ~quantity_value:"1" ()
    |> accepted_order
    |> fill ~quantity_value:"1" ~price_value:"100"
  in
  let account = T.Account.apply_fill account buy |> ok in
  Alcotest.check money_testable "buy can create a margin debit" (money "-50")
    (account_cash account);
  let funded = test_account ~initial_cash:[ ("USD", money "100") ] () in
  let funded =
    apply_trade funded ~id:"fee-position" ~side:T.Order.Buy ~quantity_value:"1"
      ~price_value:"50" ~fee_value:"0"
  in
  let expensive_sell =
    request ~side:T.Order.Sell ~quantity_value:"1" ()
    |> accepted_order ~id:"order-expensive-sell"
    |> fill ~id:"fill-expensive-sell" ~quantity_value:"1" ~price_value:"1"
         ~fee_value:"52"
  in
  let funded = T.Account.apply_fill funded expensive_sell |> ok in
  Alcotest.check money_testable "fees settle even when cash becomes negative"
    (money "-1") (account_cash funded)

let risk_reserves_working_sells () =
  let account = test_account () in
  let account =
    apply_trade account ~id:"position" ~side:T.Order.Buy ~quantity_value:"10"
      ~price_value:"100" ~fee_value:"0"
  in
  let risk = risk () in
  let first = request ~side:T.Order.Sell ~quantity_value:"6" () in
  Alcotest.check
    Alcotest.(result unit string)
    "first sell passes" (Ok ())
    (risk_check risk ~account ~oms:T.Oms.empty first);
  let oms, _ = oms_with_order first in
  let second = request ~side:T.Order.Sell ~quantity_value:"5" () in
  Alcotest.(check bool)
    "second sell oversubscribes holdings" true
    (Result.is_error (risk_check risk ~account ~oms second))

let add_working_order oms ~id ~accepted_sequence request =
  T.Oms.accept oms ~id:(order_id id)
    ~created_event_id:(event_id (id ^ "-event"))
    ~accepted_sequence
    ~created_at:(timestamp "2026-01-02T21:00:02Z")
    ~eligible_after_slice_sequence:1L request
  |> ok |> fst

let risk_rejects_self_crossing_orders () =
  let account = test_account () in
  let configured = risk () in
  let buy = request ~side:T.Order.Buy ~quantity_value:"4" () in
  let buy_oms =
    add_working_order T.Oms.empty ~id:"buy" ~accepted_sequence:1L buy
  in
  let sell = request ~side:T.Order.Sell ~quantity_value:"1" () in
  Alcotest.(check string)
    "sell against working buy"
    "order would self-cross an active opposite-side order"
    (risk_check configured ~account ~oms:buy_oms sell |> error);
  let sell_oms =
    add_working_order T.Oms.empty ~id:"sell" ~accepted_sequence:1L sell
  in
  Alcotest.(check string)
    "buy against working sell"
    "order would self-cross an active opposite-side order"
    (risk_check configured ~account ~oms:sell_oms buy |> error)

let risk_reserves_partial_order_remainders () =
  let account = test_account () in
  let configured = risk ~max_long:"5" () in
  let oms, order =
    oms_with_order (request ~side:T.Order.Buy ~quantity_value:"10" ())
  in
  let partial = fill ~quantity_value:"6" order in
  let oms =
    match T.Oms.apply_fill oms partial with
    | Ok (oms, T.Oms.Applied _) -> oms
    | Ok (_, T.Oms.Duplicate) -> Alcotest.fail "expected an applied fill"
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check (result unit string))
    "one unit fits after partial fill" (Ok ())
    (risk_check configured ~account ~oms
       (request ~side:T.Order.Buy ~quantity_value:"1" ()));
  Alcotest.(check string)
    "two units exceed the reserved long limit"
    "position would exceed the instrument maximum long position"
    (risk_check configured ~account ~oms
       (request ~side:T.Order.Buy ~quantity_value:"2" ())
    |> error)

let risk_values_directional_reservations_without_netting () =
  let primary = instrument () in
  let hedge = instrument ~id:"hedge" ~symbol:"HEDGE" () in
  let configured = risk ~instruments:[ primary; hedge ] ~max_gross:"1000" () in
  let account = test_account () in
  let primary_id = instrument_id "test-equity" in
  let buy =
    request ~instrument:primary_id ~side:T.Order.Buy ~quantity_value:"8" ()
  in
  let sell =
    request ~instrument:primary_id ~side:T.Order.Sell ~quantity_value:"8" ()
  in
  let oms =
    add_working_order T.Oms.empty ~id:"buy" ~accepted_sequence:1L buy
    |> fun oms -> add_working_order oms ~id:"sell" ~accepted_sequence:2L sell
  in
  let result =
    T.Risk.check configured ~account ~oms
      ~marks:[ (primary_id, price "100"); (instrument_id "hedge", price "100") ]
      ~fx_rates:[ ("USD", price "1") ]
      (request ~instrument:(instrument_id "hedge") ~side:T.Order.Buy
         ~quantity_value:"4" ())
  in
  Alcotest.(check string)
    "opposing reservations retain their directional exposure"
    "portfolio would exceed maximum gross exposure" (result |> error)

let accounting_identity_property =
  let open QCheck2 in
  let generator =
    Gen.bind (Gen.int_range 1 100) (fun bought ->
        Gen.map
          (fun (sold, buy_price, sell_price, mark) ->
            (bought, sold, buy_price, sell_price, mark))
          Gen.(
            quad (int_range 0 bought) (int_range 1 500) (int_range 1 500)
              (int_range 1 500)))
  in
  Test.make ~name:"equity change equals realized plus unrealized" ~count:500
    generator (fun (bought, sold, buy_price, sell_price, mark) ->
      let int_string = string_of_int in
      try
        let initial = money "1000000" in
        let account = test_account ~initial_cash:[ ("USD", initial) ] () in
        let account =
          apply_trade account ~id:"property-buy" ~side:T.Order.Buy
            ~quantity_value:(int_string bought)
            ~price_value:(int_string buy_price) ~fee_value:"0.01"
        in
        let account =
          if sold = 0 then account
          else
            apply_trade account ~id:"property-sell" ~side:T.Order.Sell
              ~quantity_value:(int_string sold)
              ~price_value:(int_string sell_price) ~fee_value:"0.01"
        in
        let valuation =
          account_value account
            ~marks:[ (instrument_id "test-equity", price (int_string mark)) ]
        in
        let change = T.Scalar.Money.subtract valuation.equity initial |> ok in
        let pnl =
          T.Scalar.Money.add valuation.realized_pnl valuation.unrealized_pnl
          |> ok
        in
        let remaining =
          T.Account.position account (instrument_id "test-equity")
        in
        T.Scalar.Money.equal change pnl
        && Int64.equal
             (T.Scalar.Quantity.to_micros remaining.quantity)
             (Int64.mul (Int64.of_int (bought - sold)) T.Scalar.Quantity.scale)
        &&
        if bought = sold then
          T.Scalar.Money.equal remaining.cost_basis T.Scalar.Money.zero
        else T.Scalar.Money.compare remaining.cost_basis T.Scalar.Money.zero > 0
      with Alcotest.Test_error -> false)

let tests =
  [
    Alcotest.test_case "exact cost basis and P&L" `Quick
      exact_cost_basis_and_pnl;
    Alcotest.test_case "flat attribution does not require a mark" `Quick
      flat_attribution_does_not_require_a_mark;
    Alcotest.test_case "sell opens a short position" `Quick
      sell_opens_short_position;
    Alcotest.test_case "fills settle to explicit margin cash" `Quick
      fills_settle_to_explicit_margin_cash;
    Alcotest.test_case "risk reserves working sells" `Quick
      risk_reserves_working_sells;
    Alcotest.test_case "risk rejects self-crossing orders" `Quick
      risk_rejects_self_crossing_orders;
    Alcotest.test_case "risk reserves partial-order remainders" `Quick
      risk_reserves_partial_order_remainders;
    Alcotest.test_case "risk values directional reservations without netting"
      `Quick risk_values_directional_reservations_without_netting;
    QCheck_alcotest.to_alcotest ~speed_level:`Quick accounting_identity_property;
  ]
