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
  let account = T.Account.create ~initial_cash:(money "10000") in
  let account =
    apply_trade account ~id:"buy" ~side:T.Order.Buy ~quantity_value:"10"
      ~price_value:"100" ~fee_value:"1"
  in
  Alcotest.check money_testable "cash after buy" (money "8999")
    (T.Account.cash account);
  let position = T.Account.position account (instrument_id "test-equity") in
  Alcotest.check quantity_testable "ten shares" (quantity "10")
    position.quantity;
  Alcotest.check money_testable "fee included in basis" (money "1001")
    position.cost_basis;
  let marked =
    T.Account.value account
      ~marks:[ (instrument_id "test-equity", price "110") ]
    |> ok
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
    (T.Account.cash account);
  Alcotest.check money_testable "partial realized" (money "79.1")
    (T.Account.realized_pnl account);
  let remaining = T.Account.position account (instrument_id "test-equity") in
  Alcotest.check quantity_testable "six shares remain" (quantity "6")
    remaining.quantity;
  Alcotest.check money_testable "proportional basis" (money "600.6")
    remaining.cost_basis;
  let account =
    apply_trade account ~id:"sell-two" ~side:T.Order.Sell ~quantity_value:"6"
      ~price_value:"90" ~fee_value:"0.5"
  in
  Alcotest.check money_testable "final cash" (money "10018")
    (T.Account.cash account);
  Alcotest.check money_testable "final realized" (money "18")
    (T.Account.realized_pnl account);
  Alcotest.check money_testable "all fees" (money "2")
    (T.Account.total_fees account);
  Alcotest.check quantity_testable "position closed" T.Scalar.Quantity.zero
    (T.Account.position_quantity account (instrument_id "test-equity"));
  let closed =
    T.Account.value account ~marks:[ (instrument_id "test-equity", price "95") ]
    |> ok
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

let sell_cannot_make_position_negative () =
  let account = T.Account.create ~initial_cash:(money "1000") in
  let sell =
    request ~side:T.Order.Sell ~quantity_value:"1" ()
    |> accepted_order |> fill ~quantity_value:"1"
  in
  Alcotest.(check bool)
    "naked sell rejected" true
    (Result.is_error (T.Account.apply_fill account sell))

let fills_cannot_make_cash_negative () =
  let account = T.Account.create ~initial_cash:(money "50") in
  let buy =
    request ~quantity_value:"1" ()
    |> accepted_order
    |> fill ~quantity_value:"1" ~price_value:"100"
  in
  Alcotest.(check bool)
    "unaffordable buy rejected" true
    (Result.is_error (T.Account.apply_fill account buy));
  let funded = T.Account.create ~initial_cash:(money "100") in
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
  Alcotest.(check bool)
    "sell fee cannot overdraw cash" true
    (Result.is_error (T.Account.apply_fill funded expensive_sell))

let risk_reserves_working_sells () =
  let account = T.Account.create ~initial_cash:(money "10000") in
  let account =
    apply_trade account ~id:"position" ~side:T.Order.Buy ~quantity_value:"10"
      ~price_value:"100" ~fee_value:"0"
  in
  let risk = risk () in
  let first = request ~side:T.Order.Sell ~quantity_value:"6" () in
  Alcotest.check
    Alcotest.(result unit string)
    "first sell passes" (Ok ())
    (T.Risk.check risk ~account ~oms:T.Oms.empty first);
  let oms, _ = oms_with_order first in
  let second = request ~side:T.Order.Sell ~quantity_value:"5" () in
  Alcotest.(check bool)
    "second sell oversubscribes holdings" true
    (Result.is_error (T.Risk.check risk ~account ~oms second))

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
        let account = T.Account.create ~initial_cash:initial in
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
          T.Account.value account
            ~marks:[ (instrument_id "test-equity", price (int_string mark)) ]
          |> ok
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
             (T.Scalar.Quantity.to_int64 remaining.quantity)
             (Int64.of_int (bought - sold))
        &&
        if bought = sold then
          T.Scalar.Money.equal remaining.cost_basis T.Scalar.Money.zero
        else T.Scalar.Money.compare remaining.cost_basis T.Scalar.Money.zero > 0
      with Alcotest.Test_error -> false)

let tests =
  [
    Alcotest.test_case "exact cost basis and P&L" `Quick
      exact_cost_basis_and_pnl;
    Alcotest.test_case "sell cannot create negative position" `Quick
      sell_cannot_make_position_negative;
    Alcotest.test_case "fills cannot create negative cash" `Quick
      fills_cannot_make_cash_negative;
    Alcotest.test_case "risk reserves working sells" `Quick
      risk_reserves_working_sells;
    QCheck_alcotest.to_alcotest ~speed_level:`Quick accounting_identity_property;
  ]
