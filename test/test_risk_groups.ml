open Test_support
module T = Trading_engine

let policy instrument ?(max_order = "100") ?(max_long = "100")
    ?(max_short = "100") ?(max_notional = "100000") ?(initial_margin_bps = 5000)
    ?(maintenance_margin_bps = 2500) ?(shorting_allowed = true) () =
  T.Risk.create_instrument_policy ~instrument
    ~max_order_quantity:(quantity max_order)
    ~max_long_position:(quantity max_long)
    ~max_short_position:(quantity max_short)
    ~max_notional_exposure:(Some (money max_notional))
    ~initial_margin_bps ~maintenance_margin_bps ~shorting_allowed
  |> ok

let limits ?gross ?long ?short ?absolute_net ?concentration () =
  T.Risk.create_group_limits ~max_gross_exposure:(Option.map money gross)
    ~max_long_exposure:(Option.map money long)
    ~max_short_exposure:(Option.map money short)
    ~max_absolute_net_exposure:(Option.map money absolute_net)
    ~max_concentration:
      (Option.map
         (fun value -> T.Scalar.Ratio.of_decimal_string value |> ok)
         concentration)
  |> ok

let group id instruments limits =
  T.Risk.create_group
    ~group_id:(T.Id.Risk_group.of_string_exn id)
    ~group_kind:T.Risk.Issuer
    ~instrument_ids:(List.map (fun item -> item.T.Instrument.id) instruments)
    ~limits
  |> ok

let setup ?(groups = []) ?(shorting_allowed = true) () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let second = instrument ~id:"second" ~symbol:"SECOND" () in
  let policies =
    [
      policy first ~shorting_allowed (); policy second ~shorting_allowed:true ();
    ]
  in
  let risk =
    T.Risk.create_v7 ~base_currency:"USD" ~instruments:[ first; second ]
      ~instrument_policies:policies ~groups ~max_gross_exposure:(money "100000")
      ~max_leverage:(T.Scalar.Ratio.of_decimal_string "10" |> ok)
      ~short_borrow_bps:0
    |> ok
  in
  (first, second, risk)

let exact_coverage_and_short_policy () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let second = instrument ~id:"second" ~symbol:"SECOND" () in
  let result =
    T.Risk.create_v7 ~base_currency:"USD" ~instruments:[ first; second ]
      ~instrument_policies:[ policy first () ]
      ~groups:[] ~max_gross_exposure:(money "100000")
      ~max_leverage:(T.Scalar.Ratio.of_decimal_string "10" |> ok)
      ~short_borrow_bps:0
  in
  Alcotest.(check string)
    "policy required for every instrument"
    "risk must define exactly one policy for every instrument" (error result);
  let first, _, risk = setup ~shorting_allowed:false () in
  let account = test_account () in
  let request =
    request ~instrument:first.id ~side:T.Order.Sell ~quantity_value:"1" ()
  in
  let result =
    T.Risk.check risk ~account ~oms:T.Oms.empty
      ~marks:[ (first.id, price "100"); (instrument_id "second", price "100") ]
      ~fx_rates:[ ("USD", price "1") ]
      request
  in
  Alcotest.(check string)
    "short prohibition is explicit"
    "instrument policy does not allow short positions" (error result)

let overlapping_groups_are_deterministic () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let second = instrument ~id:"second" ~symbol:"SECOND" () in
  let constrained = limits ~gross:"900" () in
  let groups =
    [
      group "z-group" [ first; second ] constrained;
      group "a-group" [ first; second ] constrained;
    ]
  in
  let _, _, risk = setup ~groups () in
  let account = test_account () in
  let first_request =
    request ~instrument:first.id ~quantity_value:"5"
      ~kind:(T.Order.Limit (price "100"))
      ()
  in
  let oms, _ = oms_with_order first_request in
  let second_request = request ~instrument:second.id ~quantity_value:"5" () in
  let result =
    T.Risk.check risk ~account ~oms
      ~marks:[ (first.id, price "100"); (second.id, price "100") ]
      ~fx_rates:[ ("USD", price "1") ]
      second_request
  in
  Alcotest.(check string)
    "lexically first limiting group"
    "position would exceed group a-group maximum gross exposure" (error result)

let fill_reserves_remainder_and_reports_group () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let second = instrument ~id:"second" ~symbol:"SECOND" () in
  let groups = [ group "issuer" [ first; second ] (limits ~gross:"700" ()) ] in
  let _, _, risk = setup ~groups () in
  let account = test_account () in
  let oms, order =
    request ~instrument:first.id ~quantity_value:"10" () |> oms_with_order
  in
  let candidate = fill ~quantity_value:"5" order in
  let account = T.Account.apply_fill account candidate |> ok in
  let valuation =
    account_value ~instruments:[ first; second ] account
      ~marks:[ (first.id, price "100"); (second.id, price "100") ]
  in
  (match
     T.Risk.check_reserved_fill risk ~account ~oms
       ~marks:[ (first.id, price "100"); (second.id, price "100") ]
       ~fx_rates:[ ("USD", price "1") ]
       ~order ~filled_quantity:(quantity "5") ~after:valuation
   with
  | Error (T.Risk.Limit (T.Risk.Group_maximum_gross (id, limit))) ->
      Alcotest.(check string)
        "limiting group" "issuer"
        (T.Id.Risk_group.to_string id);
      Alcotest.check money_testable "group threshold" (money "700") limit
  | _ -> Alcotest.fail "expected group gross fill limit");
  let snapshot = T.Risk.group_exposures risk valuation |> ok |> List.hd in
  Alcotest.check money_testable "actual group gross" (money "500")
    snapshot.gross_exposure

let initialized_positions_use_instrument_margin_and_groups () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let second = instrument ~id:"second" ~symbol:"SECOND" () in
  let group_limit = limits ~gross:"150" () in
  let risk =
    T.Risk.create_v7 ~base_currency:"USD" ~instruments:[ first; second ]
      ~instrument_policies:
        [
          policy first ~initial_margin_bps:10_000 ~maintenance_margin_bps:5000
            ();
          policy second ~initial_margin_bps:2500 ~maintenance_margin_bps:1000 ();
        ]
      ~groups:[ group "initial-group" [ first; second ] group_limit ]
      ~max_gross_exposure:(money "100000")
      ~max_leverage:(T.Scalar.Ratio.of_decimal_string "10" |> ok)
      ~short_borrow_bps:0
    |> ok
  in
  let account = test_account () in
  let first_order =
    request ~instrument:first.id ~quantity_value:"1" ()
    |> accepted_order ~id:"first-order"
  in
  let second_order =
    request ~instrument:second.id ~quantity_value:"1" ()
    |> accepted_order ~id:"second-order"
  in
  let account =
    T.Account.apply_fill account (fill ~id:"first-fill" first_order) |> ok
  in
  let account =
    T.Account.apply_fill account (fill ~id:"second-fill" second_order) |> ok
  in
  let valuation =
    account_value ~instruments:[ first; second ] account
      ~marks:[ (first.id, price "100"); (second.id, price "100") ]
  in
  let margin = T.Risk.margin_snapshot risk valuation |> ok in
  Alcotest.check money_testable "per-instrument initial margin" (money "125")
    margin.initial_requirement;
  Alcotest.(check string)
    "initialized group exposure enforced"
    "initial portfolio exceeds group initial-group maximum gross exposure"
    (T.Risk.check_initial risk valuation |> error)

let tests =
  [
    Alcotest.test_case "exact policy coverage and short prohibition" `Quick
      exact_coverage_and_short_policy;
    Alcotest.test_case "overlapping groups use deterministic IDs" `Quick
      overlapping_groups_are_deterministic;
    Alcotest.test_case "fill reserves remainder and reports group" `Quick
      fill_reserves_remainder_and_reports_group;
    Alcotest.test_case "initialized positions use exact risk policies" `Quick
      initialized_positions_use_instrument_margin_and_groups;
  ]
