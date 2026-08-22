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

let reserved_result ?(side = T.Order.Buy) ?(initial_cash = "10000")
    ?(max_long = "100") ?(max_short = "100") ?(max_notional = "100000")
    ?(shorting_allowed = true) ?(initial_margin_bps = 5000)
    ?(max_gross = "100000") ?(max_leverage = "10") ?group_limits
    ?(include_mark = true) ?(include_fx = true) () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let second = instrument ~id:"second" ~symbol:"SECOND" () in
  let groups =
    Option.to_list
      (Option.map
         (fun value -> group "group-a" [ first; second ] value)
         group_limits)
  in
  let risk =
    T.Risk.create_v7 ~base_currency:"USD" ~instruments:[ first; second ]
      ~instrument_policies:
        [
          policy first ~max_long ~max_short ~max_notional ~shorting_allowed
            ~initial_margin_bps
            ~maintenance_margin_bps:(min initial_margin_bps 2500)
            ();
          policy second ();
        ]
      ~groups ~max_gross_exposure:(money max_gross)
      ~max_leverage:(T.Scalar.Ratio.of_decimal_string max_leverage |> ok)
      ~short_borrow_bps:0
    |> ok
  in
  let account = test_account ~initial_cash:[ ("USD", money initial_cash) ] () in
  let oms, order =
    request ~instrument:first.id ~side ~quantity_value:"10" () |> oms_with_order
  in
  let candidate = fill ~quantity_value:"1" order in
  let account = T.Account.apply_fill account candidate |> ok in
  let valuation =
    account_value ~instruments:[ first; second ] account
      ~marks:[ (first.id, price "100"); (second.id, price "100") ]
  in
  T.Risk.check_reserved_fill risk ~account ~oms
    ~marks:
      (if include_mark then
         [ (first.id, price "100"); (second.id, price "100") ]
       else [])
    ~fx_rates:(if include_fx then [ ("USD", price "1") ] else [])
    ~order ~filled_quantity:(quantity "1") ~after:valuation

let clipping_taxonomy_is_exact () =
  let is_expected expected = function
    | Error (T.Risk.Limit actual) when expected actual -> ()
    | _ -> Alcotest.fail "unexpected reserved fill result"
  in
  reserved_result ~max_long:"7" ()
  |> is_expected (function
    | T.Risk.Instrument_maximum_long_position _ -> true
    | _ -> false);
  reserved_result ~side:T.Order.Sell ~max_short:"7" ()
  |> is_expected (function
    | T.Risk.Instrument_maximum_short_position _ -> true
    | _ -> false);
  reserved_result ~max_notional:"700" ()
  |> is_expected (function
    | T.Risk.Instrument_maximum_notional _ -> true
    | _ -> false);
  reserved_result ~side:T.Order.Sell ~shorting_allowed:false ()
  |> is_expected (function
    | T.Risk.Instrument_shorting_disabled _ -> true
    | _ -> false);
  reserved_result ~max_gross:"700" ()
  |> is_expected (function
    | T.Risk.Maximum_gross_exposure _ -> true
    | _ -> false);
  reserved_result ~max_leverage:"0.05" ()
  |> is_expected (function T.Risk.Maximum_leverage _ -> true | _ -> false);
  reserved_result ~initial_cash:"500" ~initial_margin_bps:10_000 ()
  |> is_expected (function
    | T.Risk.Instrument_initial_margin _ -> true
    | _ -> false);
  reserved_result ~group_limits:(limits ~long:"700" ()) ()
  |> is_expected (function T.Risk.Group_maximum_long _ -> true | _ -> false);
  reserved_result ~side:T.Order.Sell ~group_limits:(limits ~short:"700" ()) ()
  |> is_expected (function T.Risk.Group_maximum_short _ -> true | _ -> false);
  reserved_result ~group_limits:(limits ~absolute_net:"700" ()) ()
  |> is_expected (function
    | T.Risk.Group_maximum_absolute_net _ -> true
    | _ -> false);
  reserved_result ~group_limits:(limits ~concentration:"0.05" ()) ()
  |> is_expected (function
    | T.Risk.Group_maximum_concentration _ -> true
    | _ -> false)

let constructors_reject_ambiguous_policies () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let bad_policy ?(max_order = "10") ?(max_long = "10") ?(max_short = "10")
      ?(notional = Some (money "100")) ?(initial = 5000) ?(maintenance = 2500)
      () =
    T.Risk.create_instrument_policy ~instrument:first
      ~max_order_quantity:(quantity max_order)
      ~max_long_position:(quantity max_long)
      ~max_short_position:(quantity max_short) ~max_notional_exposure:notional
      ~initial_margin_bps:initial ~maintenance_margin_bps:maintenance
      ~shorting_allowed:true
  in
  List.iter
    (fun result ->
      Alcotest.(check bool) "invalid policy" true (Result.is_error result))
    [
      bad_policy ~max_order:"0" ();
      bad_policy ~max_long:"0" ();
      bad_policy ~max_short:"0" ();
      bad_policy ~notional:(Some (money "0")) ();
      bad_policy ~initial:0 ();
      bad_policy ~initial:10_001 ();
      bad_policy ~maintenance:0 ();
      bad_policy ~maintenance:10_001 ();
      bad_policy ~initial:1000 ~maintenance:2000 ();
    ];
  List.iter
    (fun result ->
      Alcotest.(check bool) "invalid group limits" true (Result.is_error result))
    [
      T.Risk.create_group_limits ~max_gross_exposure:None
        ~max_long_exposure:None ~max_short_exposure:None
        ~max_absolute_net_exposure:None ~max_concentration:None;
      T.Risk.create_group_limits
        ~max_gross_exposure:(Some (money "0"))
        ~max_long_exposure:None ~max_short_exposure:None
        ~max_absolute_net_exposure:None ~max_concentration:None;
      T.Risk.create_group_limits ~max_gross_exposure:None
        ~max_long_exposure:None ~max_short_exposure:None
        ~max_absolute_net_exposure:None
        ~max_concentration:(Some (T.Scalar.Ratio.of_decimal_string "2" |> ok));
    ];
  let valid_limits = limits ~gross:"100" () in
  Alcotest.(check bool)
    "empty group rejected" true
    (Result.is_error
       (T.Risk.create_group
          ~group_id:(T.Id.Risk_group.of_string_exn "group")
          ~group_kind:T.Risk.Custom ~instrument_ids:[] ~limits:valid_limits));
  Alcotest.(check bool)
    "duplicate membership rejected" true
    (Result.is_error
       (T.Risk.create_group
          ~group_id:(T.Id.Risk_group.of_string_exn "group")
          ~group_kind:T.Risk.Custom ~instrument_ids:[ first.id; first.id ]
          ~limits:valid_limits))

let create_v7_rejects_inconsistent_configuration () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let second = instrument ~id:"second" ~symbol:"SECOND" () in
  let first_policy = policy first () in
  let second_policy = policy second () in
  let unknown = instrument ~id:"unknown" ~symbol:"UNKNOWN" () in
  let unknown_policy = policy unknown () in
  let valid_group = group "group" [ first ] (limits ~gross:"100" ()) in
  let unknown_group =
    group "unknown-group" [ unknown ] (limits ~gross:"100" ())
  in
  let create ?(base_currency = "USD") ?(instruments = [ first; second ])
      ?(policies = [ first_policy; second_policy ]) ?(groups = [ valid_group ])
      ?(max_gross = "100000") ?(short_borrow_bps = 0) () =
    T.Risk.create_v7 ~base_currency ~instruments ~instrument_policies:policies
      ~groups ~max_gross_exposure:(money max_gross)
      ~max_leverage:(T.Scalar.Ratio.of_decimal_string "10" |> ok)
      ~short_borrow_bps
  in
  List.iter
    (fun result ->
      Alcotest.(check bool)
        "invalid v7 configuration" true (Result.is_error result))
    [
      create ~base_currency:"" ();
      create ~instruments:[] ~policies:[] ~groups:[] ();
      create ~max_gross:"0" ();
      create ~short_borrow_bps:(-1) ();
      create ~short_borrow_bps:10_001 ();
      create ~instruments:[ first; first ] ();
      create ~policies:[ first_policy; unknown_policy ] ();
      create ~policies:[ first_policy; first_policy ] ();
      create ~policies:[ first_policy ] ();
      create ~groups:[ valid_group; valid_group ] ();
      create ~groups:[ unknown_group ] ();
    ]

let admission_result ?(side = T.Order.Buy) ?(initial_cash = "10000")
    ?(max_order = "100") ?(max_long = "100") ?(max_short = "100")
    ?(max_notional = "100000") ?(shorting_allowed = true)
    ?(initial_margin_bps = 5000) ?(max_gross = "100000") ?(max_leverage = "10")
    ?group_limits () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let second = instrument ~id:"second" ~symbol:"SECOND" () in
  let groups =
    Option.to_list
      (Option.map
         (fun value -> group "group-a" [ first; second ] value)
         group_limits)
  in
  let risk =
    T.Risk.create_v7 ~base_currency:"USD" ~instruments:[ first; second ]
      ~instrument_policies:
        [
          policy first ~max_order ~max_long ~max_short ~max_notional
            ~shorting_allowed ~initial_margin_bps
            ~maintenance_margin_bps:(min initial_margin_bps 2500)
            ();
          policy second ();
        ]
      ~groups ~max_gross_exposure:(money max_gross)
      ~max_leverage:(T.Scalar.Ratio.of_decimal_string max_leverage |> ok)
      ~short_borrow_bps:0
    |> ok
  in
  T.Risk.check risk
    ~account:(test_account ~initial_cash:[ ("USD", money initial_cash) ] ())
    ~oms:T.Oms.empty
    ~marks:[ (first.id, price "100"); (second.id, price "100") ]
    ~fx_rates:[ ("USD", price "1") ]
    (request ~instrument:first.id ~side ~quantity_value:"10" ())

let admission_enforces_every_v7_limit () =
  let check_error expected result =
    Alcotest.(check string) "admission error" expected (error result)
  in
  admission_result ~max_order:"7" ()
  |> check_error "order exceeds the instrument maximum order quantity";
  admission_result ~max_long:"7" ()
  |> check_error "position would exceed the instrument maximum long position";
  admission_result ~side:T.Order.Sell ~max_short:"7" ()
  |> check_error "position would exceed the instrument maximum short position";
  admission_result ~side:T.Order.Sell ~shorting_allowed:false ()
  |> check_error "instrument policy does not allow short positions";
  admission_result ~max_notional:"700" ()
  |> check_error
       "position would exceed the instrument maximum notional exposure";
  admission_result ~max_gross:"700" ()
  |> check_error "portfolio would exceed maximum gross exposure";
  admission_result ~max_leverage:"0.05" ()
  |> check_error "portfolio would exceed maximum leverage";
  admission_result ~initial_cash:"500" ~initial_margin_bps:10_000 ()
  |> check_error
       "portfolio would violate instrument initial margin requirements";
  admission_result ~group_limits:(limits ~long:"700" ()) ()
  |> check_error "position would exceed group group-a maximum long exposure";
  admission_result ~side:T.Order.Sell ~group_limits:(limits ~short:"700" ()) ()
  |> check_error "position would exceed group group-a maximum short exposure";
  admission_result ~group_limits:(limits ~absolute_net:"700" ()) ()
  |> check_error
       "position would exceed group group-a maximum absolute net exposure";
  admission_result ~group_limits:(limits ~concentration:"0.05" ()) ()
  |> check_error "position would exceed group group-a maximum concentration"

let group_exposures_include_short_and_zero_equity () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let second = instrument ~id:"second" ~symbol:"SECOND" () in
  let risk =
    T.Risk.create_v7 ~base_currency:"USD" ~instruments:[ first; second ]
      ~instrument_policies:[ policy first (); policy second () ]
      ~groups:[ group "group" [ first ] (limits ~gross:"100000" ()) ]
      ~max_gross_exposure:(money "100000")
      ~max_leverage:(T.Scalar.Ratio.of_decimal_string "10" |> ok)
      ~short_borrow_bps:0
    |> ok
  in
  let order =
    request ~instrument:first.id ~side:T.Order.Sell ~quantity_value:"1" ()
    |> accepted_order
  in
  let account =
    test_account ~initial_cash:[ ("USD", money "0") ] () |> fun account ->
    T.Account.apply_fill account (fill order) |> ok
  in
  let valuation =
    account_value ~instruments:[ first; second ] account
      ~marks:[ (first.id, price "100"); (second.id, price "100") ]
  in
  let exposure = T.Risk.group_exposures risk valuation |> ok |> List.hd in
  Alcotest.check money_testable "short exposure" (money "100")
    exposure.short_exposure;
  Alcotest.(check bool)
    "zero-equity concentration omitted" true
    (Option.is_none exposure.concentration)

let initial_result ?(side = T.Order.Buy) ?(initial_cash = "10000")
    ?(max_long = "100") ?(max_short = "100") ?(max_notional = "100000")
    ?(shorting_allowed = true) ?(initial_margin_bps = 5000)
    ?(max_gross = "100000") ?(max_leverage = "10") ?group_limits () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let second = instrument ~id:"second" ~symbol:"SECOND" () in
  let groups =
    Option.to_list
      (Option.map
         (fun value -> group "group-a" [ first; second ] value)
         group_limits)
  in
  let risk =
    T.Risk.create_v7 ~base_currency:"USD" ~instruments:[ first; second ]
      ~instrument_policies:
        [
          policy first ~max_long ~max_short ~max_notional ~shorting_allowed
            ~initial_margin_bps
            ~maintenance_margin_bps:(min initial_margin_bps 2500)
            ();
          policy second ();
        ]
      ~groups ~max_gross_exposure:(money max_gross)
      ~max_leverage:(T.Scalar.Ratio.of_decimal_string max_leverage |> ok)
      ~short_borrow_bps:0
    |> ok
  in
  let order =
    request ~instrument:first.id ~side ~quantity_value:"10" () |> accepted_order
  in
  let account =
    test_account ~initial_cash:[ ("USD", money initial_cash) ] ()
    |> fun account ->
    T.Account.apply_fill account (fill ~quantity_value:"10" order) |> ok
  in
  let valuation =
    account_value ~instruments:[ first; second ] account
      ~marks:[ (first.id, price "100"); (second.id, price "100") ]
  in
  T.Risk.check_initial risk valuation

let initial_portfolio_enforces_every_v7_limit () =
  let check_error expected result =
    Alcotest.(check string) "initial portfolio error" expected (error result)
  in
  initial_result ~max_gross:"700" ()
  |> check_error "portfolio would exceed maximum gross exposure";
  initial_result ~max_leverage:"0.05" ()
  |> check_error "portfolio would exceed maximum leverage";
  initial_result ~max_long:"7" ()
  |> check_error "initial position exceeds its maximum long position";
  initial_result ~side:T.Order.Sell ~max_short:"7" ()
  |> check_error "initial position exceeds its maximum short position";
  initial_result ~side:T.Order.Sell ~shorting_allowed:false ()
  |> check_error "initial position violates its shorting policy";
  initial_result ~max_notional:"700" ()
  |> check_error
       "initial position exceeds the instrument maximum notional exposure";
  initial_result ~initial_cash:"500" ~initial_margin_bps:10_000 ()
  |> check_error
       "portfolio would violate instrument initial margin requirements";
  initial_result ~group_limits:(limits ~long:"700" ()) ()
  |> check_error "initial portfolio exceeds group group-a maximum long exposure";
  initial_result ~side:T.Order.Sell ~group_limits:(limits ~short:"700" ()) ()
  |> check_error
       "initial portfolio exceeds group group-a maximum short exposure";
  initial_result ~group_limits:(limits ~absolute_net:"700" ()) ()
  |> check_error
       "initial portfolio exceeds group group-a maximum absolute net exposure";
  initial_result ~group_limits:(limits ~concentration:"0.05" ()) ()
  |> check_error "initial portfolio exceeds group group-a maximum concentration"

let legacy_and_policy_boundaries_are_rejected () =
  let first = instrument ~id:"first" ~symbol:"FIRST" () in
  let large_lot = instrument ~id:"large" ~symbol:"LARGE" ~lot_size:"2" () in
  let ratio = T.Scalar.Ratio.of_decimal_string "10" |> ok in
  let create ?(base_currency = "USD") ?(instruments = [ first ])
      ?(max_order = "10") ?(max_long = "10") ?(max_short = "10")
      ?(max_gross = "1000") ?(initial = 5000) ?(maintenance = 2500)
      ?(borrow = 0) () =
    T.Risk.create ~base_currency ~instruments
      ~max_order_quantity:(quantity max_order)
      ~max_long_position:(quantity max_long)
      ~max_short_position:(quantity max_short)
      ~max_gross_exposure:(money max_gross) ~max_leverage:ratio
      ~initial_margin_bps:initial ~maintenance_margin_bps:maintenance
      ~short_borrow_bps:borrow
  in
  List.iter
    (fun result ->
      Alcotest.(check bool) "invalid legacy risk" true (Result.is_error result))
    [
      create ~base_currency:"" ();
      create ~max_order:"0" ();
      create ~max_long:"0" ();
      create ~max_short:"0" ();
      create ~max_gross:"0" ();
      create ~initial:0 ();
      create ~maintenance:0 ();
      create ~initial:10_001 ();
      create ~maintenance:10_001 ();
      create ~initial:1000 ~maintenance:2000 ();
      create ~borrow:(-1) ();
      create ~borrow:10_001 ();
      create ~instruments:[] ();
      create ~instruments:[ large_lot ] ~max_order:"1" ();
      create ~instruments:[ large_lot ] ~max_long:"1" ();
      create ~instruments:[ large_lot ] ~max_short:"1" ();
      create ~instruments:[ first; first ] ();
    ];
  let invalid_policy value selector =
    T.Risk.create_instrument_policy ~instrument:large_lot
      ~max_order_quantity:(quantity (selector "order" value))
      ~max_long_position:(quantity (selector "long" value))
      ~max_short_position:(quantity (selector "short" value))
      ~max_notional_exposure:None ~initial_margin_bps:5000
      ~maintenance_margin_bps:2500 ~shorting_allowed:true
  in
  let selected field value target =
    if String.equal field target then value else "10"
  in
  List.iter
    (fun result ->
      Alcotest.(check bool) "policy below lot" true (Result.is_error result))
    [
      invalid_policy "1" (fun field value -> selected field value "order");
      invalid_policy "1" (fun field value -> selected field value "long");
      invalid_policy "1" (fun field value -> selected field value "short");
    ]

let public_checks_cover_success_and_diagnostics () =
  let legacy = risk ~max_order:"7" ~max_long:"7" ~max_short:"7" () in
  Alcotest.(check bool)
    "position accepted" true
    (Result.is_ok (T.Risk.check_position legacy (quantity "7")));
  Alcotest.(check string)
    "long rejected" "position would exceed the maximum long position"
    (T.Risk.check_position legacy (quantity "8") |> error);
  Alcotest.(check string)
    "short rejected" "position would exceed the maximum short position"
    (T.Risk.check_position legacy (quantity "-8") |> error);
  Alcotest.(check string)
    "unknown policy" "position refers to an unknown instrument risk policy"
    (T.Risk.check_position_for legacy (instrument_id "unknown") (quantity "1")
    |> error);
  let unknown_request = request ~instrument:(instrument_id "unknown") () in
  Alcotest.(check string)
    "unknown instrument" "order refers to an unknown instrument"
    (T.Risk.check legacy ~account:(test_account ()) ~oms:T.Oms.empty ~marks:[]
       ~fx_rates:[] unknown_request
    |> error);
  Alcotest.(check string)
    "legacy order limit" "order exceeds the maximum order quantity"
    (risk_check legacy ~account:(test_account ()) ~oms:T.Oms.empty
       (request ~quantity_value:"8" ())
    |> error);
  (match reserved_result ~include_mark:false () with
  | Error (T.Risk.Invalid message) ->
      Alcotest.(check string)
        "missing mark" "projected position has no current market price" message
  | _ -> Alcotest.fail "expected missing mark diagnostic");
  (match reserved_result ~include_fx:false () with
  | Error (T.Risk.Invalid message) ->
      Alcotest.(check string)
        "missing FX" "projected position has no current FX rate" message
  | _ -> Alcotest.fail "expected missing FX diagnostic");
  Alcotest.(check bool)
    "unconstrained group fill accepted" true
    (Result.is_ok (reserved_result ~group_limits:(limits ~gross:"10000" ()) ()));
  Alcotest.(check bool)
    "concentration-compliant fill accepted" true
    (Result.is_ok
       (reserved_result ~group_limits:(limits ~concentration:"1" ()) ()));
  Alcotest.(check bool)
    "unconstrained group admission accepted" true
    (Result.is_ok
       (admission_result ~group_limits:(limits ~gross:"10000" ()) ()))

let legacy_post_fill_covers_gross_and_reduction () =
  let first = instrument () in
  let before_account = test_account () in
  let before =
    account_value before_account ~marks:[ (first.id, price "100") ]
  in
  let order =
    request ~instrument:first.id ~quantity_value:"10" () |> accepted_order
  in
  let after_account =
    T.Account.apply_fill before_account (fill ~quantity_value:"10" order) |> ok
  in
  let after = account_value after_account ~marks:[ (first.id, price "100") ] in
  let limited = risk ~max_gross:"700" () in
  (match
     T.Risk.check_post_fill limited ~before_position:(quantity "0")
       ~after_position:(quantity "10") ~before ~after
   with
  | Error (T.Risk.Limit (T.Risk.Maximum_gross_exposure _)) -> ()
  | _ -> Alcotest.fail "expected legacy gross fill limit");
  Alcotest.(check bool)
    "gross-reducing fill accepted" true
    (Result.is_ok
       (T.Risk.check_post_fill limited ~before_position:(quantity "10")
          ~after_position:(quantity "9") ~before:after ~after:before))

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
    Alcotest.test_case "clipping taxonomy is exact" `Quick
      clipping_taxonomy_is_exact;
    Alcotest.test_case "constructors reject ambiguous policies" `Quick
      constructors_reject_ambiguous_policies;
    Alcotest.test_case "v7 rejects inconsistent configuration" `Quick
      create_v7_rejects_inconsistent_configuration;
    Alcotest.test_case "admission enforces every v7 limit" `Quick
      admission_enforces_every_v7_limit;
    Alcotest.test_case "group exposures include short and zero equity" `Quick
      group_exposures_include_short_and_zero_equity;
    Alcotest.test_case "initial portfolio enforces every v7 limit" `Quick
      initial_portfolio_enforces_every_v7_limit;
    Alcotest.test_case "legacy and policy boundaries are rejected" `Quick
      legacy_and_policy_boundaries_are_rejected;
    Alcotest.test_case "public checks cover success and diagnostics" `Quick
      public_checks_cover_success_and_diagnostics;
    Alcotest.test_case "legacy post-fill covers gross and reduction" `Quick
      legacy_post_fill_covers_gross_and_reduction;
  ]
