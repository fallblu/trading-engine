open Test_support
module T = Trading_engine
module Runner = T.Engine.Make (T.Scripted_strategy)

let action_id value = T.Id.Corporate_action.of_string_exn value

let distribution_allocates_basis_and_fractional_cash () =
  let source = instrument ~id:"source" ~symbol:"SRC" () in
  let child = instrument ~id:"child" ~symbol:"CHD" () in
  let account = test_account ~initial_cash:[ ("USD", money "1000") ] () in
  let order =
    request ~instrument:source.id ~quantity_value:"3" () |> accepted_order
  in
  let account =
    T.Account.apply_fill account
      (fill ~price_value:"100" ~quantity_value:"3" order)
    |> ok
  in
  let fractional_policy =
    T.Corporate_action.Cash_in_lieu { price = price "20"; currency = "USD" }
  in
  let account, result =
    T.Account.apply_distribution account ~source_instrument_id:source.id
      ~destination_instrument_id:child.id ~destination_lot_size:child.lot_size
      ~numerator:1L ~denominator:2L ~basis_allocation_bps:2000
      ~fractional_policy
    |> ok
  in
  Alcotest.check quantity_testable "delivered child units" (quantity "1")
    result.destination_quantity;
  Alcotest.check quantity_testable "fractional child units" (quantity "0.5")
    result.fractional_quantity;
  Alcotest.check money_testable "allocated source basis" (money "60")
    result.allocated_basis;
  Alcotest.check money_testable "fractional basis" (money "20")
    result.fractional_basis;
  Alcotest.check money_testable "cash in lieu" (money "10") result.cash_in_lieu;
  Alcotest.check money_testable "source basis retained" (money "240")
    (T.Account.position account source.id).cost_basis;
  Alcotest.check money_testable "child basis delivered" (money "40")
    (T.Account.position account child.id).cost_basis;
  Alcotest.check money_testable "cash credited in declared currency"
    (money "710") (account_cash account)

let fractional_policy_is_explicit () =
  let source = instrument ~id:"source" ~symbol:"SRC" () in
  let child = instrument ~id:"child" ~symbol:"CHD" () in
  let account = test_account () in
  let order =
    request ~instrument:source.id ~quantity_value:"3" () |> accepted_order
  in
  let account =
    T.Account.apply_fill account
      (fill ~price_value:"100" ~quantity_value:"3" order)
    |> ok
  in
  Alcotest.(check bool)
    "fractional entitlement rejected" true
    (Result.is_error
       (T.Account.apply_distribution account ~source_instrument_id:source.id
          ~destination_instrument_id:child.id
          ~destination_lot_size:child.lot_size ~numerator:1L ~denominator:2L
          ~basis_allocation_bps:2000
          ~fractional_policy:T.Corporate_action.Reject_fractional))

let lifecycle_preserves_identity_and_terminal_state () =
  let configured = instrument ~id:"stable-id" ~symbol:"OLD" () in
  let state = T.Instrument_lifecycle.create [ configured ] |> ok in
  let event name kind =
    T.Instrument_lifecycle.create_event ~id:(action_id name)
      ~instrument_id:configured.id ~kind
    |> ok
  in
  let state =
    T.Instrument_lifecycle.apply state
      (event "rename"
         (T.Instrument_lifecycle.Identifier_change
            {
              symbol = "NEW";
              provider = "sip";
              provider_instrument_id = "NEW.X";
            }))
    |> ok
  in
  let listing =
    T.Instrument_lifecycle.listing state configured.id |> Option.get
  in
  Alcotest.(check string)
    "stable identity" "stable-id"
    (T.Id.Instrument.to_string listing.instrument_id);
  Alcotest.(check string) "new symbol" "NEW" listing.symbol;
  Alcotest.(check (list (pair string string)))
    "provider provenance"
    [ ("sip", "NEW.X") ]
    listing.provider_mappings;
  let state =
    T.Instrument_lifecycle.apply state
      (event "halt" (T.Instrument_lifecycle.Halt { reason = "volatility" }))
    |> ok
  in
  Alcotest.(check bool)
    "halt is not tradable" false
    (T.Instrument_lifecycle.is_tradable state configured.id);
  let state =
    T.Instrument_lifecycle.apply state
      (event "resume" T.Instrument_lifecycle.Resume)
    |> ok
  in
  Alcotest.(check bool)
    "resume is tradable" true
    (T.Instrument_lifecycle.is_tradable state configured.id);
  let state =
    T.Instrument_lifecycle.apply state
      (event "expire"
         (T.Instrument_lifecycle.Expiration
            { terminal_policy = T.Instrument_lifecycle.Hold }))
    |> ok
  in
  Alcotest.(check bool)
    "expiration is terminal" false
    (T.Instrument_lifecycle.is_tradable state configured.id);
  Alcotest.(check bool)
    "terminal event rejects resume" true
    (Result.is_error
       (T.Instrument_lifecycle.apply state
          (event "late-resume" T.Instrument_lifecycle.Resume)))

let constructors_reject_ambiguous_policies () =
  let id = action_id "distribution" in
  let source = instrument_id "source" in
  let child = instrument_id "child" in
  Alcotest.(check bool)
    "stock destination must be source" true
    (Result.is_error
       (T.Corporate_action.distribution ~id ~instrument_id:source
          ~distribution_type:T.Corporate_action.Stock_dividend
          ~destination_instrument_id:child ~numerator:1L ~denominator:10L
          ~basis_allocation_bps:0
          ~fractional_policy:T.Corporate_action.Reject_fractional));
  Alcotest.(check bool)
    "basis allocation bounded" true
    (Result.is_error
       (T.Corporate_action.distribution ~id ~instrument_id:source
          ~distribution_type:T.Corporate_action.Spin_off
          ~destination_instrument_id:child ~numerator:1L ~denominator:10L
          ~basis_allocation_bps:10_001
          ~fractional_policy:T.Corporate_action.Reject_fractional))

let policy_and_transition_boundaries () =
  let id = action_id "boundary-event" in
  let source = instrument_id "source" in
  let child = instrument_id "child" in
  let distribution ?(distribution_type = T.Corporate_action.Spin_off)
      ?(destination = child) ?(numerator = 1L) ?(denominator = 2L)
      ?(basis = 1000)
      ?(fractional_policy = T.Corporate_action.Reject_fractional) () =
    T.Corporate_action.distribution ~id ~instrument_id:source ~distribution_type
      ~destination_instrument_id:destination ~numerator ~denominator
      ~basis_allocation_bps:basis ~fractional_policy
  in
  List.iter
    (fun (name, result) ->
      Alcotest.(check bool) name true (Result.is_error result))
    [
      ("zero numerator", distribution ~numerator:0L ());
      ("zero denominator", distribution ~denominator:0L ());
      ("negative basis", distribution ~basis:(-1) ());
      ( "stock basis must be zero",
        distribution ~distribution_type:T.Corporate_action.Stock_dividend
          ~destination:source () );
      ( "stock ratio overflow",
        distribution ~distribution_type:T.Corporate_action.Stock_dividend
          ~destination:source ~basis:0 ~numerator:Int64.max_int () );
      ("spin-off destination differs", distribution ~destination:source ());
      ( "cash currency label",
        distribution
          ~fractional_policy:
            (T.Corporate_action.Cash_in_lieu
               { price = price "1"; currency = "bad currency" })
          () );
    ];
  Alcotest.(check (list string))
    "distribution labels"
    [ "stock_dividend"; "rights"; "spin_off" ]
    (List.map T.Corporate_action.distribution_type_to_string
       [
         T.Corporate_action.Stock_dividend;
         T.Corporate_action.Rights;
         T.Corporate_action.Spin_off;
       ]);
  Alcotest.(check string)
    "distribution formatting" "boundary-event spin_off 1:2 source"
    (Format.asprintf "%a" T.Corporate_action.pp (distribution () |> ok));
  let configured = instrument ~id:"stable" ~symbol:"OLD" () in
  Alcotest.(check bool)
    "duplicate lifecycle catalog" true
    (Result.is_error (T.Instrument_lifecycle.create [ configured; configured ]));
  let state = T.Instrument_lifecycle.create [ configured ] |> ok in
  Alcotest.(check bool)
    "unknown instrument is not tradable" false
    (T.Instrument_lifecycle.is_tradable state (instrument_id "unknown"));
  let create ?(instrument_id = configured.id) name kind =
    T.Instrument_lifecycle.create_event ~id:(action_id name) ~instrument_id
      ~kind
  in
  List.iter
    (fun (name, result) ->
      Alcotest.(check bool) name true (Result.is_error result))
    [
      ( "invalid halt reason",
        create "bad-halt" (T.Instrument_lifecycle.Halt { reason = "" }) );
      ( "invalid delisting reason",
        create "bad-delist"
          (T.Instrument_lifecycle.Delisting
             {
               terminal_policy = T.Instrument_lifecycle.Hold;
               reason = "bad reason";
             }) );
      ( "invalid identifier mapping",
        create "bad-id"
          (T.Instrument_lifecycle.Identifier_change
             { symbol = ""; provider = "sip"; provider_instrument_id = "x" }) );
      ( "invalid terminal currency",
        create "bad-terminal"
          (T.Instrument_lifecycle.Expiration
             {
               terminal_policy =
                 T.Instrument_lifecycle.Cash_out
                   { price = price "1"; currency = "" };
             }) );
    ];
  let unknown =
    create ~instrument_id:(instrument_id "unknown") "unknown-event"
      (T.Instrument_lifecycle.Halt { reason = "halt" })
    |> ok
  in
  Alcotest.(check bool)
    "unknown lifecycle instrument" true
    (Result.is_error (T.Instrument_lifecycle.apply state unknown));
  let resume = create "early-resume" T.Instrument_lifecycle.Resume |> ok in
  Alcotest.(check bool)
    "tradable cannot resume" true
    (Result.is_error (T.Instrument_lifecycle.apply state resume));
  let halt =
    create "first-halt" (T.Instrument_lifecycle.Halt { reason = "halt" }) |> ok
  in
  let halted = T.Instrument_lifecycle.apply state halt |> ok in
  let second_halt =
    create "second-halt" (T.Instrument_lifecycle.Halt { reason = "halt" }) |> ok
  in
  Alcotest.(check bool)
    "halted cannot halt" true
    (Result.is_error (T.Instrument_lifecycle.apply halted second_halt));
  Alcotest.(check (list string))
    "status labels"
    [ "tradable"; "halted"; "expired"; "delisted" ]
    (List.map T.Instrument_lifecycle.status_to_string
       [
         T.Instrument_lifecycle.Tradable;
         T.Instrument_lifecycle.Halted;
         T.Instrument_lifecycle.Expired;
         T.Instrument_lifecycle.Delisted;
       ]);
  let kinds =
    [
      T.Instrument_lifecycle.Halt { reason = "halt" };
      T.Instrument_lifecycle.Resume;
      T.Instrument_lifecycle.Identifier_change
        { symbol = "NEW"; provider = "sip"; provider_instrument_id = "NEW.X" };
      T.Instrument_lifecycle.Expiration
        { terminal_policy = T.Instrument_lifecycle.Hold };
      T.Instrument_lifecycle.Delisting
        {
          terminal_policy = T.Instrument_lifecycle.Hold;
          reason = "acquisition";
        };
    ]
  in
  Alcotest.(check (list string))
    "kind labels"
    [ "halt"; "resume"; "identifier_change"; "expiration"; "delisting" ]
    (List.map T.Instrument_lifecycle.kind_to_string kinds);
  let renamed =
    T.Instrument_lifecycle.apply state
      (create "provider-b"
         (T.Instrument_lifecycle.Identifier_change
            { symbol = "NEW"; provider = "b"; provider_instrument_id = "2" })
      |> ok)
    |> ok
  in
  let renamed =
    T.Instrument_lifecycle.apply renamed
      (create "provider-a"
         (T.Instrument_lifecycle.Identifier_change
            { symbol = "NEW"; provider = "a"; provider_instrument_id = "1" })
      |> ok)
    |> ok
  in
  let delisting =
    create "valid-delisting"
      (T.Instrument_lifecycle.Delisting
         {
           terminal_policy = T.Instrument_lifecycle.Hold;
           reason = "acquisition";
         })
    |> ok
  in
  let delisted = T.Instrument_lifecycle.apply renamed delisting |> ok in
  Alcotest.(check bool)
    "delisted is terminal" true
    (Result.is_error (T.Instrument_lifecycle.apply delisted resume));
  let expiration =
    create "halted-expiration"
      (T.Instrument_lifecycle.Expiration
         { terminal_policy = T.Instrument_lifecycle.Hold })
    |> ok
  in
  ignore (T.Instrument_lifecycle.apply halted expiration |> ok)

let lifecycle_slice ?(corporate_actions = []) ?(lifecycle_events = []) sequence
    =
  let date = Int64.to_int sequence + 1 in
  T.Market_slice.create ~slice_sequence:sequence
    ~start_at:(timestamp (Printf.sprintf "2026-03-%02dT14:30:00Z" date))
    ~end_at:(timestamp (Printf.sprintf "2026-03-%02dT21:00:00Z" date))
    ~available_at:(timestamp (Printf.sprintf "2026-03-%02dT21:00:01Z" date))
    ~received_at:(timestamp (Printf.sprintf "2026-03-%02dT21:00:02Z" date))
    ~bars:[ bar sequence ]
    ~fx_rates:[ fx_mark () ]
    ~corporate_actions ~borrow_observations:[] ~cash_rate_observations:[]
    ~settlement_failures:[] ~lifecycle_events ~market_events:[]
    ~order_book_events:[]
  |> ok

let lifecycle_runner schedule run =
  let config = engine_config ~contract_version:"1" () in
  let strategy_state = T.Scripted_strategy.create schedule |> ok in
  Runner.create ~run_id:(run_id run) ~scenario_sha256 ~config
    ~initial_portfolio:(initial_portfolio ~cash:[ ("USD", money "10000") ] ())
    ~strategy_state
  |> ok

let halt_cancels_orders_and_rejects_new_exposure () =
  let working =
    request ~kind:(T.Order.Limit (price "50")) ~quantity_value:"2" ()
  in
  let schedule =
    [
      (1L, [ T.Strategy.Submit_order working ]);
      (2L, [ T.Strategy.Submit_order working ]);
    ]
  in
  let state, _ =
    Runner.process_slice
      (lifecycle_runner schedule "halt-run")
      (lifecycle_slice 1L)
    |> ok
  in
  let halt =
    T.Instrument_lifecycle.create_event ~id:(action_id "halt-event")
      ~instrument_id:(instrument_id "test-equity")
      ~kind:(T.Instrument_lifecycle.Halt { reason = "regulatory" })
    |> ok
  in
  let state, events =
    Runner.process_slice state (lifecycle_slice ~lifecycle_events:[ halt ] 2L)
    |> ok
  in
  Alcotest.(check bool)
    "halt audit" true
    (List.exists
       (fun (audit : T.Audit.t) ->
         match audit.event with
         | T.Audit.Lifecycle_applied _ -> true
         | _ -> false)
       events);
  Alcotest.(check bool)
    "working order cancelled" true
    (List.exists
       (fun (audit : T.Audit.t) ->
         match audit.event with
         | T.Audit.Order_cancelled { reason = T.Audit.Instrument_halt; _ } ->
             true
         | _ -> false)
       events);
  Alcotest.(check bool)
    "same-slice new order rejected" true
    (List.exists
       (fun (audit : T.Audit.t) ->
         match audit.event with
         | T.Audit.Order_rejected order ->
             order.status = T.Order.Rejected "instrument is not tradable"
         | _ -> false)
       events);
  Alcotest.(check int)
    "no active orders" 0
    (List.length (T.Oms.active_orders (Runner.oms state)))

let terminal_cash_out_is_auditable () =
  let target =
    T.Strategy.Target_quantities
      [
        { instrument_id = instrument_id "test-equity"; quantity = quantity "3" };
      ]
  in
  let state = lifecycle_runner [ (1L, [ target ]) ] "terminal-run" in
  let state, _ = Runner.process_slice state (lifecycle_slice 1L) |> ok in
  let state, _ = Runner.process_slice state (lifecycle_slice 2L) |> ok in
  let expiration =
    T.Instrument_lifecycle.create_event
      ~id:(action_id "expiration-event")
      ~instrument_id:(instrument_id "test-equity")
      ~kind:
        (T.Instrument_lifecycle.Expiration
           {
             terminal_policy =
               T.Instrument_lifecycle.Cash_out
                 { price = price "90"; currency = "USD" };
           })
    |> ok
  in
  let state, events =
    Runner.process_slice state
      (lifecycle_slice ~lifecycle_events:[ expiration ] 3L)
    |> ok
  in
  Alcotest.check quantity_testable "terminal position cleared" (quantity "0")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check bool)
    "terminal attribution" true
    (List.exists
       (fun (audit : T.Audit.t) ->
         match audit.event with
         | T.Audit.Lifecycle_applied
             { liquidated_quantity; cash_amount; listing; _ } ->
             T.Scalar.Quantity.equal liquidated_quantity (quantity "3")
             && T.Scalar.Money.equal cash_amount (money "270")
             && listing.status = T.Instrument_lifecycle.Expired
         | _ -> false)
       events)

let stock_dividend_adjusts_account_and_target () =
  let target =
    T.Strategy.Target_quantities
      [
        { instrument_id = instrument_id "test-equity"; quantity = quantity "3" };
      ]
  in
  let state = lifecycle_runner [ (1L, [ target ]) ] "stock-dividend-run" in
  let state, _ = Runner.process_slice state (lifecycle_slice 1L) |> ok in
  let state, _ = Runner.process_slice state (lifecycle_slice 2L) |> ok in
  let action =
    T.Corporate_action.distribution
      ~id:(action_id "stock-dividend")
      ~instrument_id:(instrument_id "test-equity")
      ~distribution_type:T.Corporate_action.Stock_dividend
      ~destination_instrument_id:(instrument_id "test-equity")
      ~numerator:1L ~denominator:2L ~basis_allocation_bps:0
      ~fractional_policy:
        (T.Corporate_action.Cash_in_lieu
           { price = price "20"; currency = "USD" })
    |> ok
  in
  let state, events =
    Runner.process_slice state
      (lifecycle_slice ~corporate_actions:[ action ] 3L)
    |> ok
  in
  Alcotest.check quantity_testable "lot-aligned stock entitlement"
    (quantity "4")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check bool)
    "distribution attribution" true
    (List.exists
       (fun (audit : T.Audit.t) ->
         match audit.event with
         | T.Audit.Distribution_applied { result; _ } ->
             T.Scalar.Quantity.equal result.destination_quantity (quantity "1")
             && T.Scalar.Quantity.equal result.fractional_quantity
                  (quantity "0.5")
             && T.Scalar.Money.equal result.cash_in_lieu (money "10")
         | _ -> false)
       events);
  Alcotest.(check bool)
    "fraction does not create a target order" true
    (T.Oms.active_orders (Runner.oms state) = [])

let tests =
  [
    Alcotest.test_case "distribution basis and fractional cash" `Quick
      distribution_allocates_basis_and_fractional_cash;
    Alcotest.test_case "fractional policy is explicit" `Quick
      fractional_policy_is_explicit;
    Alcotest.test_case "lifecycle identity and terminal state" `Quick
      lifecycle_preserves_identity_and_terminal_state;
    Alcotest.test_case "constructors reject ambiguous policies" `Quick
      constructors_reject_ambiguous_policies;
    Alcotest.test_case "policy and transition boundaries" `Quick
      policy_and_transition_boundaries;
    Alcotest.test_case "halt cancels and rejects exposure" `Quick
      halt_cancels_orders_and_rejects_new_exposure;
    Alcotest.test_case "terminal cash-out is auditable" `Quick
      terminal_cash_out_is_auditable;
    Alcotest.test_case "stock dividend adjusts account and target" `Quick
      stock_dividend_adjusts_account_and_target;
  ]
