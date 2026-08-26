open Test_support
module T = Trading_engine
module Runner = T.Engine.Make (T.Scripted_strategy)

let policy ?(day_count = T.Financing.Actual_360)
    ?(compounding = T.Financing.Simple)
    ?(borrow_missing_data = T.Financing.Reject)
    ?(cash_missing_data = T.Financing.Reject)
    ?(locate_policy = T.Financing.Clip_fill)
    ?(recall_policy = T.Financing.Close_out) () =
  T.Financing.policy ~day_count ~compounding ~borrow_missing_data
    ~cash_missing_data ~locate_policy ~recall_policy

let one_day =
  Ptime.diff
    (timestamp "2026-01-02T00:00:00Z")
    (timestamp "2026-01-01T00:00:00Z")

let explicit_accrual_policies () =
  let principal = money "36000" in
  let simple =
    T.Financing.accrue (policy ()) ~principal ~annual_rate_bps:10_000 one_day
    |> ok
  in
  Alcotest.check money_testable "actual/360 one-day interest" (money "100")
    simple;
  let two_days = Ptime.Span.add one_day one_day in
  let daily =
    T.Financing.accrue
      (policy ~compounding:T.Financing.Daily ())
      ~principal ~annual_rate_bps:10_000 two_days
    |> ok
  in
  Alcotest.check money_testable "daily capitalization" (money "200.277778")
    daily;
  let rebate =
    T.Financing.accrue (policy ()) ~principal ~annual_rate_bps:(-1000) one_day
    |> ok
  in
  Alcotest.check money_testable "negative rate" (money "-10") rebate

let accrual_boundaries_and_policy_names () =
  let actual_365 =
    T.Financing.accrue
      (policy ~day_count:T.Financing.Actual_365 ())
      ~principal:(money "36500") ~annual_rate_bps:10_000 one_day
    |> ok
  in
  Alcotest.check money_testable "actual/365 one-day interest" (money "100")
    actual_365;
  Alcotest.(check bool)
    "invalid accrual rate" true
    (Result.is_error
       (T.Financing.accrue (policy ()) ~principal:(money "1")
          ~annual_rate_bps:1_000_001 one_day));
  Alcotest.(check bool)
    "negative accrual interval" true
    (Result.is_error
       (T.Financing.accrue (policy ()) ~principal:(money "1")
          ~annual_rate_bps:100 (Ptime.Span.neg one_day)));
  let year =
    Ptime.diff
      (timestamp "2027-01-01T00:00:00Z")
      (timestamp "2026-01-01T00:00:00Z")
  in
  Alcotest.(check bool)
    "accrual overflow" true
    (Result.is_error
       (T.Financing.accrue
          (policy ~day_count:T.Financing.Actual_365 ())
          ~principal:(T.Scalar.Money.of_micros Int64.max_int)
          ~annual_rate_bps:1_000_000 year));
  Alcotest.(check (list string))
    "policy names"
    [
      "actual_365";
      "actual_360";
      "simple";
      "daily";
      "reject";
      "zero";
      "reject_order";
      "clip_fill";
      "reject_new_shorts";
      "close_out";
    ]
    [
      T.Financing.day_count_to_string T.Financing.Actual_365;
      T.Financing.day_count_to_string T.Financing.Actual_360;
      T.Financing.compounding_to_string T.Financing.Simple;
      T.Financing.compounding_to_string T.Financing.Daily;
      T.Financing.missing_data_to_string T.Financing.Reject;
      T.Financing.missing_data_to_string T.Financing.Zero;
      T.Financing.locate_policy_to_string T.Financing.Reject_order;
      T.Financing.locate_policy_to_string T.Financing.Clip_fill;
      T.Financing.recall_policy_to_string T.Financing.Reject_new_shorts;
      T.Financing.recall_policy_to_string T.Financing.Close_out;
    ]

let cash_interest_is_ledger_attributed () =
  let account =
    test_account ~initial_cash:[ ("USD", money "100"); ("EUR", money "0") ] ()
  in
  let account =
    T.Account.apply_cash_interest account ~currency:"USD" ~interest:(money "1")
    |> ok
  in
  let account =
    T.Account.apply_cash_interest account ~currency:"EUR"
      ~interest:(money "-0.5")
    |> ok
  in
  let valuation =
    account_value ~instruments:[]
      ~fx_rates:[ ("USD", price "1"); ("EUR", price "2") ]
      account ~marks:[]
  in
  Alcotest.check money_testable "base interest" (money "0")
    valuation.cash_interest;
  let eur =
    List.find
      (fun (row : T.Account.cash_attribution) ->
        String.equal row.currency "EUR")
      valuation.cash_balances
  in
  Alcotest.check money_testable "native debit interest" (money "-0.5")
    eur.interest;
  Alcotest.check money_testable "base debit interest" (money "-1")
    eur.base_interest;
  Alcotest.check money_testable "cash interest contributes to realized P&L"
    (money "0") valuation.realized_pnl;
  let negative_account = test_account ~initial_cash:[ ("USD", money "0") ] () in
  let negative_account =
    T.Account.apply_cash_interest negative_account ~currency:"USD"
      ~interest:(money "-1")
    |> ok
  in
  let negative_valuation =
    account_value ~instruments:[]
      ~fx_rates:[ ("USD", price "1") ]
      negative_account ~marks:[]
  in
  Alcotest.check money_testable "debit interest can produce negative equity"
    (money "-1") negative_valuation.equity;
  Alcotest.check money_testable "negative equity retains realized attribution"
    (money "-1") negative_valuation.realized_pnl

let financing_slice ?(borrow_observations = []) ?(cash_rate_observations = [])
    sequence =
  let day = day sequence in
  let start_at = timestamp (Printf.sprintf "2026-01-%02dT14:30:00Z" day) in
  let end_at = timestamp (Printf.sprintf "2026-01-%02dT21:00:00Z" day) in
  let available_at = timestamp (Printf.sprintf "2026-01-%02dT21:00:01Z" day) in
  let received_at = timestamp (Printf.sprintf "2026-01-%02dT21:00:02Z" day) in
  T.Market_slice.create ~slice_sequence:sequence ~start_at ~end_at ~available_at
    ~received_at
    ~bars:[ bar sequence ]
    ~fx_rates:[ fx_mark () ]
    ~corporate_actions:[] ~borrow_observations ~cash_rate_observations
    ~settlement_failures:[] ~lifecycle_events:[] ~market_events:[]
    ~order_book_events:[]
  |> ok

let borrow_observation ?(available = "5") ?(rate = 3600) ?(recalled = false)
    effective_at =
  T.Financing.borrow_observation
    ~instrument_id:(instrument_id "test-equity")
    ~effective_at ~available_quantity:(quantity available) ~annual_rate_bps:rate
    ~recalled
  |> ok

let cash_rate effective_at =
  T.Financing.cash_rate_observation ~currency:"USD" ~effective_at
    ~credit_rate_bps:0 ~debit_rate_bps:3600
  |> ok

let financing_config financing =
  T.Engine.config ~contract_version:T.Contract.version ~risk:(risk ())
    ~venue_calendars:[]
    ~execution_model:(T.Execution_model.find "completed_bar_v1" |> ok)
    ~execution:(execution ()) ~financing ~settlement:(settlement_policy ())
    ~max_internal_events:1000
  |> ok

let empty_strategy () = T.Scripted_strategy.create [] |> ok

let missing_data_policies_are_explicit () =
  let cash_state =
    Runner.create ~run_id:(run_id "missing-cash") ~scenario_sha256
      ~config:(financing_config (policy ()))
      ~initial_portfolio:(initial_portfolio ~cash:[ ("USD", money "1000") ] ())
      ~strategy_state:(empty_strategy ())
    |> ok
  in
  let cash_error =
    Runner.process_slice cash_state (financing_slice 1L) |> error
  in
  Alcotest.(check string)
    "missing cash rate rejected"
    "nonzero cash balance has no effective rate for currency USD" cash_error;
  let short =
    T.Initial_portfolio.position
      ~instrument_id:(instrument_id "test-equity")
      ~quantity:(quantity "-1") ~cost_basis:(money "-100")
      ~realized_pnl:T.Scalar.Money.zero ~dividend_pnl:T.Scalar.Money.zero
      ~execution_fees:T.Scalar.Money.zero ~borrow_fees:T.Scalar.Money.zero
    |> ok
  in
  let initial_portfolio =
    T.Initial_portfolio.create ~base_currency:"USD"
      ~cash:[ ("USD", money "1100") ]
      ~positions:[ short ]
      ~marks:[ (instrument_id "test-equity", price "100") ]
      ~fx_rates:[ ("USD", price "1") ]
    |> ok
  in
  let borrow_state =
    Runner.create ~run_id:(run_id "missing-borrow") ~scenario_sha256
      ~config:(financing_config (policy ~cash_missing_data:T.Financing.Zero ()))
      ~initial_portfolio ~strategy_state:(empty_strategy ())
    |> ok
  in
  let borrow_error =
    Runner.process_slice borrow_state (financing_slice 1L) |> error
  in
  Alcotest.(check string)
    "missing borrow observation rejected"
    "open short has no effective borrow observation for test-equity"
    borrow_error

let reject_order_policy_uses_current_locate () =
  let target =
    T.Strategy.Target_quantities
      [
        T.Strategy.
          {
            instrument_id = instrument_id "test-equity";
            quantity = quantity "-10";
          };
      ]
  in
  let strategy_state = T.Scripted_strategy.create [ (1L, [ target ]) ] |> ok in
  let financing =
    policy ~borrow_missing_data:T.Financing.Zero
      ~cash_missing_data:T.Financing.Zero
      ~locate_policy:T.Financing.Reject_order ()
  in
  let state =
    Runner.create ~run_id:(run_id "reject-locate") ~scenario_sha256
      ~config:(financing_config financing)
      ~initial_portfolio:(initial_portfolio ~cash:[ ("USD", money "1000") ] ())
      ~strategy_state
    |> ok
  in
  let start_at = timestamp "2026-01-02T14:30:00Z" in
  let state, events =
    Runner.process_slice state
      (financing_slice ~borrow_observations:[ borrow_observation start_at ] 1L)
    |> ok
  in
  Alcotest.check quantity_testable "rejected order leaves position flat"
    T.Scalar.Quantity.zero
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check bool)
    "oversized locate order rejected" true
    (List.exists
       (fun event ->
         match event.T.Audit.event with
         | T.Audit.Order_rejected { status = T.Order.Rejected reason; _ } ->
             String.equal reason "order exceeds effective borrow availability"
         | _ -> false)
       events)

let zero_missing_data_and_recall_retention () =
  let short =
    T.Initial_portfolio.position
      ~instrument_id:(instrument_id "test-equity")
      ~quantity:(quantity "-1") ~cost_basis:(money "-100")
      ~realized_pnl:T.Scalar.Money.zero ~dividend_pnl:T.Scalar.Money.zero
      ~execution_fees:T.Scalar.Money.zero ~borrow_fees:T.Scalar.Money.zero
    |> ok
  in
  let initial_portfolio =
    T.Initial_portfolio.create ~base_currency:"USD"
      ~cash:[ ("USD", money "1100") ]
      ~positions:[ short ]
      ~marks:[ (instrument_id "test-equity", price "100") ]
      ~fx_rates:[ ("USD", price "1") ]
    |> ok
  in
  let financing =
    policy ~borrow_missing_data:T.Financing.Zero
      ~cash_missing_data:T.Financing.Zero
      ~recall_policy:T.Financing.Reject_new_shorts ()
  in
  let state =
    Runner.create ~run_id:(run_id "retain-recall") ~scenario_sha256
      ~config:(financing_config financing)
      ~initial_portfolio ~strategy_state:(empty_strategy ())
    |> ok
  in
  let state, _ = Runner.process_slice state (financing_slice 1L) |> ok in
  let recall_at = timestamp "2026-01-03T14:30:00Z" in
  let cash_observation =
    T.Financing.cash_rate_observation ~currency:"USD" ~effective_at:recall_at
      ~credit_rate_bps:0 ~debit_rate_bps:0
    |> ok
  in
  let state, events =
    Runner.process_slice state
      (financing_slice
         ~borrow_observations:
           [
             borrow_observation ~available:"0" ~rate:(-100) ~recalled:true
               recall_at;
           ]
         ~cash_rate_observations:[ cash_observation ] 2L)
    |> ok
  in
  Alcotest.check quantity_testable "reject-new-shorts retains recalled position"
    (quantity "-1")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check bool)
    "retained recall audited without close-out" true
    (List.exists
       (fun event ->
         match event.T.Audit.event with
         | T.Audit.Borrow_recall_received { close_out_quantity; _ } ->
             T.Scalar.Quantity.is_zero close_out_quantity
         | _ -> false)
       events)

let availability_clips_and_recall_closes () =
  let target =
    T.Strategy.Target_quantities
      [
        T.Strategy.
          {
            instrument_id = instrument_id "test-equity";
            quantity = quantity "-10";
          };
      ]
  in
  let strategy_state = T.Scripted_strategy.create [ (1L, [ target ]) ] |> ok in
  let configured_risk = risk () in
  let financing = policy () in
  let config =
    T.Engine.config ~contract_version:T.Contract.version ~risk:configured_risk
      ~venue_calendars:[]
      ~execution_model:(T.Execution_model.find "completed_bar_v1" |> ok)
      ~execution:(execution ()) ~financing ~settlement:(settlement_policy ())
      ~max_internal_events:1000
    |> ok
  in
  let state =
    Runner.create ~run_id:(run_id "financing") ~scenario_sha256 ~config
      ~initial_portfolio:(initial_portfolio ~cash:[ ("USD", money "1000") ] ())
      ~strategy_state
    |> ok
  in
  let first_start = timestamp "2026-01-02T14:30:00Z" in
  let state, _ =
    Runner.process_slice state
      (financing_slice
         ~borrow_observations:[ borrow_observation first_start ]
         ~cash_rate_observations:[ cash_rate first_start ]
         1L)
    |> ok
  in
  let state, events = Runner.process_slice state (financing_slice 2L) |> ok in
  let encoded = List.map T.Codec.audit_to_string events in
  Alcotest.(check bool)
    "availability audits serialize" true
    (List.for_all (fun value -> String.length value > 0) encoded);
  Alcotest.check quantity_testable "locate-limited short" (quantity "-5")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check bool)
    "availability clipping audited" true
    (List.exists
       (fun event ->
         match event.T.Audit.event with
         | T.Audit.Fill_clipped
             { limit = T.Risk.Instrument_borrow_availability _; _ } ->
             true
         | _ -> false)
       events);
  let recall_start = timestamp "2026-01-04T14:30:00Z" in
  let recall = borrow_observation ~available:"0" ~recalled:true recall_start in
  let state, events =
    Runner.process_slice state
      (financing_slice ~borrow_observations:[ recall ] 3L)
    |> ok
  in
  let encoded = List.map T.Codec.audit_to_string events in
  Alcotest.(check bool)
    "recall and financing audits serialize" true
    (List.for_all (fun value -> String.length value > 0) encoded);
  Alcotest.check quantity_testable "recalled short closed"
    T.Scalar.Quantity.zero
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check bool)
    "recall and observed charge audited" true
    (List.exists
       (fun event ->
         match event.T.Audit.event with
         | T.Audit.Borrow_recall_received _ -> true
         | _ -> false)
       events
    && List.exists
         (fun event ->
           match event.T.Audit.event with
           | T.Audit.Borrow_charge_applied _ -> true
           | _ -> false)
         events)

let constructors_reject_ambiguous_observations () =
  let effective_at = timestamp "2026-01-01T00:00:00Z" in
  Alcotest.(check bool)
    "borrow rate bounds enforced" true
    (Result.is_error
       (T.Financing.borrow_observation
          ~instrument_id:(instrument_id "test-equity")
          ~effective_at ~available_quantity:(quantity "1")
          ~annual_rate_bps:(-1_000_001) ~recalled:false));
  Alcotest.(check bool)
    "recall cannot retain availability" true
    (Result.is_error
       (T.Financing.borrow_observation
          ~instrument_id:(instrument_id "test-equity")
          ~effective_at ~available_quantity:(quantity "1") ~annual_rate_bps:100
          ~recalled:true));
  Alcotest.(check bool)
    "rate bounds enforced" true
    (Result.is_error
       (T.Financing.cash_rate_observation ~currency:"USD" ~effective_at
          ~credit_rate_bps:1_000_001 ~debit_rate_bps:0));
  Alcotest.(check bool)
    "currency validation enforced" true
    (Result.is_error
       (T.Financing.cash_rate_observation ~currency:"" ~effective_at
          ~credit_rate_bps:0 ~debit_rate_bps:0))

let tests =
  [
    Alcotest.test_case "explicit accrual policies" `Quick
      explicit_accrual_policies;
    Alcotest.test_case "accrual boundaries and names" `Quick
      accrual_boundaries_and_policy_names;
    Alcotest.test_case "cash interest attribution" `Quick
      cash_interest_is_ledger_attributed;
    Alcotest.test_case "explicit missing data" `Quick
      missing_data_policies_are_explicit;
    Alcotest.test_case "locate order rejection" `Quick
      reject_order_policy_uses_current_locate;
    Alcotest.test_case "zero missing data and retained recall" `Quick
      zero_missing_data_and_recall_retention;
    Alcotest.test_case "availability and recall" `Quick
      availability_clips_and_recall_closes;
    Alcotest.test_case "observation validation" `Quick
      constructors_reject_ambiguous_observations;
  ]
