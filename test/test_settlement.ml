open Test_support
module T = Trading_engine
module Runner = T.Engine.Make (T.Scripted_strategy)

let settlement_policy ?(cash_buying_power = T.Settlement.Total_cash)
    ?(position_availability = T.Settlement.Total_positions) ?(lag = 1) () =
  let calendar =
    T.Settlement.calendar ~calendar_id:"test-settlement" ~version:"1"
      ~business_dates:[ "2026-01-02"; "2026-01-03"; "2026-01-04"; "2026-01-05" ]
    |> ok
  in
  let rule =
    T.Settlement.rule
      ~instrument_id:(instrument_id "test-equity")
      ~calendar_id:"test-settlement" ~lag_business_days:lag
    |> ok
  in
  T.Settlement.policy ~cash_buying_power ~position_availability
    ~calendars:[ calendar ] ~rules:[ rule ]
  |> ok

let buy_fill () =
  let order = request ~quantity_value:"2" () |> accepted_order in
  fill ~quantity_value:"2" ~fee_value:"1"
    ~executed_at:(timestamp "2026-01-03T21:00:00Z")
    order

let calendar_and_trade_date_accounting () =
  let policy = settlement_policy () in
  let fill = buy_fill () in
  let instruction = T.Settlement.instruction policy fill |> ok in
  Alcotest.(check string) "trade date" "2026-01-03" instruction.trade_date;
  Alcotest.(check string) "due date" "2026-01-04" instruction.due_date;
  Alcotest.check money_testable "cash movement" (money "-201")
    instruction.cash_movement;
  Alcotest.check quantity_testable "position movement" (quantity "2")
    instruction.position_movement;
  let account = test_account ~initial_cash:[ ("USD", money "1000") ] () in
  let account = T.Account.apply_unsettled_fill account fill |> ok in
  Alcotest.check money_testable "economic cash" (money "799")
    (account_cash account);
  Alcotest.check money_testable "settled cash unchanged" (money "1000")
    (T.Account.settled_cash account "USD" |> Option.get);
  Alcotest.check quantity_testable "economic position" (quantity "2")
    (T.Account.position_quantity account (instrument_id "test-equity"));
  Alcotest.check quantity_testable "settled position unchanged" (quantity "0")
    (T.Account.settled_position_quantity account (instrument_id "test-equity"));
  let valuation =
    account_value account ~marks:[ (instrument_id "test-equity", price "100") ]
  in
  Alcotest.check money_testable "unsettled cash valuation" (money "-201")
    valuation.unsettled_cash;
  Alcotest.check quantity_testable "unsettled position valuation" (quantity "2")
    (List.hd valuation.positions).unsettled_quantity;
  let account = T.Account.apply_settlement account instruction |> ok in
  Alcotest.check money_testable "settled cash reconciled" (money "799")
    (T.Account.settled_cash account "USD" |> Option.get);
  Alcotest.check quantity_testable "settled position reconciled" (quantity "2")
    (T.Account.settled_position_quantity account (instrument_id "test-equity"))

let slice ?(settlement_failures = []) sequence =
  let date = match sequence with 1L -> "02" | 2L -> "03" | _ -> "04" in
  T.Market_slice.create ~slice_sequence:sequence
    ~start_at:(timestamp ("2026-01-" ^ date ^ "T14:30:00Z"))
    ~end_at:(timestamp ("2026-01-" ^ date ^ "T21:00:00Z"))
    ~available_at:(timestamp ("2026-01-" ^ date ^ "T21:00:01Z"))
    ~received_at:(timestamp ("2026-01-" ^ date ^ "T21:00:02Z"))
    ~bars:[ bar sequence ]
    ~fx_rates:[ fx_mark () ]
    ~corporate_actions:[] ~borrow_observations:[] ~cash_rate_observations:[]
    ~settlement_failures ~lifecycle_events:[] ~market_events:[]
    ~order_book_events:[]
  |> ok

let runner ?(initial_cash = "1000") ?schedule policy run =
  let config =
    T.Engine.config ~contract_version:"1" ~risk:(risk ()) ~venue_calendars:[]
      ~execution_model:(T.Execution_model.find "completed_bar_v1" |> ok)
      ~execution:(execution ()) ~financing:(financing_policy ())
      ~settlement:policy ~max_internal_events:1000
    |> ok
  in
  let schedule =
    Option.value schedule
      ~default:
        [
          ( 1L,
            [
              T.Strategy.Target_quantities
                [
                  {
                    instrument_id = instrument_id "test-equity";
                    quantity = quantity "2";
                  };
                ];
            ] );
        ]
  in
  let strategy_state = T.Scripted_strategy.create schedule |> ok in
  Runner.create ~run_id:(run_id run) ~scenario_sha256 ~config
    ~initial_portfolio:
      (initial_portfolio ~cash:[ ("USD", money initial_cash) ] ())
    ~strategy_state
  |> ok

let find_instruction events =
  List.find_map
    (fun (event : T.Audit.t) ->
      match event.event with
      | T.Audit.Settlement_instruction_created instruction -> Some instruction
      | _ -> None)
    events
  |> Option.get

let engine_settles_due_instruction () =
  let state, _ =
    Runner.process_slice (runner (settlement_policy ()) "settle") (slice 1L)
    |> ok
  in
  let state, events = Runner.process_slice state (slice 2L) |> ok in
  let instruction = find_instruction events in
  Alcotest.(check string)
    "pending instruction" "pending"
    (T.Settlement.status_to_string instruction.status);
  Alcotest.check quantity_testable "trade-date position" (quantity "2")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.check quantity_testable "not yet settled" (quantity "0")
    (T.Account.settled_position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  let state, events = Runner.process_slice state (slice 3L) |> ok in
  Alcotest.(check bool)
    "completion event" true
    (List.exists
       (fun (event : T.Audit.t) ->
         match event.event with
         | T.Audit.Settlement_completed _ -> true
         | _ -> false)
       events);
  Alcotest.check quantity_testable "settled position" (quantity "2")
    (T.Account.settled_position_quantity (Runner.account state)
       (instrument_id "test-equity"))

let engine_records_settlement_failure () =
  let state, _ =
    Runner.process_slice (runner (settlement_policy ()) "failure") (slice 1L)
    |> ok
  in
  let state, events = Runner.process_slice state (slice 2L) |> ok in
  let instruction = find_instruction events in
  let failure =
    T.Settlement.failure ~instruction_id:instruction.instruction_id
      ~reason:"counterparty default"
    |> ok
  in
  let state, events =
    match
      Runner.process_slice state (slice ~settlement_failures:[ failure ] 3L)
    with
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check bool)
    "failure event" true
    (List.exists
       (fun (event : T.Audit.t) ->
         match event.event with
         | T.Audit.Settlement_failed _ -> true
         | _ -> false)
       events);
  Alcotest.check quantity_testable "failed position remains unsettled"
    (quantity "0")
    (T.Account.settled_position_quantity (Runner.account state)
       (instrument_id "test-equity"))

let constructors_reject_ambiguous_inputs () =
  let invalid_calendar ?(id = "calendar") ?(version = "1") dates =
    T.Settlement.calendar ~calendar_id:id ~version ~business_dates:dates
  in
  List.iter
    (fun result ->
      Alcotest.(check bool) "invalid calendar" true (Result.is_error result))
    [
      invalid_calendar ~id:"" [ "2026-01-02" ];
      invalid_calendar ~version:"2" [ "2026-01-02" ];
      invalid_calendar [];
      invalid_calendar [ "2026/01/02" ];
      invalid_calendar [ "2026-01-03"; "2026-01-02" ];
    ];
  let id = instrument_id "test-equity" in
  Alcotest.(check bool)
    "empty rule calendar" true
    (Result.is_error
       (T.Settlement.rule ~instrument_id:id ~calendar_id:"" ~lag_business_days:1));
  Alcotest.(check bool)
    "invalid lag" true
    (Result.is_error
       (T.Settlement.rule ~instrument_id:id ~calendar_id:"calendar"
          ~lag_business_days:31));
  Alcotest.(check bool)
    "negative lag" true
    (Result.is_error
       (T.Settlement.rule ~instrument_id:id ~calendar_id:"calendar"
          ~lag_business_days:(-1)));
  let calendar = invalid_calendar [ "2026-01-03" ] |> ok in
  let rule =
    T.Settlement.rule ~instrument_id:id ~calendar_id:"calendar"
      ~lag_business_days:1
    |> ok
  in
  let make_policy calendars rules =
    T.Settlement.policy ~cash_buying_power:T.Settlement.Total_cash
      ~position_availability:T.Settlement.Total_positions ~calendars ~rules
  in
  List.iter
    (fun result ->
      Alcotest.(check bool) "invalid policy" true (Result.is_error result))
    [
      make_policy [] [ rule ];
      make_policy [ calendar ] [];
      make_policy [ calendar; calendar ] [ rule ];
      make_policy [ calendar ] [ rule; rule ];
      make_policy [ calendar ]
        [
          T.Settlement.rule ~instrument_id:id ~calendar_id:"unknown"
            ~lag_business_days:1
          |> ok;
        ];
    ];
  let policy = make_policy [ calendar ] [ rule ] |> ok in
  Alcotest.(check bool)
    "calendar misses due date" true
    (Result.is_error (T.Settlement.instruction policy (buy_fill ())));
  let absent_trade_calendar =
    invalid_calendar [ "2026-01-02"; "2026-01-04" ] |> ok
  in
  let absent_trade_policy =
    make_policy [ absent_trade_calendar ] [ rule ] |> ok
  in
  Alcotest.(check bool)
    "calendar misses trade date" true
    (Result.is_error
       (T.Settlement.instruction absent_trade_policy (buy_fill ())));
  let other_order =
    request ~instrument:(instrument_id "other") () |> accepted_order
  in
  Alcotest.(check bool)
    "missing instrument rule" true
    (Result.is_error (T.Settlement.instruction policy (fill other_order)));
  Alcotest.(check bool)
    "failure instruction required" true
    (Result.is_error (T.Settlement.failure ~instruction_id:"" ~reason:"reason"));
  Alcotest.(check bool)
    "failure reason trimmed" true
    (Result.is_error
       (T.Settlement.failure ~instruction_id:"instruction" ~reason:" bad "));
  let instruction =
    T.Settlement.instruction (settlement_policy ()) (buy_fill ()) |> ok
  in
  let settled =
    T.Settlement.settle instruction
      ~settled_at:(timestamp "2026-01-04T21:00:00Z")
    |> ok
  in
  Alcotest.(check string)
    "settled status" "settled"
    (T.Settlement.status_to_string settled.status);
  Alcotest.(check bool)
    "settled instruction terminal" true
    (Result.is_error
       (T.Settlement.settle settled
          ~settled_at:(timestamp "2026-01-05T21:00:00Z")));
  let failed =
    T.Settlement.fail instruction
      ~failed_at:(timestamp "2026-01-04T21:00:00Z")
      ~reason:"default"
    |> ok
  in
  Alcotest.(check string)
    "failed status" "failed"
    (T.Settlement.status_to_string failed.status);
  Alcotest.(check bool)
    "failed instruction terminal" true
    (Result.is_error
       (T.Settlement.fail failed
          ~failed_at:(timestamp "2026-01-05T21:00:00Z")
          ~reason:"again"));
  Alcotest.(check bool)
    "failed instruction cannot settle" true
    (Result.is_error
       (T.Settlement.settle failed
          ~settled_at:(timestamp "2026-01-05T21:00:00Z")));
  Alcotest.(check bool)
    "settled instruction cannot fail" true
    (Result.is_error
       (T.Settlement.fail settled
          ~failed_at:(timestamp "2026-01-05T21:00:00Z")
          ~reason:"again"));
  Alcotest.(check string)
    "total buying power name" "total_cash"
    (T.Settlement.cash_buying_power_to_string T.Settlement.Total_cash);
  Alcotest.(check string)
    "settled buying power name" "settled_cash"
    (T.Settlement.cash_buying_power_to_string T.Settlement.Settled_cash);
  Alcotest.(check string)
    "total position name" "total_positions"
    (T.Settlement.position_availability_to_string T.Settlement.Total_positions);
  Alcotest.(check string)
    "settled position name" "settled_positions"
    (T.Settlement.position_availability_to_string T.Settlement.Settled_positions)

let settlement_limits_are_explicit () =
  let unknown_failure =
    T.Settlement.failure ~instruction_id:"unknown-settlement" ~reason:"default"
    |> ok
  in
  let state = runner (settlement_policy ()) "unknown-failure" in
  let state, _ = Runner.process_slice state (slice 1L) |> ok in
  Alcotest.(check bool)
    "unknown settlement failure rejected" true
    (Result.is_error
       (Runner.process_slice state
          (slice ~settlement_failures:[ unknown_failure ] 2L)));
  let state = runner ~initial_cash:"150" (settlement_policy ()) "cash-limit" in
  let state, _ = Runner.process_slice state (slice 1L) |> ok in
  let _, events = Runner.process_slice state (slice 2L) |> ok in
  Alcotest.(check bool)
    "cash buying-power limit" true
    (List.exists
       (fun (event : T.Audit.t) ->
         match event.event with
         | T.Audit.Fill_clipped
             { limit = T.Risk.Settlement_cash_buying_power _; _ } ->
             true
         | _ -> false)
       events);
  let target value =
    T.Strategy.Target_quantities
      [
        {
          instrument_id = instrument_id "test-equity";
          quantity = quantity value;
        };
      ]
  in
  let schedule = [ (1L, [ target "2" ]); (2L, [ target "0" ]) ] in
  let policy =
    settlement_policy ~position_availability:T.Settlement.Settled_positions
      ~lag:2 ()
  in
  let state = runner ~schedule policy "position-limit" in
  let state, _ = Runner.process_slice state (slice 1L) |> ok in
  let state, _ = Runner.process_slice state (slice 2L) |> ok in
  let _, events = Runner.process_slice state (slice 3L) |> ok in
  Alcotest.(check bool)
    "settled-position limit" true
    (List.exists
       (fun (event : T.Audit.t) ->
         match event.event with
         | T.Audit.Fill_clipped
             { limit = T.Risk.Settlement_position_availability _; _ } ->
             true
         | _ -> false)
       events)

let tests =
  [
    Alcotest.test_case "calendar and trade-date accounting" `Quick
      calendar_and_trade_date_accounting;
    Alcotest.test_case "engine settles due instruction" `Quick
      engine_settles_due_instruction;
    Alcotest.test_case "engine records settlement failure" `Quick
      engine_records_settlement_failure;
    Alcotest.test_case "constructors reject ambiguous inputs" `Quick
      constructors_reject_ambiguous_inputs;
    Alcotest.test_case "settlement limits are explicit" `Quick
      settlement_limits_are_explicit;
  ]
