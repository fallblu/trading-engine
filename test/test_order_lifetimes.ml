open Test_support
module T = Trading_engine
module Runner = T.Engine.Make (T.Scripted_strategy)

let event_names events =
  List.map (fun event -> T.Audit.event_name event.T.Audit.event) events

let runner ?(initial_cash = "10000") ?(risk = risk ()) ?(venue_calendars = [])
    schedule =
  let strategy_state = T.Scripted_strategy.create schedule |> ok in
  Runner.create ~run_id:(run_id "lifetime-run") ~scenario_sha256
    ~config:(engine_config_v8 ~risk ~venue_calendars ())
    ~initial_cash:[ ("USD", money initial_cash) ]
    ~strategy_state
  |> ok

let calendar ?(venue = "XNAS") ?(covered_instrument = "test-equity") () =
  let phase =
    T.Venue_calendar.create_phase ~kind:T.Venue_calendar.Regular
      ~opens_at:(timestamp "2026-01-03T14:30:00Z")
      ~closes_at:(timestamp "2026-01-03T21:00:00Z")
    |> ok
  in
  let session =
    T.Venue_calendar.create_session ~session_date:"2026-01-03"
      ~kind:T.Venue_calendar.Regular_session ~phases:[ phase ]
    |> ok
  in
  T.Venue_calendar.create
    ~id:(T.Id.Venue_calendar.of_string_exn "xnas-test")
    ~version:"1"
    ~venue_id:(T.Id.Venue.of_string_exn venue)
    ~instrument_ids:[ instrument_id covered_instrument ]
    ~sessions:[ session ]
  |> ok

let compatibility_mapping () =
  let market = request () in
  let limit = request ~kind:(T.Order.Limit (price "100")) () in
  Alcotest.(check string)
    "legacy market is IOC" "ioc"
    (T.Order.time_in_force_to_string market.time_in_force);
  Alcotest.(check string)
    "legacy limit is GTC" "gtc"
    (T.Order.time_in_force_to_string limit.time_in_force)

let validates_stop_limit_and_gtd () =
  Alcotest.(check bool)
    "invalid buy stop-limit" true
    (Result.is_error
       (T.Order.request_v8
          ~instrument_id:(instrument_id "test-equity")
          ~side:T.Order.Buy ~quantity:(quantity "1")
          ~kind:
            (T.Order.Stop_limit
               { trigger_price = price "100"; limit_price = price "99" })
          ~time_in_force:T.Order.Gtc ~origin:T.Order.Direct));
  let request =
    request_v8
      ~time_in_force:(T.Order.Gtd (timestamp "2026-01-02T20:00:00Z"))
      ()
  in
  Alcotest.(check bool)
    "expiry follows creation" true
    (Result.is_error
       (T.Order.accept ~id:(order_id "expired")
          ~created_event_id:(event_id "expired-event") ~accepted_sequence:1L
          ~created_at:(timestamp "2026-01-02T21:00:00Z")
          ~eligible_after_slice_sequence:1L request))

let v8_intent_requires_explicit_companions () =
  let intent =
    `Assoc
      [
        ("type", `String "submit_order");
        ("instrument_id", `String "test-equity");
        ("side", `String "buy");
        ("quantity", `String "1");
        ("order_kind", `String "stop");
        ("trigger_price", `String "110");
        ("limit_price", `Null);
        ("time_in_force", `String "gtd");
        ("venue_id", `Null);
        ("calendar_id", `Null);
        ("expires_at", `String "2026-01-03T20:00:00Z");
      ]
  in
  Alcotest.(check bool)
    "explicit stop/GTD parses" true
    (Result.is_ok (T.Scenario.intent_of_yojson ~contract_version:"8" intent));
  let missing_trigger =
    match intent with
    | `Assoc fields -> `Assoc (List.remove_assoc "trigger_price" fields)
    | _ -> assert false
  in
  Alcotest.(check bool)
    "missing companion is rejected" true
    (Result.is_error
       (T.Scenario.intent_of_yojson ~contract_version:"8" missing_trigger));
  let submit kind trigger limit tif venue calendar expires =
    `Assoc
      [
        ("type", `String "submit_order");
        ("instrument_id", `String "test-equity");
        ("side", `String "buy");
        ("quantity", `String "1");
        ("order_kind", `String kind);
        ("trigger_price", trigger);
        ("limit_price", limit);
        ("time_in_force", `String tif);
        ("venue_id", venue);
        ("calendar_id", calendar);
        ("expires_at", expires);
      ]
  in
  let valid =
    [
      submit "market" `Null `Null "ioc" `Null `Null `Null;
      submit "limit" `Null (`String "100") "fok" `Null `Null `Null;
      submit "stop_limit" (`String "100") (`String "101") "day" (`String "XNAS")
        (`String "xnas-test") `Null;
      submit "limit" `Null (`String "100") "gtc" `Null `Null `Null;
    ]
  in
  List.iter
    (fun json ->
      Alcotest.(check bool)
        "v8 order variant parses" true
        (Result.is_ok (T.Scenario.intent_of_yojson ~contract_version:"8" json)))
    valid;
  Alcotest.(check bool)
    "inconsistent TIF companions rejected" true
    (Result.is_error
       (T.Scenario.intent_of_yojson ~contract_version:"8"
          (submit "market" `Null `Null "gtc" (`String "XNAS") `Null `Null)))

let order_validation_and_serialization_branches () =
  Alcotest.(check bool)
    "nonpositive quantity rejected" true
    (Result.is_error
       (T.Order.request_v8
          ~instrument_id:(instrument_id "test-equity")
          ~side:T.Order.Buy ~quantity:T.Scalar.Quantity.zero
          ~kind:T.Order.Market ~time_in_force:T.Order.Gtc ~origin:T.Order.Direct));
  Alcotest.(check bool)
    "invalid sell stop-limit rejected" true
    (Result.is_error
       (T.Order.request_v8
          ~instrument_id:(instrument_id "test-equity")
          ~side:T.Order.Sell ~quantity:(quantity "1")
          ~kind:
            (T.Order.Stop_limit
               { trigger_price = price "100"; limit_price = price "101" })
          ~time_in_force:T.Order.Gtc ~origin:T.Order.Direct));
  let ordinary = request_v8 () in
  Alcotest.(check bool)
    "negative accepted sequence rejected" true
    (Result.is_error
       (T.Order.accept
          ~id:(order_id "negative-sequence")
          ~created_event_id:(event_id "negative-sequence-event")
          ~accepted_sequence:(-1L)
          ~created_at:(timestamp "2026-01-02T21:00:00Z")
          ~eligible_after_slice_sequence:0L ordinary));
  Alcotest.(check bool)
    "negative eligibility rejected" true
    (Result.is_error
       (T.Order.accept
          ~id:(order_id "negative-eligibility")
          ~created_event_id:(event_id "negative-eligibility-event")
          ~accepted_sequence:1L
          ~created_at:(timestamp "2026-01-02T21:00:00Z")
          ~eligible_after_slice_sequence:(-1L) ordinary));
  Alcotest.(check bool)
    "empty rejection reason rejected" true
    (Result.is_error
       (T.Order.reject
          ~id:(order_id "empty-rejection")
          ~created_event_id:(event_id "empty-rejection-event")
          ~rejected_sequence:1L
          ~created_at:(timestamp "2026-01-02T21:00:00Z")
          ~eligible_after_slice_sequence:0L ordinary ~reason:""));
  let cases =
    [
      (T.Order.Market, T.Order.Ioc);
      (T.Order.Limit (price "100"), T.Order.Fok);
      (T.Order.Stop (price "110"), T.Order.Gtc);
      ( T.Order.Stop_limit
          { trigger_price = price "110"; limit_price = price "111" },
        T.Order.Day
          {
            venue_id = T.Id.Venue.of_string_exn "XNAS";
            calendar_id = T.Id.Venue_calendar.of_string_exn "xnas-test";
          } );
      ( T.Order.Limit (price "100"),
        T.Order.Gtd (timestamp "2026-01-03T20:00:00Z") );
    ]
  in
  List.iteri
    (fun index (kind, time_in_force) ->
      let request = request_v8 ~kind ~time_in_force () in
      ignore (T.Order.kind_to_string kind);
      ignore (T.Order.time_in_force_to_string time_in_force);
      let order =
        accepted_order ~id:(Printf.sprintf "serialized-%d" index) request
      in
      ignore (T.Order.is_market order);
      ignore (T.Order.effective_kind order);
      match T.Codec.order_to_yojson_v8 order with
      | `Assoc fields ->
          Alcotest.(check bool)
            "TIF serialized" true
            (List.mem_assoc "time_in_force" fields)
      | _ -> Alcotest.fail "serialized order must be an object")
    cases;
  let unconditional = accepted_order (request_v8 ()) in
  Alcotest.(check bool)
    "unconditional order cannot trigger" true
    (Result.is_error
       (T.Order.trigger unconditional
          ~updated_event_id:(event_id "invalid-trigger")
          ~triggered_at:(timestamp "2026-01-03T20:00:00Z")
          ~triggered_slice_sequence:2L));
  let conditional =
    accepted_order (request_v8 ~kind:(T.Order.Stop (price "110")) ())
  in
  Alcotest.(check bool)
    "negative trigger sequence rejected" true
    (Result.is_error
       (T.Order.trigger conditional
          ~updated_event_id:(event_id "invalid-sequence")
          ~triggered_at:(timestamp "2026-01-03T20:00:00Z")
          ~triggered_slice_sequence:(-1L)));
  let triggered =
    T.Order.trigger conditional ~updated_event_id:(event_id "valid-trigger")
      ~triggered_at:(timestamp "2026-01-03T20:00:00Z")
      ~triggered_slice_sequence:2L
    |> ok
  in
  ignore (T.Codec.order_to_yojson_v8 triggered);
  Alcotest.(check bool)
    "duplicate trigger rejected" true
    (Result.is_error
       (T.Order.trigger triggered
          ~updated_event_id:(event_id "duplicate-trigger")
          ~triggered_at:(timestamp "2026-01-03T20:00:00Z")
          ~triggered_slice_sequence:3L));
  let cancelled = T.Order.cancel conditional |> ok in
  Alcotest.(check bool)
    "cancelled order is terminal" true
    (T.Order.is_terminal cancelled);
  Alcotest.(check bool)
    "terminal trigger rejected" true
    (Result.is_error
       (T.Order.trigger cancelled
          ~updated_event_id:(event_id "terminal-trigger")
          ~triggered_at:(timestamp "2026-01-03T20:00:00Z")
          ~triggered_slice_sequence:2L))

let trigger_then_execute_on_following_slice () =
  let request =
    request_v8 ~kind:(T.Order.Stop (price "110")) ~time_in_force:T.Order.Gtc ()
  in
  let oms, order = oms_with_order request in
  let trigger_slice =
    market_slice
      ~bars:[ bar ~open_price:"100" ~high_price:"115" ~low_price:"95" 2L ]
      2L
  in
  let pure_match =
    T.Execution.match_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms trigger_slice
    |> ok
  in
  Alcotest.(check int)
    "pure match reports one trigger" 1
    (List.length pure_match.triggers);
  Alcotest.(check int)
    "trigger slice has no fill" 0
    (List.length pure_match.fills);
  let cursor =
    T.Execution.start_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms trigger_slice
    |> ok
  in
  let triggered_at, triggered_sequence =
    match T.Execution.next cursor ~oms |> ok with
    | T.Execution.Triggered (id, at, sequence, _) ->
        Alcotest.check order_id_testable "triggered order" order.id id;
        (at, sequence)
    | T.Execution.Finished _ | T.Execution.Proposed _ ->
        Alcotest.fail "expected a trigger"
  in
  Alcotest.(check string)
    "intrabar trigger is timestamped at bar end" "2026-01-03T21:00:00.000000Z"
    (T.Codec.ptime_to_string triggered_at);
  Alcotest.(check int64) "trigger slice persisted" 2L triggered_sequence;
  let oms, _ =
    T.Oms.trigger oms order.id ~updated_event_id:(event_id "trigger-event")
      ~triggered_at ~triggered_slice_sequence:triggered_sequence
    |> ok
  in
  let next =
    T.Execution.match_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms
      (market_slice ~bars:[ bar ~open_price:"112" 3L ] 3L)
    |> ok
  in
  match next.fills with
  | [ fill ] ->
      Alcotest.check price_testable "stop becomes next-slice market order"
        (price "112") fill.price
  | _ -> Alcotest.fail "expected one next-slice fill"

let stop_limit_uses_limit_after_trigger () =
  let request =
    request_v8
      ~kind:
        (T.Order.Stop_limit
           { trigger_price = price "110"; limit_price = price "111" })
      ()
  in
  let oms, order = oms_with_order request in
  let oms, _ =
    T.Oms.trigger oms order.id ~updated_event_id:(event_id "trigger-event")
      ~triggered_at:(timestamp "2026-01-03T21:00:00Z")
      ~triggered_slice_sequence:2L
    |> ok
  in
  let missed =
    T.Execution.match_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms
      (market_slice
         ~bars:
           [
             bar ~open_price:"115" ~high_price:"118" ~low_price:"112"
               ~close_price:"115" 3L;
           ]
         3L)
    |> ok
  in
  Alcotest.(check int)
    "activated limit can remain working" 0 (List.length missed.fills);
  let touched =
    T.Execution.match_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms
      (market_slice
         ~bars:
           [
             bar ~open_price:"115" ~high_price:"118" ~low_price:"110"
               ~close_price:"115" 4L;
           ]
         4L)
    |> ok
  in
  Alcotest.check price_testable "activated limit fills at its limit"
    (price "111") (List.hd touched.fills).price

let sell_stop_gap_and_partial_fill () =
  let request =
    request_v8 ~side:T.Order.Sell ~quantity_value:"10"
      ~kind:(T.Order.Stop (price "90"))
      ()
  in
  let oms, order = oms_with_order request in
  let gap_slice =
    market_slice
      ~bars:
        [
          bar ~open_price:"85" ~high_price:"90" ~low_price:"80"
            ~close_price:"85" 2L;
        ]
      2L
  in
  let matched =
    T.Execution.match_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms gap_slice
    |> ok
  in
  let triggered_at =
    match matched.triggers with
    | [ (id, triggered_at, 2L) ] ->
        Alcotest.check order_id_testable "sell stop ID" order.id id;
        triggered_at
    | _ -> Alcotest.fail "expected one sell-stop trigger"
  in
  Alcotest.(check string)
    "gap trigger uses bar start" "2026-01-03T14:30:00.000000Z"
    (T.Codec.ptime_to_string triggered_at);
  let oms, _ =
    T.Oms.trigger oms order.id ~updated_event_id:(event_id "sell-trigger")
      ~triggered_at ~triggered_slice_sequence:2L
    |> ok
  in
  let partial =
    T.Execution.match_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms
      (market_slice ~bars:[ bar ~volume:(Some "4") 3L ] 3L)
    |> ok
  in
  match partial.fills with
  | [ fill ] ->
      Alcotest.check quantity_testable "activated stop can partially fill"
        (quantity "4") fill.quantity
  | _ -> Alcotest.fail "expected one partial sell-stop fill"

let fok_is_all_or_cancel () =
  let request = request_v8 ~quantity_value:"10" ~time_in_force:T.Order.Fok () in
  let oms, order = oms_with_order request in
  let matched =
    T.Execution.match_slice (execution ())
      ~instruments:[ instrument () ]
      ~oms
      (market_slice ~bars:[ bar ~volume:(Some "5") 2L ] 2L)
    |> ok
  in
  Alcotest.(check int) "no partial FOK fill" 0 (List.length matched.fills);
  Alcotest.check order_id_testable "FOK is cancelled" order.id
    (List.hd matched.market_ioc_orders)

let split_adjusts_stop_prices () =
  let request =
    request_v8 ~quantity_value:"10"
      ~kind:
        (T.Order.Stop_limit
           { trigger_price = price "110"; limit_price = price "112" })
      ()
  in
  let order = accepted_order request in
  let adjusted =
    T.Order.adjust_for_split order ~updated_event_id:(event_id "split-event")
      ~numerator:2L ~denominator:1L
    |> ok
  in
  Alcotest.check quantity_testable "quantity doubles" (quantity "20")
    adjusted.request.quantity;
  match adjusted.request.kind with
  | T.Order.Stop_limit { trigger_price; limit_price } ->
      Alcotest.check price_testable "trigger halves" (price "55") trigger_price;
      Alcotest.check price_testable "limit halves" (price "56") limit_price
  | T.Order.Market | T.Order.Limit _ | T.Order.Stop _ ->
      Alcotest.fail "expected adjusted stop-limit"

let engine_audits_trigger_and_defers_fill () =
  let request =
    request_v8 ~kind:(T.Order.Stop (price "110")) ~time_in_force:T.Order.Gtc ()
  in
  let state = runner [ (1L, [ T.Strategy.Submit_order request ]) ] in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let state, triggered =
    Runner.process_slice state
      (market_slice
         ~bars:
           [
             bar ~open_price:"100" ~high_price:"115" ~low_price:"95"
               ~close_price:"105" 2L;
           ]
         2L)
    |> ok
  in
  Alcotest.(check bool)
    "trigger is audited" true
    (List.mem "order_triggered" (event_names triggered));
  Alcotest.(check bool)
    "trigger slice does not fill" false
    (List.mem "fill_applied" (event_names triggered));
  let _, filled =
    Runner.process_slice state
      (market_slice ~bars:[ bar ~open_price:"112" 3L ] 3L)
    |> ok
  in
  Alcotest.(check bool)
    "following slice fills" true
    (List.mem "fill_applied" (event_names filled))

let gtd_and_day_expire_deterministically () =
  let gtd =
    request_v8
      ~kind:(T.Order.Limit (price "90"))
      ~time_in_force:(T.Order.Gtd (timestamp "2026-01-03T20:00:00Z"))
      ()
  in
  let state = runner [ (1L, [ T.Strategy.Submit_order gtd ]) ] in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let _, expired = Runner.process_slice state (market_slice 2L) |> ok in
  let reason =
    match
      List.find_map
        (fun audit ->
          match audit.T.Audit.event with
          | T.Audit.Order_cancelled { reason; _ } -> Some reason
          | _ -> None)
        expired
    with
    | Some reason -> reason
    | None ->
        Alcotest.failf "missing GTD cancellation in [%s]"
          (String.concat ", " (event_names expired))
  in
  Alcotest.(check string)
    "GTD reason" "gtd_expired"
    (T.Audit.cancellation_reason_to_string reason);
  let calendar = calendar () in
  let day =
    request_v8
      ~kind:(T.Order.Stop (price "101"))
      ~time_in_force:
        (T.Order.Day
           {
             venue_id = T.Id.Venue.of_string_exn "XNAS";
             calendar_id = T.Id.Venue_calendar.of_string_exn "xnas-test";
           })
      ()
  in
  let state =
    runner ~venue_calendars:[ calendar ]
      [ (1L, [ T.Strategy.Submit_order day ]) ]
  in
  let state, _ = Runner.process_slice state (market_slice 1L) |> ok in
  let _, expired =
    Runner.process_slice state
      (market_slice
         ~bars:
           [
             bar ~open_price:"100" ~high_price:"105" ~low_price:"95"
               ~close_price:"100" 2L;
           ]
         2L)
    |> ok
  in
  let reason =
    List.find_map
      (fun audit ->
        match audit.T.Audit.event with
        | T.Audit.Order_cancelled { reason; _ } -> Some reason
        | _ -> None)
      expired
    |> Option.get
  in
  Alcotest.(check string)
    "DAY reason" "day_expired"
    (T.Audit.cancellation_reason_to_string reason);
  Alcotest.(check bool)
    "DAY stop triggers at session boundary" true
    (List.mem "order_triggered" (event_names expired));
  Alcotest.(check bool)
    "DAY stop cannot fill after its session" false
    (List.mem "fill_applied" (event_names expired))

let fok_rejects_risk_clipped_fill () =
  let order = request_v8 ~time_in_force:T.Order.Fok () in
  let state =
    runner ~initial_cash:"550"
      ~risk:(risk ~max_leverage:"1" ())
      [ (1L, [ T.Strategy.Submit_order order ]) ]
  in
  let state, _ =
    Runner.process_slice state
      (market_slice ~bars:[ bar ~close_price:"50" ~low_price:"50" 1L ] 1L)
    |> ok
  in
  let state, events =
    Runner.process_slice state
      (market_slice ~bars:[ bar ~open_price:"100" ~close_price:"100" 2L ] 2L)
    |> ok
  in
  Alcotest.check quantity_testable "FOK applies no position"
    T.Scalar.Quantity.zero
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check bool)
    "risk clipping is audited" true
    (List.mem "fill_clipped" (event_names events));
  let reason =
    List.find_map
      (fun audit ->
        match audit.T.Audit.event with
        | T.Audit.Order_cancelled { reason; _ } -> Some reason
        | _ -> None)
      events
    |> Option.get
  in
  Alcotest.(check string)
    "FOK cancellation reason" "fill_or_kill"
    (T.Audit.cancellation_reason_to_string reason)

let day_identity_is_validated () =
  let day venue =
    request_v8
      ~kind:(T.Order.Limit (price "90"))
      ~time_in_force:
        (T.Order.Day
           {
             venue_id = T.Id.Venue.of_string_exn venue;
             calendar_id = T.Id.Venue_calendar.of_string_exn "xnas-test";
           })
      ()
  in
  let rejection state =
    let _, events = Runner.process_slice state (market_slice 1L) |> ok in
    List.find_map
      (fun audit ->
        match audit.T.Audit.event with
        | T.Audit.Order_rejected { status = T.Order.Rejected reason; _ } ->
            Some reason
        | _ -> None)
      events
    |> Option.get
  in
  let unknown = runner [ (1L, [ T.Strategy.Submit_order (day "XNAS") ]) ] in
  Alcotest.(check string)
    "unknown calendar" "DAY order refers to an unknown calendar"
    (rejection unknown);
  let wrong_venue =
    runner
      ~venue_calendars:[ calendar () ]
      [ (1L, [ T.Strategy.Submit_order (day "XNYS") ]) ]
  in
  Alcotest.(check string)
    "venue mismatch" "DAY order venue differs from its calendar"
    (rejection wrong_venue);
  let uncovered =
    runner
      ~venue_calendars:[ calendar ~covered_instrument:"other-equity" () ]
      [ (1L, [ T.Strategy.Submit_order (day "XNAS") ]) ]
  in
  Alcotest.(check string)
    "instrument coverage" "DAY order calendar does not cover its instrument"
    (rejection uncovered)

let tests =
  [
    Alcotest.test_case "legacy compatibility mapping" `Quick
      compatibility_mapping;
    Alcotest.test_case "stop-limit and GTD validation" `Quick
      validates_stop_limit_and_gtd;
    Alcotest.test_case "v8 intent companions" `Quick
      v8_intent_requires_explicit_companions;
    Alcotest.test_case "order validation and serialization" `Quick
      order_validation_and_serialization_branches;
    Alcotest.test_case "stop triggers before later execution" `Quick
      trigger_then_execute_on_following_slice;
    Alcotest.test_case "stop-limit activation" `Quick
      stop_limit_uses_limit_after_trigger;
    Alcotest.test_case "sell stop gap and partial fill" `Quick
      sell_stop_gap_and_partial_fill;
    Alcotest.test_case "FOK all-or-cancel" `Quick fok_is_all_or_cancel;
    Alcotest.test_case "split adjusts stop prices" `Quick
      split_adjusts_stop_prices;
    Alcotest.test_case "engine audits stop trigger" `Quick
      engine_audits_trigger_and_defers_fill;
    Alcotest.test_case "GTD and DAY expiration" `Quick
      gtd_and_day_expire_deterministically;
    Alcotest.test_case "FOK rejects risk clipping" `Quick
      fok_rejects_risk_clipped_fill;
    Alcotest.test_case "DAY identity validation" `Quick
      day_identity_is_validated;
  ]
