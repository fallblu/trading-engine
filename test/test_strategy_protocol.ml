open Test_support
module T = Trading_engine

let initialization () =
  let instrument = instrument () in
  let component =
    T.Fee_schedule.create_component ~name:"broker" ~currency:"USD"
      ~basis:(T.Fee_schedule.Fixed (money "0.25"))
      ~rounding:T.Fee_schedule.Up ~applicability:T.Fee_schedule.Any
    |> ok
  in
  let fee_schedule =
    T.Fee_schedule.create ~schedule_id:"test-fees-v1"
      ~instrument_id:instrument.id ~settlement_currency:"USD" ~minimum:None
      ~maximum:None ~components:[ component ]
    |> ok
  in
  T.Strategy_protocol.
    {
      scenario_contract_version = T.Contract.version;
      scenario_sha256;
      metadata = `Assoc [ ("experiment", `String "demo") ];
      run_id = run_id "test-run";
      base_currency = "USD";
      initial_cash = [ ("USD", money "10000") ];
      initial_portfolio = None;
      instruments = [ instrument ];
      venue_calendars = [];
      risk = risk ~instruments:[ instrument ] ();
      execution_model = T.Execution_model.find "completed_bar_v1" |> ok;
      execution =
        T.Execution.create_v2 ~participation_bps:10_000
          ~fee_schedules:[ fee_schedule ]
        |> ok;
      financing = Some T.Financing.legacy_policy;
      settlement = None;
    }

let field name = function
  | `Assoc fields -> List.assoc name fields
  | _ -> Alcotest.fail "expected JSON object"

let initialize_message_is_complete () =
  let message =
    T.Strategy_protocol.initialize_message ~sequence:1L (initialization ())
  in
  Alcotest.(check string)
    "protocol version" "10"
    (match field "strategy_protocol_version" message with
    | `String value -> value
    | _ -> Alcotest.fail "expected version string");
  Alcotest.(check string)
    "message type" "initialize"
    (match field "message_type" message with
    | `String value -> value
    | _ -> Alcotest.fail "expected message type");
  let payload = field "payload" message in
  Alcotest.(check string)
    "scenario hash" scenario_sha256
    (match field "scenario_sha256" payload with
    | `String value -> value
    | _ -> Alcotest.fail "expected scenario hash");
  Alcotest.(check int)
    "one instrument" 1
    (match field "instruments" payload with
    | `List values -> List.length values
    | _ -> Alcotest.fail "expected instruments")

let initialize_message_includes_calendars () =
  let phase =
    T.Venue_calendar.create_phase ~kind:T.Venue_calendar.Regular
      ~opens_at:(timestamp "2026-01-02T14:30:00Z")
      ~closes_at:(timestamp "2026-01-02T21:00:00Z")
    |> ok
  in
  let session =
    T.Venue_calendar.create_session ~session_date:"2026-01-02"
      ~kind:T.Venue_calendar.Regular_session ~phases:[ phase ]
    |> ok
  in
  let holiday =
    T.Venue_calendar.create_session ~session_date:"2026-01-03"
      ~kind:T.Venue_calendar.Holiday ~phases:[]
    |> ok
  in
  let calendar =
    T.Venue_calendar.create
      ~id:(T.Id.Venue_calendar.of_string_exn "xnas-test")
      ~version:"1"
      ~venue_id:(T.Id.Venue.of_string_exn "XNAS")
      ~instrument_ids:[ instrument_id "test-equity" ]
      ~sessions:[ session; holiday ]
    |> ok
  in
  let message =
    T.Strategy_protocol.initialize_message ~sequence:1L
      { (initialization ()) with venue_calendars = [ calendar ] }
  in
  match field "payload" message |> field "venue_calendars" with
  | `List [ calendar ] ->
      Alcotest.(check string)
        "calendar identity" "xnas-test"
        (match field "calendar_id" calendar with
        | `String value -> value
        | _ -> Alcotest.fail "expected calendar ID")
  | _ -> Alcotest.fail "expected one serialized venue calendar"

let legacy_initialize_message_remains_frozen () =
  let initialization =
    {
      (initialization ()) with
      scenario_contract_version = T.Contract.legacy_journal_version;
    }
  in
  let message =
    T.Strategy_protocol.initialize_message ~sequence:1L initialization
  in
  Alcotest.(check string)
    "legacy protocol version" "3"
    (match field "strategy_protocol_version" message with
    | `String value -> value
    | _ -> Alcotest.fail "expected version string");
  let payload = field "payload" message in
  Alcotest.(check bool)
    "no v4 initial portfolio" false
    (match payload with
    | `Assoc fields -> List.mem_assoc "initial_portfolio" fields
    | _ -> Alcotest.fail "expected payload object");
  let execution = field "execution" payload in
  Alcotest.(check bool)
    "flat v3 execution" true
    (match execution with
    | `Assoc fields ->
        List.mem_assoc "participation_bps" fields
        && not (List.mem_assoc "configuration" fields)
    | _ -> Alcotest.fail "expected execution object")

let event_message_contains_complete_context () =
  let account = test_account () in
  let slice = market_slice 1L in
  let valuation =
    account_value account ~marks:[ (instrument_id "test-equity", price "105") ]
  in
  let context =
    T.Strategy.context ~now:slice.received_at ~valuation ~group_exposures:[]
      ~working_orders:[] ~latest_bars:slice.bars
    |> ok
  in
  let message =
    T.Strategy_protocol.event_message ~sequence:2L context
      (T.Strategy.Market_slice_closed slice)
  in
  let payload = field "payload" message in
  let context = field "context" payload in
  let portfolio = field "portfolio" context in
  Alcotest.(check int)
    "one cash ledger" 1
    (match field "cash_balances" portfolio with
    | `List values -> List.length values
    | _ -> Alcotest.fail "expected cash balances");
  Alcotest.(check string)
    "equity" "10000"
    (match field "equity" portfolio with
    | `String value -> value
    | _ -> Alcotest.fail "expected equity");
  Alcotest.(check bool)
    "weights available" true
    (match field "weights_available" portfolio with
    | `Bool value -> value
    | _ -> Alcotest.fail "expected weight availability");
  Alcotest.(check string)
    "cash weight" "1"
    (match field "cash_weight" portfolio with
    | `String value -> value
    | _ -> Alcotest.fail "expected cash weight");
  Alcotest.(check string)
    "zero position" "0"
    (match field "positions" portfolio with
    | `List [ position ] -> (
        match field "quantity" position with
        | `String value -> value
        | _ -> Alcotest.fail "expected quantity")
    | _ -> Alcotest.fail "expected complete positions");
  Alcotest.(check string)
    "event type" "market_slice_closed"
    (match field "event" payload |> field "type" with
    | `String value -> value
    | _ -> Alcotest.fail "expected event type")

let nonpositive_equity_omits_weights () =
  let account = test_account ~initial_cash:[ ("USD", money "0") ] () in
  let slice = market_slice 1L in
  let valuation =
    account_value account ~marks:[ (instrument_id "test-equity", price "105") ]
  in
  let context =
    T.Strategy.context ~now:slice.received_at ~valuation ~group_exposures:[]
      ~working_orders:[] ~latest_bars:slice.bars
    |> ok
  in
  let message =
    T.Strategy_protocol.event_message ~sequence:2L context
      (T.Strategy.Market_slice_closed slice)
  in
  let portfolio =
    field "payload" message |> field "context" |> field "portfolio"
  in
  Alcotest.(check bool)
    "weights unavailable" false
    (match field "weights_available" portfolio with
    | `Bool value -> value
    | _ -> Alcotest.fail "expected weight availability");
  Alcotest.(check bool)
    "cash weight null" true
    (field "cash_weight" portfolio = `Null);
  Alcotest.(check bool)
    "position weight null" true
    (match field "positions" portfolio with
    | `List [ position ] -> field "weight" position = `Null
    | _ -> Alcotest.fail "expected complete positions")

let response message_type payload =
  `Assoc
    [
      ("strategy_protocol_version", `String "10");
      ("strategy_sequence", `String "3");
      ("message_type", `String message_type);
      ("payload", payload);
    ]

let responses_are_strict_and_typed () =
  let ready =
    response "ready"
      (`Assoc
         [
           ("strategy_name", `String "momentum");
           ("strategy_version", `String "1.2.3");
         ])
  in
  (match
     T.Strategy_protocol.response_of_yojson ~expected_sequence:3L ready |> ok
   with
  | T.Strategy_protocol.Ready identity ->
      Alcotest.(check string)
        "strategy name" "momentum"
        (T.Id.Strategy.to_string identity.name);
      Alcotest.(check (option string))
        "strategy version" (Some "1.2.3") identity.version
  | _ -> Alcotest.fail "expected ready response");
  let intents =
    response "intents"
      (`Assoc
         [
           ( "intents",
             `List
               [
                 `Assoc
                   [
                     ("type", `String "emit_metric");
                     ("name", `String "signal");
                     ("value", `String "0.5");
                   ];
               ] );
         ])
  in
  (match
     T.Strategy_protocol.response_of_yojson ~expected_sequence:3L intents |> ok
   with
  | T.Strategy_protocol.Intents [ T.Strategy.Emit_metric { name; value } ] ->
      Alcotest.(check string) "metric name" "signal" name;
      Alcotest.(check string) "metric value" "0.5" value
  | _ -> Alcotest.fail "expected metric intent");
  let wrong_sequence =
    T.Strategy_protocol.response_of_yojson ~expected_sequence:4L ready
    |> diagnostic_message
  in
  Alcotest.(check bool)
    "wrong sequence rejected" true
    (String.starts_with ~prefix:"expected strategy sequence 4" wrong_sequence);
  let duplicate =
    `Assoc
      [
        ("strategy_protocol_version", `String "7");
        ("strategy_protocol_version", `String "7");
        ("strategy_sequence", `String "3");
        ("message_type", `String "stopped");
        ("payload", `Assoc []);
      ]
  in
  Alcotest.(check string)
    "duplicate field rejected"
    "strategy response must not contain duplicate fields"
    (T.Strategy_protocol.response_of_yojson ~expected_sequence:3L duplicate
    |> diagnostic_message);
  let wrong_version =
    `Assoc
      [
        ("strategy_protocol_version", `String "1");
        ("strategy_sequence", `String "3");
        ("message_type", `String "stopped");
        ("payload", `Assoc []);
      ]
  in
  Alcotest.(check string)
    "wrong version rejected" "unsupported strategy protocol version: 1"
    (T.Strategy_protocol.response_of_yojson ~expected_sequence:3L wrong_version
    |> diagnostic_message);
  let unknown_field =
    `Assoc
      [
        ("strategy_protocol_version", `String "7");
        ("strategy_sequence", `String "3");
        ("message_type", `String "stopped");
        ("payload", `Assoc []);
        ("unexpected", `Bool true);
      ]
  in
  Alcotest.(check string)
    "unknown field rejected" "strategy response has unknown or missing fields"
    (T.Strategy_protocol.response_of_yojson ~expected_sequence:3L unknown_field
    |> diagnostic_message);
  Alcotest.(check bool)
    "malformed JSON rejected" true
    (T.Strategy_protocol.response_of_string ~expected_sequence:3L "{"
    |> diagnostic_message
    |> String.starts_with ~prefix:"invalid strategy response JSON:");
  let oversized =
    T.Strategy_protocol.response_of_string ~expected_sequence:3L
      (String.make (T.Strategy_protocol.max_message_bytes + 1) 'x')
    |> error
  in
  Alcotest.(check string)
    "oversized response code" "resource.limit"
    (T.Diagnostic.code_to_string oversized.code);
  Alcotest.(check string)
    "oversized response rejected"
    (Printf.sprintf "strategy message is %d bytes; limit is %d bytes"
       (T.Strategy_protocol.max_message_bytes + 1)
       T.Strategy_protocol.max_message_bytes)
    oversized.message;
  let oversized_intents =
    response "intents"
      (`Assoc
         [
           ( "intents",
             `List
               (List.init (T.Resource_limits.intents_per_batch + 1) (fun _ ->
                    `Null)) );
         ])
    |> T.Strategy_protocol.response_of_yojson ~expected_sequence:3L
    |> error
  in
  Alcotest.(check string)
    "intent limit code" "resource.limit"
    (T.Diagnostic.code_to_string oversized_intents.code);
  Alcotest.(check (option string))
    "intent limit path" (Some "$.payload.intents")
    oversized_intents.context.json_path

let strategy_configuration_is_validated_before_use () =
  let initialization =
    {
      (initialization ()) with
      metadata =
        `Assoc
          [
            ( "padding",
              `String (String.make T.Resource_limits.strategy_message_bytes 'x')
            );
          ];
    }
  in
  let diagnostic =
    T.Strategy_process.validate_configuration ~command:[ "unused" ] ~timeout:1.0
      ~initialization
    |> error
  in
  Alcotest.(check string)
    "initialization limit code" "resource.limit"
    (T.Diagnostic.code_to_string diagnostic.code);
  Alcotest.(check (option int64))
    "initialization sequence" (Some 1L) diagnostic.context.sequence

let transcript_records_direction_and_sequence () =
  let message = T.Strategy_protocol.shutdown_message ~sequence:9L in
  let record =
    T.Strategy_protocol.transcript_record ~transcript_sequence:17L
      ~direction:T.Strategy_protocol.Engine_to_strategy ~message
  in
  Alcotest.(check string)
    "transcript sequence" "17"
    (match field "transcript_sequence" record with
    | `String value -> value
    | _ -> Alcotest.fail "expected transcript sequence");
  Alcotest.(check string)
    "direction" "engine_to_strategy"
    (match field "direction" record with
    | `String value -> value
    | _ -> Alcotest.fail "expected transcript direction")

let absent_temp_path suffix =
  let path = Filename.temp_file "trading-engine-process" suffix in
  Sys.remove path;
  path

let remove_if_exists path = if Sys.file_exists path then Sys.remove path

let grandchild_pid pid_path =
  In_channel.with_open_text pid_path (fun channel ->
      In_channel.input_all channel |> String.trim |> int_of_string)

let check_process_gone pid =
  match Unix.kill pid 0 with
  | () -> Alcotest.fail "grandchild process survived session cleanup"
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ()

let with_process_tree_paths test =
  let pid_path = absent_temp_path ".pid" in
  let transcript_path = absent_temp_path ".jsonl" in
  Fun.protect
    ~finally:(fun () ->
      remove_if_exists pid_path;
      remove_if_exists transcript_path;
      remove_if_exists (transcript_path ^ ".partial"))
    (fun () -> test pid_path transcript_path)

let invalid_configuration_creates_no_transcript_or_process () =
  with_process_tree_paths @@ fun _ transcript_path ->
  let initialization =
    {
      (initialization ()) with
      metadata =
        `Assoc
          [
            ( "padding",
              `String (String.make T.Resource_limits.strategy_message_bytes 'x')
            );
          ];
    }
  in
  let diagnostic =
    Eio_main.run @@ fun env ->
    T.Strategy_process.with_session ~env
      ~command:[ "/definitely/missing/strategy" ]
      ~timeout:1.0 ~transcript_path ~initialization (fun _ -> Ok ())
    |> error
  in
  Alcotest.(check string)
    "configuration fails before spawn" "resource.limit"
    (T.Diagnostic.code_to_string diagnostic.code);
  Alcotest.(check bool) "no transcript" false (Sys.file_exists transcript_path);
  Alcotest.(check bool)
    "no partial transcript" false
    (Sys.file_exists (transcript_path ^ ".partial"))

let callback_exception_reaps_process_tree () =
  with_process_tree_paths @@ fun pid_path transcript_path ->
  let result =
    Eio_main.run @@ fun env ->
    T.Strategy_process.with_session ~env
      ~command:[ "./fake_strategy.py"; "spawn-grandchild-success"; pid_path ]
      ~timeout:1.0 ~transcript_path ~initialization:(initialization ())
      (fun _ -> raise Exit)
  in
  let message = diagnostic_message result in
  Alcotest.(check bool)
    "callback exception reported" true
    (String.ends_with ~suffix:"Stdlib.Exit" message);
  grandchild_pid pid_path |> check_process_gone

let cancellation_reaps_process_tree () =
  with_process_tree_paths @@ fun pid_path transcript_path ->
  let timed_out =
    Eio_main.run @@ fun env ->
    try
      ignore
        (Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.2 (fun () ->
             T.Strategy_process.with_session ~env
               ~command:
                 [ "./fake_strategy.py"; "spawn-grandchild-success"; pid_path ]
               ~timeout:1.0 ~transcript_path ~initialization:(initialization ())
               (fun _ ->
                 Eio.Time.sleep (Eio.Stdenv.clock env) 60.0;
                 Ok ())));
      false
    with Eio.Time.Timeout -> true
  in
  Alcotest.(check bool) "session cancellation timed out" true timed_out;
  grandchild_pid pid_path |> check_process_gone

let tests =
  [
    Alcotest.test_case "initialize message is complete" `Quick
      initialize_message_is_complete;
    Alcotest.test_case "initialize message includes calendars" `Quick
      initialize_message_includes_calendars;
    Alcotest.test_case "legacy initialize message remains frozen" `Quick
      legacy_initialize_message_remains_frozen;
    Alcotest.test_case "event context is complete" `Quick
      event_message_contains_complete_context;
    Alcotest.test_case "nonpositive equity omits weights" `Quick
      nonpositive_equity_omits_weights;
    Alcotest.test_case "responses are strict and typed" `Quick
      responses_are_strict_and_typed;
    Alcotest.test_case "strategy configuration is bounded" `Quick
      strategy_configuration_is_validated_before_use;
    Alcotest.test_case "configuration precedes transcript and process" `Quick
      invalid_configuration_creates_no_transcript_or_process;
    Alcotest.test_case "transcript records direction" `Quick
      transcript_records_direction_and_sequence;
    Alcotest.test_case "callback exception reaps process tree" `Slow
      callback_exception_reaps_process_tree;
    Alcotest.test_case "cancellation reaps process tree" `Slow
      cancellation_reaps_process_tree;
  ]
