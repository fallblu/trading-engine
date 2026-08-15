open Test_support
module T = Trading_engine

let initialization () =
  let instrument = instrument () in
  T.Strategy_protocol.
    {
      scenario_contract_version = T.Contract.version;
      scenario_sha256;
      metadata = `Assoc [ ("experiment", `String "demo") ];
      run_id = run_id "test-run";
      base_currency = "USD";
      initial_cash = [ ("USD", money "10000") ];
      instruments = [ instrument ];
      risk = risk ~instruments:[ instrument ] ();
      execution_model = T.Execution_model.find "completed_bar_v1" |> ok;
      execution = execution ();
    }

let field name = function
  | `Assoc fields -> List.assoc name fields
  | _ -> Alcotest.fail "expected JSON object"

let initialize_message_is_complete () =
  let message =
    T.Strategy_protocol.initialize_message ~sequence:1L (initialization ())
  in
  Alcotest.(check string)
    "protocol version" "1"
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

let event_message_contains_complete_context () =
  let account = test_account () in
  let slice = market_slice 1L in
  let context =
    T.Strategy.context ~now:slice.received_at ~account ~working_orders:[]
      ~latest_bars:slice.bars
  in
  let message =
    T.Strategy_protocol.event_message ~sequence:2L
      ~instruments:[ instrument () ]
      context (T.Strategy.Market_slice_closed slice)
  in
  let payload = field "payload" message in
  let context = field "context" payload in
  Alcotest.(check int)
    "one cash ledger" 1
    (match field "cash_balances" context with
    | `List values -> List.length values
    | _ -> Alcotest.fail "expected cash balances");
  Alcotest.(check string)
    "zero position" "0"
    (match field "positions" context with
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

let response message_type payload =
  `Assoc
    [
      ("strategy_protocol_version", `String "1");
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
    T.Strategy_protocol.response_of_yojson ~expected_sequence:4L ready |> error
  in
  Alcotest.(check bool)
    "wrong sequence rejected" true
    (String.starts_with ~prefix:"expected strategy sequence 4" wrong_sequence);
  let duplicate =
    `Assoc
      [
        ("strategy_protocol_version", `String "1");
        ("strategy_protocol_version", `String "1");
        ("strategy_sequence", `String "3");
        ("message_type", `String "stopped");
        ("payload", `Assoc []);
      ]
  in
  Alcotest.(check string)
    "duplicate field rejected"
    "strategy response must not contain duplicate fields"
    (T.Strategy_protocol.response_of_yojson ~expected_sequence:3L duplicate
    |> error);
  let wrong_version =
    `Assoc
      [
        ("strategy_protocol_version", `String "2");
        ("strategy_sequence", `String "3");
        ("message_type", `String "stopped");
        ("payload", `Assoc []);
      ]
  in
  Alcotest.(check string)
    "wrong version rejected" "unsupported strategy protocol version: 2"
    (T.Strategy_protocol.response_of_yojson ~expected_sequence:3L wrong_version
    |> error);
  let unknown_field =
    `Assoc
      [
        ("strategy_protocol_version", `String "1");
        ("strategy_sequence", `String "3");
        ("message_type", `String "stopped");
        ("payload", `Assoc []);
        ("unexpected", `Bool true);
      ]
  in
  Alcotest.(check string)
    "unknown field rejected" "strategy response has unknown or missing fields"
    (T.Strategy_protocol.response_of_yojson ~expected_sequence:3L unknown_field
    |> error);
  Alcotest.(check bool)
    "malformed JSON rejected" true
    (T.Strategy_protocol.response_of_string ~expected_sequence:3L "{"
    |> error
    |> String.starts_with ~prefix:"invalid strategy response JSON:");
  Alcotest.(check string)
    "oversized response rejected"
    "strategy response exceeds the maximum message size"
    (T.Strategy_protocol.response_of_string ~expected_sequence:3L
       (String.make (T.Strategy_protocol.max_message_bytes + 1) 'x')
    |> error)

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

let tests =
  [
    Alcotest.test_case "initialize message is complete" `Quick
      initialize_message_is_complete;
    Alcotest.test_case "event context is complete" `Quick
      event_message_contains_complete_context;
    Alcotest.test_case "responses are strict and typed" `Quick
      responses_are_strict_and_typed;
    Alcotest.test_case "transcript records direction" `Quick
      transcript_records_direction_and_sequence;
  ]
