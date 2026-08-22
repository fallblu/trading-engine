module T = Trading_engine

let field name = function
  | `Assoc fields -> List.assoc name fields
  | _ -> Alcotest.fail "expected JSON object"

let renders_stable_machine_context () =
  let diagnostic =
    T.Diagnostic.make ~code:T.Diagnostic.Scenario_stream_invalid
      ~phase:T.Diagnostic.Validation ~json_path:"$.payload.market_slice" ~line:7
      ~sequence:9L ~event_id:"event-9" ~order_id:"order-2"
      ~causation_ids:[ "event-7"; "event-8" ] "invalid market slice"
  in
  let json = T.Diagnostic.to_yojson diagnostic in
  Alcotest.(check string)
    "diagnostic version" "1"
    (match field "diagnostic_version" json with
    | `String value -> value
    | _ -> Alcotest.fail "expected version");
  Alcotest.(check string)
    "stable code" "scenario_stream.invalid"
    (match field "code" json with
    | `String value -> value
    | _ -> Alcotest.fail "expected code");
  let context = field "context" json in
  Alcotest.(check string)
    "JSON path" "$.payload.market_slice"
    (match field "json_path" context with
    | `String value -> value
    | _ -> Alcotest.fail "expected JSON path");
  Alcotest.(check int)
    "line" 7
    (match field "line" context with
    | `Int value -> value
    | _ -> Alcotest.fail "expected line");
  Alcotest.(check string)
    "sequence" "9"
    (match field "sequence" context with
    | `String value -> value
    | _ -> Alcotest.fail "expected sequence")

let preserves_sanitized_exception () =
  let diagnostic =
    T.Diagnostic.of_exception ~code:T.Diagnostic.Artifact_io
      ~phase:T.Diagnostic.Artifact ~message:"could not publish artifact"
      (Unix.Unix_error (Unix.EACCES, "link", "/tmp/output"))
  in
  let cause = T.Diagnostic.to_yojson diagnostic |> field "cause" in
  Alcotest.(check string)
    "cause kind" "unix_error"
    (match field "kind" cause with
    | `String value -> value
    | _ -> Alcotest.fail "expected cause kind");
  Alcotest.(check string)
    "operation" "link"
    (match field "operation" cause with
    | `String value -> value
    | _ -> Alcotest.fail "expected operation");
  Alcotest.(check string)
    "target" "/tmp/output"
    (match field "target" cause with
    | `String value -> value
    | _ -> Alcotest.fail "expected target")

let capabilities_publish_versioned_resource_limits () =
  let limits =
    T.Contract.capabilities_to_yojson () |> field "resource_limits"
  in
  Alcotest.(check string)
    "resource contract version" T.Resource_limits.version
    (match field "version" limits with
    | `String value -> value
    | _ -> Alcotest.fail "expected resource limit version");
  List.iter
    (fun (name, expected) ->
      Alcotest.(check int)
        name expected
        (match field name limits with
        | `Int value -> value
        | _ -> Alcotest.fail ("expected integer limit: " ^ name)))
    [
      ("scenario_record_bytes", T.Resource_limits.scenario_record_bytes);
      ("strategy_message_bytes", T.Resource_limits.strategy_message_bytes);
      ("internal_events", T.Resource_limits.internal_events);
      ("catalog_instruments", T.Resource_limits.catalog_instruments);
      ("intents_per_batch", T.Resource_limits.intents_per_batch);
      ("artifact_record_bytes", T.Resource_limits.artifact_record_bytes);
    ];
  Alcotest.(check string)
    "resource diagnostic code" "resource.limit"
    (T.Diagnostic.code_to_string T.Diagnostic.Resource_limit)

let capabilities_describe_execution_contracts () =
  let model =
    match
      T.Contract.capabilities_to_yojson () |> field "execution_model_contracts"
    with
    | `List [ model ] -> model
    | _ -> Alcotest.fail "expected one execution-model capability"
  in
  Alcotest.(check string)
    "stable model name" "completed_bar_v1"
    (match field "name" model with
    | `String value -> value
    | _ -> Alcotest.fail "expected execution-model name");
  let strings name =
    match field name model with
    | `List values ->
        List.map
          (function
            | `String value -> value
            | _ -> Alcotest.fail (name ^ " must contain strings"))
          values
    | _ -> Alcotest.fail (name ^ " must be an array")
  in
  Alcotest.(check (list string))
    "configuration versions" [ "2"; "1" ]
    (strings "configuration_versions");
  Alcotest.(check (list string))
    "scenario contracts"
    [ "11"; "10"; "9"; "8"; "7"; "6"; "5"; "4"; "3" ]
    (strings "scenario_contract_versions");
  Alcotest.(check (list string))
    "required fields"
    [ "version"; "participation_bps"; "fee_schedules" ]
    (strings "required_fields");
  Alcotest.(check (list string))
    "order types"
    [ "market"; "limit"; "stop"; "stop_limit" ]
    (strings "supported_order_types");
  Alcotest.(check (list string))
    "market data" [ "completed_ohlcv_bars" ]
    (strings "data_requirements")

let tests =
  [
    Alcotest.test_case "renders stable machine context" `Quick
      renders_stable_machine_context;
    Alcotest.test_case "preserves sanitized exception" `Quick
      preserves_sanitized_exception;
    Alcotest.test_case "versioned resource capabilities" `Quick
      capabilities_publish_versioned_resource_limits;
    Alcotest.test_case "execution-model capabilities" `Quick
      capabilities_describe_execution_contracts;
  ]
