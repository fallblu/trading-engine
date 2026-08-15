open Test_support
module T = Trading_engine

let demo_document () =
  In_channel.with_open_bin "../contracts/v1/fixtures/demo.scenario.json"
    In_channel.input_all

let demo () = T.Scenario.of_string (demo_document ()) |> ok
let demo_hash () = T.Sha256.digest_string (demo_document ())

let demo_contract_parses () =
  let scenario = demo () in
  Alcotest.(check string)
    "contract" T.Contract.version scenario.contract_version;
  Alcotest.(check string) "run" "demo" (T.Id.Run.to_string scenario.run_id);
  Alcotest.(check int) "one instrument" 1 (List.length scenario.instruments);
  Alcotest.(check int) "four slices" 4 (List.length scenario.slices);
  match scenario.metadata with
  | `Assoc fields ->
      Alcotest.(check bool)
        "metadata preserved" true
        (List.mem_assoc "producer" fields)
  | _ -> Alcotest.fail "metadata must be an object"

let schema_artifacts_parse () =
  let check_schema path =
    match Yojson.Safe.from_file path with
    | `Assoc fields ->
        Alcotest.(check (option string))
          (path ^ " draft")
          (Some "https://json-schema.org/draft/2020-12/schema")
          (Option.bind (List.assoc_opt "$schema" fields) (function
            | `String value -> Some value
            | _ -> None));
        Alcotest.(check bool)
          (path ^ " definitions") true
          (List.mem_assoc "$defs" fields)
    | _ -> Alcotest.fail (path ^ " must contain a JSON object")
  in
  check_schema "../contracts/v1/scenario.schema.json";
  check_schema "../contracts/v1/journal.schema.json"

let timestamp_precision_is_bounded () =
  List.iter
    (fun value ->
      Alcotest.(check bool)
        (value ^ " accepted") true
        (Result.is_ok (T.Codec.ptime_of_string value)))
    [
      "2026-01-02T14:30:00Z";
      "2026-01-02t14:30:00.1z";
      "2026-01-02T14:30:00.123456+05:30";
      "2026-01-02T14:30:00-05:00";
    ];
  List.iter
    (fun value ->
      Alcotest.(check bool)
        (value ^ " rejected") true
        (Result.is_error (T.Codec.ptime_of_string value)))
    [
      "2026-01-02 14:30:00Z";
      "2026-01-02T14:30:00+0000";
      "2026-01-02T14:30:00-05";
      "2026-01-02T14:30:60Z";
    ];
  match T.Codec.ptime_of_string "2026-01-02T14:30:00.1234567Z" with
  | Ok _ -> Alcotest.fail "sub-microsecond timestamp accepted"
  | Error message ->
      Alcotest.(check string)
        "precision diagnosis"
        "RFC3339 timestamp must not exceed microsecond precision" message

let map_root change =
  match Yojson.Safe.from_string (demo_document ()) with
  | `Assoc fields -> `Assoc (change fields)
  | _ -> Alcotest.fail "demo must be an object"

let unknown_fields_are_rejected () =
  let changed = map_root (fun fields -> ("unexpected", `Bool true) :: fields) in
  Alcotest.(check bool)
    "unknown field rejected" true
    (Result.is_error (T.Scenario.of_yojson changed))

let contract_version_is_required_and_supported () =
  let missing =
    map_root
      (List.filter (fun (name, _) -> not (String.equal name "contract_version")))
  in
  Alcotest.(check bool)
    "unversioned scenario rejected" true
    (Result.is_error (T.Scenario.of_yojson missing));
  let unsupported =
    map_root (fun fields ->
        List.map
          (fun (name, value) ->
            if String.equal name "contract_version" then (name, `String "2")
            else (name, value))
          fields)
  in
  Alcotest.(check string)
    "unsupported version diagnosed"
    "unsupported scenario contract_version \"2\" (expected \"1\")"
    (T.Scenario.of_yojson unsupported |> error)

let duplicate_fields_are_rejected () =
  let changed =
    map_root (fun fields -> ("initial_cash", `String "0") :: fields)
  in
  let message = T.Scenario.of_yojson changed |> error in
  Alcotest.(check bool)
    "duplicate field diagnosed" true
    (String.starts_with ~prefix:"scenario has duplicate JSON fields" message)

let recursive_metadata_validation () =
  let duplicate =
    map_root (fun fields ->
        List.map
          (fun (name, value) ->
            if String.equal name "metadata" then
              ( name,
                `Assoc [ ("nested", `Assoc [ ("x", `Int 1); ("x", `Int 2) ]) ]
              )
            else (name, value))
          fields)
  in
  Alcotest.(check bool)
    "nested duplicate key rejected" true
    (Result.is_error (T.Scenario.of_yojson duplicate));
  let nonfinite =
    map_root (fun fields ->
        List.map
          (fun (name, value) ->
            if String.equal name "metadata" then
              (name, `Assoc [ ("invalid", `Float nan) ])
            else (name, value))
          fields)
  in
  Alcotest.(check bool)
    "non-finite metadata rejected" true
    (Result.is_error (T.Scenario.of_yojson nonfinite))

let update_first_schedule change =
  map_root (fun fields ->
      List.map
        (fun (name, json) ->
          if String.equal name "schedule" then
            match json with
            | `List (first :: rest) -> (name, `List (change first :: rest))
            | _ -> Alcotest.fail "demo schedule must be nonempty"
          else (name, json))
        fields)

let change_field key value = function
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (name, json) ->
             if String.equal name key then (name, value) else (name, json))
           fields)
  | _ -> Alcotest.fail "expected object"

let invalid_schedule_sequences_are_rejected () =
  let zero =
    update_first_schedule (change_field "after_slice_sequence" (`String "0"))
  in
  Alcotest.(check bool)
    "zero sequence rejected" true
    (Result.is_error (T.Scenario.of_yojson zero));
  let noncanonical =
    update_first_schedule (change_field "after_slice_sequence" (`String "01"))
  in
  Alcotest.(check bool)
    "noncanonical sequence rejected" true
    (Result.is_error (T.Scenario.of_yojson noncanonical));
  let missing =
    update_first_schedule (change_field "after_slice_sequence" (`String "999"))
  in
  let message = T.Scenario.of_yojson missing |> error in
  Alcotest.(check string)
    "missing sequence diagnosed"
    "scheduled intents refer to missing market slice sequence 999" message;
  let duplicate =
    map_root (fun fields ->
        List.map
          (fun (name, json) ->
            if String.equal name "schedule" then
              match json with
              | `List [ first; second ] ->
                  let second =
                    change_field "after_slice_sequence" (`String "1") second
                  in
                  (name, `List [ first; second ])
              | _ -> Alcotest.fail "expected two schedule entries"
            else (name, json))
          fields)
  in
  Alcotest.(check string)
    "duplicate schedule rejected" "schedule sequences must increase"
    (T.Scenario.of_yojson duplicate |> error)

let duplicate_and_incomplete_slice_bars_are_rejected () =
  let duplicate =
    map_root (fun fields ->
        List.map
          (fun (name, json) ->
            if String.equal name "slices" then
              match json with
              | `List (`Assoc slice_fields :: rest) ->
                  let first =
                    `Assoc
                      (List.map
                         (fun (key, value) ->
                           if String.equal key "bars" then
                             match value with
                             | `List [ bar ] -> (key, `List [ bar; bar ])
                             | _ -> Alcotest.fail "expected one bar"
                           else (key, value))
                         slice_fields)
                  in
                  (name, `List (first :: rest))
              | _ -> Alcotest.fail "expected slices"
            else (name, json))
          fields)
  in
  Alcotest.(check bool)
    "duplicate bar rejected" true
    (Result.is_error (T.Scenario.of_yojson duplicate))

let portfolio_targets_are_total_and_aligned () =
  let empty_targets =
    update_first_schedule (function
      | `Assoc fields ->
          `Assoc
            (List.map
               (fun (name, value) ->
                 if String.equal name "intents" then
                   match value with
                   | `List (`Assoc intent_fields :: rest) ->
                       let intent =
                         `Assoc
                           (List.map
                              (fun (key, target_value) ->
                                if String.equal key "targets" then
                                  (key, `List [])
                                else (key, target_value))
                              intent_fields)
                       in
                       (name, `List (intent :: rest))
                   | _ -> Alcotest.fail "expected intents"
                 else (name, value))
               fields)
      | _ -> Alcotest.fail "expected schedule object")
  in
  Alcotest.(check bool)
    "portfolio must cover catalog" true
    (Result.is_error (T.Scenario.of_yojson empty_targets));
  let noncanonical =
    map_root (fun fields ->
        List.map
          (fun (name, value) ->
            if String.equal name "initial_cash" then (name, `String "10000.0")
            else (name, value))
          fields)
  in
  Alcotest.(check bool)
    "noncanonical scalar rejected" true
    (Result.is_error (T.Scenario.of_yojson noncanonical))

let deterministic_replay () =
  let scenario = demo () in
  let hash = demo_hash () in
  let first = T.Replay.run ~scenario_sha256:hash scenario |> ok in
  let second = T.Replay.run ~scenario_sha256:hash scenario |> ok in
  let encode result = List.map T.Codec.audit_to_string result.T.Replay.audits in
  Alcotest.(check (list string))
    "byte-identical event encoding" (encode first) (encode second);
  Alcotest.(check int)
    "one valuation per slice" 4
    (List.length
       (List.filter
          (fun audit ->
            String.equal (T.Audit.event_name audit.T.Audit.event) "valuation")
          first.audits))

let replay_ends_with_completion_summary () =
  let hash = demo_hash () in
  let result = T.Replay.run ~scenario_sha256:hash (demo ()) |> ok in
  let first = List.hd result.audits in
  let completion = List.rev result.audits |> List.hd in
  Alcotest.(check string)
    "journal contract" T.Contract.version first.contract_version;
  (match first.event with
  | T.Audit.Run_started { scenario_sha256 = actual } ->
      Alcotest.(check string) "start hash" hash actual
  | _ -> Alcotest.fail "expected run start");
  match completion.event with
  | T.Audit.Run_completed { scenario_sha256 = actual; valuation; _ } ->
      Alcotest.(check string) "completion hash" hash actual;
      Alcotest.check money_testable "summary equity" result.valuation.equity
        valuation.equity
  | _ -> Alcotest.fail "expected run completion payload"

let replay_matches_golden_file () =
  let result = T.Replay.run ~scenario_sha256:(demo_hash ()) (demo ()) |> ok in
  let actual =
    result.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
    |> fun value -> value ^ "\n"
  in
  let expected =
    In_channel.with_open_bin "../contracts/v1/fixtures/demo.journal.jsonl"
      In_channel.input_all
  in
  Alcotest.(check string) "stable audit contract" expected actual

let journal_is_created_exclusively () =
  let scenario = demo () in
  let existing = Filename.temp_file "trading-engine" ".jsonl" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists existing then Sys.remove existing;
      if Sys.file_exists (existing ^ ".partial") then
        Sys.remove (existing ^ ".partial"))
    (fun () ->
      Alcotest.(check bool)
        "existing journal rejected" true
        (Result.is_error
           (T.Replay.run ~scenario_sha256:(demo_hash ()) ~journal_path:existing
              scenario)))

let journal_finalization_is_exclusive () =
  let path = Filename.temp_file "trading-engine-race" ".jsonl" in
  Sys.remove path;
  let partial = path ^ ".partial" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists path then Sys.remove path;
      if Sys.file_exists partial then Sys.remove partial)
    (fun () ->
      let journal = T.Journal.create path |> ok in
      Out_channel.with_open_bin path (fun channel ->
          output_string channel "rival\n");
      Alcotest.(check bool)
        "race rejected" true
        (Result.is_error (T.Journal.commit journal));
      Alcotest.(check string)
        "rival preserved" "rival\n"
        (In_channel.with_open_bin path In_channel.input_all);
      Alcotest.(check bool) "partial preserved" true (Sys.file_exists partial))

let failed_replay_preserves_partial () =
  let path = Filename.temp_file "trading-engine-failure" ".jsonl" in
  Sys.remove path;
  let partial = path ^ ".partial" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists path then Sys.remove path;
      if Sys.file_exists partial then Sys.remove partial)
    (fun () ->
      Alcotest.(check bool)
        "invalid hash fails after journal creation" true
        (Result.is_error
           (T.Replay.run ~scenario_sha256:"bad" ~journal_path:path (demo ())));
      Alcotest.(check bool) "final absent" false (Sys.file_exists path);
      Alcotest.(check bool) "partial retained" true (Sys.file_exists partial))

let journal_matches_in_memory_events () =
  let scenario = demo () in
  let path = Filename.temp_file "trading-engine" ".jsonl" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists path then Sys.remove path;
      if Sys.file_exists (path ^ ".partial") then Sys.remove (path ^ ".partial"))
    (fun () ->
      let result =
        T.Replay.run ~scenario_sha256:(demo_hash ()) ~journal_path:path scenario
        |> ok
      in
      let persisted = In_channel.with_open_bin path In_channel.input_all in
      let expected =
        result.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
        |> fun value -> value ^ "\n"
      in
      Alcotest.(check string) "journal contents" expected persisted;
      Alcotest.(check bool)
        "partial removed" false
        (Sys.file_exists (path ^ ".partial")))

let tests =
  [
    Alcotest.test_case "demo contract parses" `Quick demo_contract_parses;
    Alcotest.test_case "schema artifacts parse" `Quick schema_artifacts_parse;
    Alcotest.test_case "timestamp precision is bounded" `Quick
      timestamp_precision_is_bounded;
    Alcotest.test_case "unknown fields rejected" `Quick
      unknown_fields_are_rejected;
    Alcotest.test_case "contract version required and supported" `Quick
      contract_version_is_required_and_supported;
    Alcotest.test_case "duplicate fields rejected" `Quick
      duplicate_fields_are_rejected;
    Alcotest.test_case "metadata validation is recursive" `Quick
      recursive_metadata_validation;
    Alcotest.test_case "invalid schedule sequences rejected" `Quick
      invalid_schedule_sequences_are_rejected;
    Alcotest.test_case "duplicate slice bars rejected" `Quick
      duplicate_and_incomplete_slice_bars_are_rejected;
    Alcotest.test_case "portfolio target validation" `Quick
      portfolio_targets_are_total_and_aligned;
    Alcotest.test_case "deterministic replay" `Quick deterministic_replay;
    Alcotest.test_case "terminal completion summary" `Quick
      replay_ends_with_completion_summary;
    Alcotest.test_case "replay matches golden file" `Quick
      replay_matches_golden_file;
    Alcotest.test_case "exclusive journal creation" `Quick
      journal_is_created_exclusively;
    Alcotest.test_case "exclusive journal finalization" `Quick
      journal_finalization_is_exclusive;
    Alcotest.test_case "failed replay preserves partial" `Quick
      failed_replay_preserves_partial;
    Alcotest.test_case "journal matches events" `Quick
      journal_matches_in_memory_events;
  ]
