open Test_support
module T = Trading_engine

let demo_document () =
  In_channel.with_open_bin "../contracts/v3/fixtures/demo.scenario.json"
    In_channel.input_all

let demo () = T.Scenario.of_string (demo_document ()) |> ok
let demo_hash () = T.Sha256.digest_string (demo_document ())
let stream_path = "../contracts/v3/fixtures/demo.scenario.jsonl"

let stream_document () =
  In_channel.with_open_bin stream_path In_channel.input_all

let stream_records () =
  stream_document () |> String.split_on_char '\n'
  |> List.filter (fun line -> not (String.equal line ""))

let with_stream records function_ =
  let path = Filename.temp_file "trading-engine-scenario" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      Out_channel.with_open_bin path (fun channel ->
          output_string channel (String.concat "\n" records ^ "\n"));
      function_ path)

let add_seconds timestamp seconds =
  Ptime.add_span timestamp (Ptime.Span.of_int_s seconds) |> Option.get

let stream_record sequence record_type payload =
  `Assoc
    [
      ("contract_version", `String T.Contract.version);
      ("scenario_sequence", `String (Int64.to_string sequence));
      ("record_type", `String record_type);
      ("payload", payload);
    ]
  |> Yojson.Safe.to_string

let write_large_stream path slice_count =
  let header = List.hd (stream_records ()) in
  let base = timestamp "2026-02-01T00:00:00Z" in
  Out_channel.with_open_bin path (fun channel ->
      output_string channel (header ^ "\n");
      for index = 1 to slice_count do
        let offset = (index - 1) * 4 in
        let market_slice =
          T.Market_slice.create ~slice_sequence:(Int64.of_int index)
            ~start_at:(add_seconds base offset)
            ~end_at:(add_seconds base (offset + 1))
            ~available_at:(add_seconds base (offset + 2))
            ~received_at:(add_seconds base (offset + 3))
            ~bars:
              [
                bar
                  ~instrument:(instrument_id "demo-equity-acme")
                  (Int64.of_int index);
              ]
            ~fx_rates:[ fx_mark () ]
            ~corporate_actions:[]
          |> ok
        in
        let payload =
          `Assoc
            [
              ("market_slice", T.Codec.market_slice_to_yojson market_slice);
              ("intents", `List []);
            ]
        in
        stream_record (Int64.of_int (index + 1)) "market_slice" payload
        |> fun line -> output_string channel (line ^ "\n")
      done;
      stream_record
        (Int64.of_int (slice_count + 2))
        "scenario_end"
        (`Assoc [ ("slice_count", `String (string_of_int slice_count)) ])
      |> fun line -> output_string channel (line ^ "\n"))

let demo_contract_parses () =
  let scenario = demo () in
  Alcotest.(check string)
    "contract" T.Contract.version scenario.contract_version;
  Alcotest.(check string) "run" "demo" (T.Id.Run.to_string scenario.run_id);
  Alcotest.(check int) "one instrument" 1 (List.length scenario.instruments);
  Alcotest.(check int) "four slices" 4 (List.length scenario.slices);
  Alcotest.(check string)
    "execution model" "completed_bar_v1"
    (T.Execution_model.name scenario.execution_model);
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
  check_schema "../contracts/v3/scenario.schema.json";
  check_schema "../contracts/v3/scenario-stream.schema.json";
  check_schema "../contracts/v3/journal.schema.json"

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
  let unsupported_diagnostic = T.Scenario.of_yojson unsupported |> error in
  Alcotest.(check string)
    "unsupported version diagnosed"
    "unsupported scenario contract_version \"2\" (expected \"3\")"
    (T.Diagnostic.to_human unsupported_diagnostic);
  Alcotest.(check string)
    "unsupported version code" "scenario.unsupported_contract"
    (T.Diagnostic.code_to_string unsupported_diagnostic.code);
  Alcotest.(check (option string))
    "contract path" (Some "$.contract_version")
    unsupported_diagnostic.context.json_path

let duplicate_fields_are_rejected () =
  let changed =
    map_root (fun fields -> ("initial_cash", `String "0") :: fields)
  in
  let message = T.Scenario.of_yojson changed |> diagnostic_message in
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

let map_field key change = function
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (name, json) ->
             if String.equal name key then (name, change json) else (name, json))
           fields)
  | _ -> Alcotest.fail "expected object"

let scenario_with_second_slice_start start_at =
  map_root (fun fields ->
      List.map
        (fun (name, json) ->
          if String.equal name "schedule" then (name, `List [])
          else if String.equal name "slices" then
            match json with
            | `List (first :: second :: rest) ->
                ( name,
                  `List
                    (first
                    :: change_field "start_at" (`String start_at) second
                    :: rest) )
            | _ -> Alcotest.fail "demo must contain at least two slices"
          else (name, json))
        fields)

let market_slice_timeline_is_non_overlapping () =
  List.iter
    (fun (label, start_at) ->
      Alcotest.(check string)
        label "market slice start must not precede previous end"
        (scenario_with_second_slice_start start_at
        |> T.Scenario.of_yojson |> diagnostic_message))
    [
      ("backward start rejected", "2026-01-01T14:30:00Z");
      ("overlapping start rejected", "2026-01-02T20:00:00Z");
    ];
  Alcotest.(check bool)
    "equal boundary accepted" true
    (Result.is_ok
       (scenario_with_second_slice_start "2026-01-02T21:00:00Z"
       |> T.Scenario.of_yojson))

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
  let message = T.Scenario.of_yojson missing |> diagnostic_message in
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
    (T.Scenario.of_yojson duplicate |> diagnostic_message)

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

let execution_model_is_required_and_supported () =
  let change_execution change =
    map_root (fun fields ->
        List.map
          (fun (name, value) ->
            if String.equal name "execution" then (name, change value)
            else (name, value))
          fields)
  in
  let missing =
    change_execution (function
      | `Assoc fields ->
          `Assoc
            (List.filter
               (fun (name, _) -> not (String.equal name "model"))
               fields)
      | _ -> Alcotest.fail "execution must be an object")
  in
  Alcotest.(check bool)
    "missing model rejected" true
    (Result.is_error (T.Scenario.of_yojson missing));
  let unsupported =
    change_execution (change_field "model" (`String "future_model"))
  in
  Alcotest.(check string)
    "unsupported model diagnosed" "unsupported execution model \"future_model\""
    (T.Scenario.of_yojson unsupported |> diagnostic_message)

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

let audit_ids_are_deterministic_and_causal () =
  let result = T.Replay.run ~scenario_sha256:(demo_hash ()) (demo ()) |> ok in
  let seen = ref T.Id.Event.Set.empty in
  List.iter
    (fun audit ->
      let expected =
        T.Audit.event_id ~run_id:audit.T.Audit.run_id
          ~engine_sequence:audit.engine_sequence
      in
      Alcotest.(check string)
        "event ID derives from run and sequence"
        (T.Id.Event.to_string expected)
        (T.Id.Event.to_string audit.event_id);
      Alcotest.(check bool)
        "event ID is unique" false
        (T.Id.Event.Set.mem audit.event_id !seen);
      Alcotest.(check (list string))
        "causes are canonical"
        (List.sort_uniq T.Id.Event.compare audit.causation_ids
        |> List.map T.Id.Event.to_string)
        (List.map T.Id.Event.to_string audit.causation_ids);
      List.iter
        (fun cause ->
          Alcotest.(check bool)
            "cause is a prior event" true
            (T.Id.Event.Set.mem cause !seen))
        audit.causation_ids;
      seen := T.Id.Event.Set.add audit.event_id !seen)
    result.audits;
  let event sequence =
    List.find
      (fun audit -> Int64.equal audit.T.Audit.engine_sequence sequence)
      result.audits
  in
  let cause_strings audit =
    List.map T.Id.Event.to_string audit.T.Audit.causation_ids
  in
  Alcotest.(check (list string))
    "external slice has no engine cause" []
    (cause_strings (event 7L));
  Alcotest.(check (list string))
    "target order cites slice and target request"
    [ "demo-event-000000000002"; "demo-event-000000000003" ]
    (cause_strings (event 5L));
  Alcotest.(check (list string))
    "fill cites order creation and executable slice"
    [ "demo-event-000000000005"; "demo-event-000000000007" ]
    (cause_strings (event 8L));
  Alcotest.(check (list string))
    "completion cites terminal valuation"
    [ "demo-event-000000000019" ]
    (cause_strings (event 20L));
  match (event 5L).event with
  | T.Audit.Order_accepted order ->
      Alcotest.(check string)
        "order snapshot retains creation event"
        (T.Id.Event.to_string (event 5L).event_id)
        (T.Id.Event.to_string order.created_event_id)
  | _ -> Alcotest.fail "expected accepted order"

let replay_ends_with_completion_summary () =
  let hash = demo_hash () in
  let result = T.Replay.run ~scenario_sha256:hash (demo ()) |> ok in
  let first = List.hd result.audits in
  let completion = List.rev result.audits |> List.hd in
  Alcotest.(check string)
    "journal contract" T.Contract.version first.contract_version;
  (match first.event with
  | T.Audit.Run_started { scenario_sha256 = actual; execution_model } ->
      Alcotest.(check string) "start hash" hash actual;
      Alcotest.(check string) "start model" "completed_bar_v1" execution_model
  | _ -> Alcotest.fail "expected run start");
  match completion.event with
  | T.Audit.Run_completed
      { scenario_sha256 = actual; execution_model; valuation; _ } ->
      Alcotest.(check string) "completion hash" hash actual;
      Alcotest.(check string)
        "completion model" "completed_bar_v1" execution_model;
      Alcotest.check money_testable "summary equity" result.valuation.equity
        valuation.account.equity
  | _ -> Alcotest.fail "expected run completion payload"

let replay_matches_golden_file () =
  let result = T.Replay.run ~scenario_sha256:(demo_hash ()) (demo ()) |> ok in
  let actual =
    result.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
    |> fun value -> value ^ "\n"
  in
  let expected =
    In_channel.with_open_bin "../contracts/v3/fixtures/demo.journal.jsonl"
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

let streamed_replay_matches_batch_semantics () =
  let scenario_sha256 = T.Sha256.digest_file stream_path |> ok in
  let expected =
    T.Replay.run ~scenario_sha256 (demo ()) |> ok |> fun result ->
    result.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
    |> fun value -> value ^ "\n"
  in
  let journal = Filename.temp_file "trading-engine-stream" ".jsonl" in
  Sys.remove journal;
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists journal then Sys.remove journal;
      if Sys.file_exists (journal ^ ".partial") then
        Sys.remove (journal ^ ".partial"))
    (fun () ->
      let result =
        T.Replay.run_stream ~journal_path:journal stream_path |> ok
      in
      Alcotest.(check int64) "four streamed slices" 4L result.slice_count;
      Alcotest.(check int64) "two schedule batches" 2L result.schedule_count;
      Alcotest.(check int) "one instrument" 1 result.instrument_count;
      Alcotest.(check int64) "twenty audits" 20L result.audit_count;
      Alcotest.check money_testable "same equity" (money "10004.76812")
        result.valuation.equity;
      Alcotest.(check string)
        "stream and batch journals agree" expected
        (In_channel.with_open_bin journal In_channel.input_all))

let streamed_contract_requires_ordered_terminal_records () =
  let records = stream_records () in
  let truncated = List.rev records |> List.tl |> List.rev in
  with_stream truncated (fun path ->
      let journal =
        Filename.temp_file "trading-engine-invalid-stream" ".jsonl"
      in
      Sys.remove journal;
      Fun.protect
        ~finally:(fun () ->
          if Sys.file_exists journal then Sys.remove journal;
          if Sys.file_exists (journal ^ ".partial") then
            Sys.remove (journal ^ ".partial"))
        (fun () ->
          Alcotest.(check string)
            "truncation diagnosed"
            "scenario_end must terminate the scenario stream"
            (T.Replay.run_stream ~journal_path:journal path
            |> diagnostic_message);
          Alcotest.(check bool)
            "invalid stream has no journal" false (Sys.file_exists journal);
          Alcotest.(check bool)
            "invalid stream has no partial journal" false
            (Sys.file_exists (journal ^ ".partial"))));
  let skipped =
    List.mapi
      (fun index line ->
        if index = 2 then
          Yojson.Safe.from_string line
          |> change_field "scenario_sequence" (`String "9")
          |> Yojson.Safe.to_string
        else line)
      records
  in
  with_stream skipped (fun path ->
      let diagnostic = T.Replay.run_stream path |> error in
      Alcotest.(check string)
        "sequence gap diagnosed"
        "scenario_sequence must be contiguous and start at one"
        (T.Diagnostic.to_human diagnostic);
      Alcotest.(check string)
        "stream diagnostic code" "scenario_stream.invalid"
        (T.Diagnostic.code_to_string diagnostic.code);
      Alcotest.(check (option int))
        "record line" (Some 3) diagnostic.context.line;
      Alcotest.(check (option int64))
        "observed sequence" (Some 9L) diagnostic.context.sequence;
      Alcotest.(check (option string))
        "sequence path" (Some "$.scenario_sequence")
        diagnostic.context.json_path)

let stream_with_second_slice_start start_at =
  stream_records ()
  |> List.mapi (fun index line ->
      let record = Yojson.Safe.from_string line in
      let changed =
        if index = 1 then
          map_field "payload" (change_field "intents" (`List [])) record
        else if index = 2 then
          map_field "payload"
            (map_field "market_slice"
               (change_field "start_at" (`String start_at)))
            record
        else record
      in
      Yojson.Safe.to_string changed)

let streamed_market_slice_timeline_is_non_overlapping () =
  List.iter
    (fun (label, start_at) ->
      with_stream (stream_with_second_slice_start start_at) (fun path ->
          Alcotest.(check string)
            label "market slice start must not precede previous end"
            (T.Replay.run_stream path |> diagnostic_message)))
    [
      ("backward start rejected", "2026-01-01T14:30:00Z");
      ("overlapping start rejected", "2026-01-02T20:00:00Z");
    ];
  with_stream (stream_with_second_slice_start "2026-01-02T21:00:00Z")
    (fun path ->
      Alcotest.(check bool)
        "equal boundary accepted" true
        (Result.is_ok (T.Replay.run_stream path)))

let streamed_intents_are_causal_before_execution () =
  let changed =
    stream_records ()
    |> List.mapi (fun index line ->
        if index = 2 then
          Yojson.Safe.from_string line
          |> map_field "payload"
               (map_field "market_slice"
                  (change_field "start_at" (`String "2026-01-02T21:00:01Z")))
          |> Yojson.Safe.to_string
        else line)
  in
  with_stream changed (fun path ->
      Alcotest.(check string)
        "lookahead intent rejected"
        "scheduled order intent after slice 1 is received after the next \
         executable market slice starts"
        (T.Replay.run_stream path |> diagnostic_message))

let large_stream_replay_does_not_retain_audit_history () =
  let slice_count = 10_000 in
  let path = Filename.temp_file "trading-engine-large" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      write_large_stream path slice_count;
      let result = T.Replay.run_stream path |> ok in
      Alcotest.(check int64)
        "all slices consumed" (Int64.of_int slice_count) result.slice_count;
      Alcotest.(check int64)
        "events counted without an audit list"
        (Int64.of_int ((2 * slice_count) + 2))
        result.audit_count;
      Alcotest.(check int) "no orders accumulated" 0 (List.length result.orders))

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
    Alcotest.test_case "market slice timeline is non-overlapping" `Quick
      market_slice_timeline_is_non_overlapping;
    Alcotest.test_case "portfolio target validation" `Quick
      portfolio_targets_are_total_and_aligned;
    Alcotest.test_case "execution model required and supported" `Quick
      execution_model_is_required_and_supported;
    Alcotest.test_case "deterministic replay" `Quick deterministic_replay;
    Alcotest.test_case "audit IDs are deterministic and causal" `Quick
      audit_ids_are_deterministic_and_causal;
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
    Alcotest.test_case "stream replay matches batch semantics" `Quick
      streamed_replay_matches_batch_semantics;
    Alcotest.test_case "stream requires ordered terminal records" `Quick
      streamed_contract_requires_ordered_terminal_records;
    Alcotest.test_case "streamed market slice timeline is non-overlapping"
      `Quick streamed_market_slice_timeline_is_non_overlapping;
    Alcotest.test_case "streamed intents are causal" `Quick
      streamed_intents_are_causal_before_execution;
    Alcotest.test_case "large stream avoids retained audit history" `Slow
      large_stream_replay_does_not_retain_audit_history;
  ]
