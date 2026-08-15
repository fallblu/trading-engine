open Test_support
module T = Trading_engine

let demo () = T.Scenario.read_file "../examples/demo.json" |> ok

let demo_contract_parses () =
  let scenario = demo () in
  Alcotest.(check int) "schema" 1 scenario.schema_version;
  Alcotest.(check string) "run" "demo" (T.Id.Run.to_string scenario.run_id);
  Alcotest.(check int) "one instrument" 1 (List.length scenario.instruments);
  Alcotest.(check int) "four bars" 4 (List.length scenario.bars)

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
  check_schema "../schemas/scenario-v1.schema.json";
  check_schema "../schemas/journal-v1.schema.json"

let unknown_fields_are_rejected () =
  let document =
    In_channel.with_open_bin "../examples/demo.json" In_channel.input_all
  in
  let json = Yojson.Safe.from_string document in
  let changed =
    match json with
    | `Assoc fields -> `Assoc (("unexpected", `Bool true) :: fields)
    | _ -> Alcotest.fail "demo must be an object"
  in
  Alcotest.(check bool)
    "unknown field rejected" true
    (Result.is_error (T.Scenario.of_yojson changed))

let duplicate_fields_are_rejected () =
  let document =
    In_channel.with_open_bin "../examples/demo.json" In_channel.input_all
  in
  let json = Yojson.Safe.from_string document in
  let changed =
    match json with
    | `Assoc fields -> `Assoc (("initial_cash", `String "0") :: fields)
    | _ -> Alcotest.fail "demo must be an object"
  in
  let message = T.Scenario.of_yojson changed |> error in
  Alcotest.(check bool)
    "duplicate field diagnosed" true
    (String.starts_with ~prefix:"scenario has duplicate JSON fields" message)

let scenario_with_first_schedule_sequence value =
  let document =
    In_channel.with_open_bin "../examples/demo.json" In_channel.input_all
  in
  let replace_sequence = function
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (name, json) ->
               if String.equal name "after_bar_sequence" then
                 (name, `String value)
               else (name, json))
             fields)
    | _ -> Alcotest.fail "schedule item must be an object"
  in
  match Yojson.Safe.from_string document with
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (name, json) ->
             if String.equal name "schedule" then
               match json with
               | `List (first :: rest) ->
                   (name, `List (replace_sequence first :: rest))
               | _ -> Alcotest.fail "demo schedule must be nonempty"
             else (name, json))
           fields)
  | _ -> Alcotest.fail "demo must be an object"

let invalid_schedule_sequences_are_rejected () =
  let negative = scenario_with_first_schedule_sequence "-1" in
  Alcotest.(check bool)
    "negative sequence rejected during parsing" true
    (Result.is_error (T.Scenario.of_yojson negative));
  let missing = scenario_with_first_schedule_sequence "999" in
  let message = T.Scenario.of_yojson missing |> error in
  Alcotest.(check string)
    "missing sequence diagnosed"
    "scheduled intents refer to missing bar source sequence 999" message

let schedule_cannot_retroactively_change_next_open () =
  let document =
    In_channel.with_open_bin "../examples/demo.json" In_channel.input_all
  in
  let replace_start = function
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (name, json) ->
               if String.equal name "start_at" then
                 (name, `String "2026-01-02T21:00:00Z")
               else (name, json))
             fields)
    | _ -> Alcotest.fail "bar must be an object"
  in
  let changed =
    match Yojson.Safe.from_string document with
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (name, json) ->
               if String.equal name "bars" then
                 match json with
                 | `List (first :: second :: rest) ->
                     (name, `List (first :: replace_start second :: rest))
                 | _ -> Alcotest.fail "demo must have at least two bars"
               else (name, json))
             fields)
    | _ -> Alcotest.fail "demo must be an object"
  in
  let message = T.Scenario.of_yojson changed |> error in
  Alcotest.(check bool)
    "retroactive next-open change rejected" true
    (String.starts_with
       ~prefix:"scheduled order intent after bar 1 is received after" message)

let deterministic_replay () =
  let scenario = demo () in
  let first = T.Replay.run scenario |> ok in
  let second = T.Replay.run scenario |> ok in
  let encode result = List.map T.Codec.audit_to_string result.T.Replay.audits in
  Alcotest.(check (list string))
    "byte-identical event encoding" (encode first) (encode second);
  Alcotest.check money_testable "final cash" (money "9800.462")
    first.valuation.cash;
  Alcotest.check money_testable "final equity" (money "10012.462")
    first.valuation.equity;
  Alcotest.check money_testable "final realized" (money "6.751334")
    first.valuation.realized_pnl;
  Alcotest.check money_testable "final unrealized" (money "5.710666")
    first.valuation.unrealized_pnl;
  Alcotest.(check int) "seventeen events" 17 (List.length first.audits)

let replay_ends_with_completion_summary () =
  let result = T.Replay.run (demo ()) |> ok in
  let completion = List.rev result.audits |> List.hd in
  Alcotest.(check string)
    "terminal event" "run_completed"
    (T.Audit.event_name completion.event);
  Alcotest.(check string)
    "completion time uses final receipt" "2026-01-07T21:00:02.000000Z"
    (T.Codec.ptime_to_string completion.recorded_at);
  match completion.event with
  | T.Audit.Run_completed { valuation; order_counts } ->
      Alcotest.check money_testable "summary equity" result.valuation.equity
        valuation.equity;
      Alcotest.(check int) "total orders" 2 order_counts.total;
      Alcotest.(check int) "active orders" 0 order_counts.active;
      Alcotest.(check int) "filled orders" 1 order_counts.filled;
      Alcotest.(check int) "rejected orders" 0 order_counts.rejected;
      Alcotest.(check int) "cancelled orders" 1 order_counts.cancelled
  | _ -> Alcotest.fail "expected run completion payload"

let replay_matches_golden_file () =
  let result = T.Replay.run (demo ()) |> ok in
  let actual =
    result.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
    |> fun value -> value ^ "\n"
  in
  let expected =
    In_channel.with_open_bin "fixtures/demo.journal.jsonl" In_channel.input_all
  in
  Alcotest.(check string) "stable audit contract" expected actual

let journal_is_created_exclusively () =
  let scenario = demo () in
  let existing = Filename.temp_file "trading-engine" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists existing then Sys.remove existing)
    (fun () ->
      Alcotest.(check bool)
        "existing journal rejected" true
        (Result.is_error (T.Replay.run ~journal_path:existing scenario)))

let journal_matches_in_memory_events () =
  let scenario = demo () in
  let path = Filename.temp_file "trading-engine" ".jsonl" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let result = T.Replay.run ~journal_path:path scenario |> ok in
      let persisted = In_channel.with_open_bin path In_channel.input_all in
      let expected =
        result.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
        |> fun value -> value ^ "\n"
      in
      Alcotest.(check string) "journal contents" expected persisted)

let receipt_time_drives_audit_time () =
  let result = T.Replay.run (demo ()) |> ok in
  let first = List.hd result.audits in
  Alcotest.(check string)
    "recorded at receipt" "2026-01-02T21:00:02.000000Z"
    (T.Codec.ptime_to_string first.recorded_at)

let tests =
  [
    Alcotest.test_case "demo contract parses" `Quick demo_contract_parses;
    Alcotest.test_case "schema artifacts parse" `Quick schema_artifacts_parse;
    Alcotest.test_case "unknown fields rejected" `Quick
      unknown_fields_are_rejected;
    Alcotest.test_case "duplicate fields rejected" `Quick
      duplicate_fields_are_rejected;
    Alcotest.test_case "invalid schedule sequences rejected" `Quick
      invalid_schedule_sequences_are_rejected;
    Alcotest.test_case "schedule preserves next-open causality" `Quick
      schedule_cannot_retroactively_change_next_open;
    Alcotest.test_case "deterministic replay" `Quick deterministic_replay;
    Alcotest.test_case "terminal completion summary" `Quick
      replay_ends_with_completion_summary;
    Alcotest.test_case "replay matches golden file" `Quick
      replay_matches_golden_file;
    Alcotest.test_case "exclusive journal creation" `Quick
      journal_is_created_exclusively;
    Alcotest.test_case "journal matches events" `Quick
      journal_matches_in_memory_events;
    Alcotest.test_case "receipt time drives audit" `Quick
      receipt_time_drives_audit_time;
  ]
