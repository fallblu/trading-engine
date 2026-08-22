open Test_support
module T = Trading_engine

let demo_document () =
  In_channel.with_open_bin "../contracts/v15/fixtures/demo.scenario.json"
    In_channel.input_all

let demo () = T.Scenario.of_string (demo_document ()) |> ok
let demo_hash () = T.Sha256.digest_string (demo_document ())
let stream_path = "../contracts/v15/fixtures/demo.scenario.jsonl"
let quote_trade_path = "../contracts/v15/fixtures/quote-trade.scenario.json"

let quote_trade_stream_path =
  "../contracts/v15/fixtures/quote-trade.scenario.jsonl"

let order_book_path = "../contracts/v15/fixtures/order-book.scenario.json"

let order_book_stream_path =
  "../contracts/v15/fixtures/order-book.scenario.jsonl"

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

let with_stream_document document function_ =
  let path = Filename.temp_file "trading-engine-scenario" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      Out_channel.with_open_bin path (fun channel ->
          output_string channel document);
      function_ path)

let fold_stream_with_limit maximum path =
  In_channel.with_open_bin path (fun channel ->
      T.Scenario_stream.fold_channel ~max_record_bytes:maximum channel
        ~init:(fun _ -> Ok ())
        ~step:(fun () _ -> Ok ())
        ~finish:(fun () ~slice_count -> Ok slice_count))

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
          let start_at = add_seconds base offset in
          let borrow_observation =
            T.Financing.borrow_observation
              ~instrument_id:(instrument_id "demo-equity-acme")
              ~effective_at:start_at ~available_quantity:(quantity "1000")
              ~annual_rate_bps:100 ~recalled:false
            |> ok
          in
          let cash_rate =
            T.Financing.cash_rate_observation ~currency:"USD"
              ~effective_at:start_at ~credit_rate_bps:0 ~debit_rate_bps:0
            |> ok
          in
          T.Market_slice.create_v15 ~slice_sequence:(Int64.of_int index)
            ~start_at
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
            ~corporate_actions:[] ~borrow_observations:[ borrow_observation ]
            ~cash_rate_observations:[ cash_rate ] ~settlement_failures:[]
            ~lifecycle_events:[] ~market_events:[] ~order_book_events:[]
          |> ok
        in
        let payload =
          `Assoc
            [
              ("market_slice", T.Codec.market_slice_to_yojson_v15 market_slice);
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
    "execution model" "completed_bar_adverse_touch_v1"
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
  check_schema "../contracts/v15/scenario.schema.json";
  check_schema "../contracts/v15/scenario-stream.schema.json";
  check_schema "../contracts/v15/journal.schema.json"

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

let replace_assoc name value fields =
  (name, value) :: List.remove_assoc name fields

let v12_distributions_and_lifecycle_parse () =
  let source = instrument_id "demo-equity-acme" in
  let child = instrument_id "demo-equity-child" in
  let action name distribution_type destination fractional_policy =
    T.Corporate_action.distribution
      ~id:(T.Id.Corporate_action.of_string_exn name)
      ~instrument_id:source ~distribution_type
      ~destination_instrument_id:destination ~numerator:1L ~denominator:2L
      ~basis_allocation_bps:
        (if distribution_type = T.Corporate_action.Stock_dividend then 0
         else 2500)
      ~fractional_policy
    |> ok
  in
  let event name kind =
    T.Instrument_lifecycle.create_event
      ~id:(T.Id.Corporate_action.of_string_exn name)
      ~instrument_id:source ~kind
    |> ok
  in
  let market_slice =
    T.Market_slice.create_v15 ~slice_sequence:1L
      ~start_at:(timestamp "2026-01-02T14:30:00Z")
      ~end_at:(timestamp "2026-01-02T20:55:00Z")
      ~available_at:(timestamp "2026-01-02T21:00:00Z")
      ~received_at:(timestamp "2026-01-02T21:00:01Z")
      ~bars:[ bar ~instrument:source 1L; bar ~instrument:child 1L ]
      ~fx_rates:[ fx_mark () ]
      ~corporate_actions:
        [
          action "stock-action" T.Corporate_action.Stock_dividend source
            T.Corporate_action.Reject_fractional;
          action "rights-action" T.Corporate_action.Rights child
            (T.Corporate_action.Cash_in_lieu
               { price = price "12.5"; currency = "USD" });
          action "spinoff-action" T.Corporate_action.Spin_off child
            T.Corporate_action.Reject_fractional;
        ]
      ~borrow_observations:[] ~cash_rate_observations:[] ~settlement_failures:[]
      ~lifecycle_events:
        [
          event "rename-event"
            (T.Instrument_lifecycle.Identifier_change
               {
                 symbol = "ACME2";
                 provider = "sip";
                 provider_instrument_id = "ACME.X";
               });
          event "halt-event"
            (T.Instrument_lifecycle.Halt { reason = "regulatory" });
          event "resume-event" T.Instrument_lifecycle.Resume;
          event "expiration-event"
            (T.Instrument_lifecycle.Expiration
               { terminal_policy = T.Instrument_lifecycle.Hold });
          event "delisting-event"
            (T.Instrument_lifecycle.Delisting
               {
                 terminal_policy =
                   T.Instrument_lifecycle.Cash_out
                     { price = price "9"; currency = "USD" };
                 reason = "acquisition";
               });
        ]
      ~market_events:[] ~order_book_events:[]
    |> ok
  in
  let document =
    map_root (fun fields ->
        let instruments =
          match List.assoc "instruments" fields with
          | `List (`Assoc configured :: rest) ->
              let child_instrument =
                configured
                |> replace_assoc "instrument_id" (`String "demo-equity-child")
                |> replace_assoc "symbol" (`String "CHILD")
              in
              `List (`Assoc configured :: `Assoc child_instrument :: rest)
          | _ -> Alcotest.fail "demo instruments must be a list"
        in
        let risk =
          match List.assoc "risk" fields with
          | `Assoc risk_fields ->
              let policies =
                match List.assoc "instrument_policies" risk_fields with
                | `List (`Assoc configured :: rest) ->
                    let child_policy =
                      replace_assoc "instrument_id"
                        (`String "demo-equity-child") configured
                    in
                    `List (`Assoc configured :: `Assoc child_policy :: rest)
                | _ -> Alcotest.fail "demo risk policies must be a list"
              in
              `Assoc (replace_assoc "instrument_policies" policies risk_fields)
          | _ -> Alcotest.fail "demo risk must be an object"
        in
        let venue_calendars =
          match List.assoc "venue_calendars" fields with
          | `List [ `Assoc calendar ] ->
              `List
                [
                  `Assoc
                    (replace_assoc "instrument_ids"
                       (`List
                          [
                            `String "demo-equity-acme";
                            `String "demo-equity-child";
                          ])
                       calendar);
                ]
          | _ -> Alcotest.fail "demo venue calendars must be a singleton"
        in
        let execution =
          match List.assoc "execution" fields with
          | `Assoc execution_fields -> (
              match List.assoc "configuration" execution_fields with
              | `Assoc configuration ->
                  let schedules =
                    match List.assoc "fee_schedules" configuration with
                    | `List (`Assoc configured :: rest) ->
                        let child_schedule =
                          configured
                          |> replace_assoc "schedule_id"
                               (`String "demo-child-fees-v1")
                          |> replace_assoc "instrument_id"
                               (`String "demo-equity-child")
                        in
                        `List
                          (`Assoc configured :: `Assoc child_schedule :: rest)
                    | _ -> Alcotest.fail "demo fee schedules must be a list"
                  in
                  `Assoc
                    (replace_assoc "configuration"
                       (`Assoc
                          (replace_assoc "fee_schedules" schedules configuration))
                       execution_fields)
              | _ ->
                  Alcotest.fail "demo execution configuration must be an object"
              )
          | _ -> Alcotest.fail "demo execution must be an object"
        in
        let slices =
          match List.assoc "slices" fields with
          | `List (_ :: rest) ->
              let add_child_bar = function
                | `Assoc slice_fields -> (
                    match List.assoc "bars" slice_fields with
                    | `List (`Assoc configured :: bars) ->
                        let child_bar =
                          replace_assoc "instrument_id"
                            (`String "demo-equity-child") configured
                        in
                        `Assoc
                          (replace_assoc "bars"
                             (`List
                                (`Assoc configured :: `Assoc child_bar :: bars))
                             slice_fields)
                    | _ -> Alcotest.fail "demo slice bars must be nonempty")
                | _ -> Alcotest.fail "demo slice must be an object"
              in
              `List
                (T.Codec.market_slice_to_yojson_v15 market_slice
                :: List.map add_child_bar rest)
          | _ -> Alcotest.fail "demo slices must be nonempty"
        in
        let schedule =
          let add_child_target = function
            | `Assoc intent_fields as intent -> (
                match List.assoc_opt "targets" intent_fields with
                | Some (`List (`Assoc configured :: targets)) ->
                    let child_target =
                      configured
                      |> replace_assoc "instrument_id"
                           (`String "demo-equity-child")
                      |> fun fields ->
                      if List.mem_assoc "weight" fields then
                        replace_assoc "weight" (`String "0") fields
                      else replace_assoc "quantity" (`String "0") fields
                    in
                    `Assoc
                      (replace_assoc "targets"
                         (`List
                            (`Assoc configured :: `Assoc child_target :: targets))
                         intent_fields)
                | _ -> intent)
            | json -> json
          in
          match List.assoc "schedule" fields with
          | `List entries ->
              `List
                (List.map
                   (function
                     | `Assoc entry_fields -> (
                         match List.assoc "intents" entry_fields with
                         | `List intents ->
                             `Assoc
                               (replace_assoc "intents"
                                  (`List (List.map add_child_target intents))
                                  entry_fields)
                         | _ -> Alcotest.fail "schedule intents must be a list")
                     | _ -> Alcotest.fail "schedule entry must be an object")
                   entries)
          | _ -> Alcotest.fail "demo schedule must be a list"
        in
        fields
        |> replace_assoc "instruments" instruments
        |> replace_assoc "risk" risk
        |> replace_assoc "venue_calendars" venue_calendars
        |> replace_assoc "execution" execution
        |> replace_assoc "slices" slices
        |> replace_assoc "schedule" schedule)
    |> Yojson.Safe.to_string
  in
  let parsed =
    match T.Scenario.of_string document with
    | Ok value -> value
    | Error diagnostic -> Alcotest.fail (T.Diagnostic.to_human diagnostic)
  in
  let first = List.hd parsed.slices in
  Alcotest.(check int)
    "all distribution variants" 3
    (List.length first.corporate_actions);
  Alcotest.(check int)
    "all lifecycle variants" 5
    (List.length first.lifecycle_events)

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
    "unsupported scenario contract_version \"2\" (expected one of 15, 14, 13, \
     12, 11, 10, 9, 8, 7, 6, 5, 4, 3)"
    (T.Diagnostic.to_human unsupported_diagnostic);
  Alcotest.(check string)
    "unsupported version code" "scenario.unsupported_contract"
    (T.Diagnostic.code_to_string unsupported_diagnostic.code);
  Alcotest.(check (option string))
    "contract path" (Some "$.contract_version")
    unsupported_diagnostic.context.json_path

let duplicate_fields_are_rejected () =
  let changed =
    map_root (fun fields -> ("initial_portfolio", `String "0") :: fields)
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

let validation_layers_report_precise_context () =
  let document = Yojson.Safe.from_string (demo_document ()) in
  let instruments =
    match document with
    | `Assoc fields -> (
        match List.assoc "instruments" fields with
        | `List (instrument :: _) -> [ instrument; instrument ]
        | _ -> Alcotest.fail "demo instruments must be nonempty")
    | _ -> Alcotest.fail "demo must be an object"
  in
  let batch =
    change_field "instruments" (`List instruments) document
    |> T.Scenario.of_yojson |> error
  in
  Alcotest.(check string)
    "batch shared validation" "instrument IDs must be unique" batch.message;
  Alcotest.(check (option string))
    "batch semantic path" (Some "$.instruments") batch.context.json_path;
  let stream_header_payload =
    stream_records () |> List.hd |> Yojson.Safe.from_string |> function
    | `Assoc fields -> List.assoc "payload" fields
    | _ -> Alcotest.fail "stream header must be an object"
  in
  let stream_header =
    change_field "instruments" (`List instruments) stream_header_payload
    |> T.Scenario.stream_header_of_yojson ~contract_version:T.Contract.version
    |> error
  in
  Alcotest.(check string)
    "stream shares header semantics" batch.message stream_header.message;
  Alcotest.(check (option string))
    "stream semantic path" (Some "$.payload.instruments")
    stream_header.context.json_path;
  let malformed_stream =
    stream_records ()
    |> List.mapi (fun index line ->
        if index <> 1 then line
        else
          Yojson.Safe.from_string line
          |> map_field "payload"
               (map_field "intents" (function
                 | `List (`Assoc fields :: remaining) ->
                     `List
                       (`Assoc (("unexpected", `Bool true) :: fields)
                       :: remaining)
                 | _ -> Alcotest.fail "stream intents must be nonempty"))
          |> Yojson.Safe.to_string)
  in
  with_stream malformed_stream (fun path ->
      let diagnostic = T.Replay.run_stream path |> error in
      Alcotest.(check (option int))
        "stream record line" (Some 2) diagnostic.context.line;
      Alcotest.(check (option string))
        "stream item path" (Some "$.payload.intents[0]")
        diagnostic.context.json_path)

let dense_schedule_document slice_count =
  let base = timestamp "2026-02-01T00:00:00Z" in
  let slices =
    List.init slice_count (fun offset ->
        let index = offset + 1 in
        let time_offset = offset * 4 in
        let start_at = add_seconds base time_offset in
        let borrow_observation =
          T.Financing.borrow_observation
            ~instrument_id:(instrument_id "demo-equity-acme")
            ~effective_at:start_at ~available_quantity:(quantity "1000")
            ~annual_rate_bps:100 ~recalled:false
          |> ok
        in
        let cash_rate_observation =
          T.Financing.cash_rate_observation ~currency:"USD"
            ~effective_at:start_at ~credit_rate_bps:100 ~debit_rate_bps:200
          |> ok
        in
        T.Market_slice.create_v15 ~slice_sequence:(Int64.of_int index) ~start_at
          ~end_at:(add_seconds base (time_offset + 1))
          ~available_at:(add_seconds base (time_offset + 2))
          ~received_at:(add_seconds base (time_offset + 3))
          ~bars:
            [
              bar
                ~instrument:(instrument_id "demo-equity-acme")
                (Int64.of_int index);
            ]
          ~fx_rates:[ fx_mark () ]
          ~corporate_actions:[] ~borrow_observations:[ borrow_observation ]
          ~cash_rate_observations:[ cash_rate_observation ]
          ~settlement_failures:[] ~lifecycle_events:[] ~market_events:[]
          ~order_book_events:[]
        |> ok |> T.Codec.market_slice_to_yojson_v15)
  in
  let schedule =
    List.init slice_count (fun offset ->
        let sequence = offset + 1 in
        `Assoc
          [
            ("after_slice_sequence", `String (string_of_int sequence));
            ( "intents",
              `List
                [
                  `Assoc
                    [
                      ("type", `String "emit_metric");
                      ("name", `String "dense_schedule");
                      ("value", `String (string_of_int sequence));
                    ];
                ] );
          ])
  in
  map_root (fun fields ->
      List.map
        (fun (name, value) ->
          if String.equal name "slices" then (name, `List slices)
          else if String.equal name "schedule" then (name, `List schedule)
          else (name, value))
        fields)

let dense_batch_schedule_validation_scales () =
  let slice_count = 20_000 in
  let scenario =
    dense_schedule_document slice_count |> T.Scenario.of_yojson |> ok
  in
  Alcotest.(check int)
    "all slices retained" slice_count
    (List.length scenario.slices);
  Alcotest.(check int)
    "all schedule entries retained" slice_count
    (List.length scenario.schedule)

let configured_resources_are_bounded () =
  let document = Yojson.Safe.from_string (demo_document ()) in
  let check_limit expected_path changed =
    let diagnostic = T.Scenario.of_yojson changed |> error in
    Alcotest.(check string)
      "stable resource code" "resource.limit"
      (T.Diagnostic.code_to_string diagnostic.code);
    Alcotest.(check (option string))
      "resource path" (Some expected_path) diagnostic.context.json_path
  in
  check_limit "$.max_internal_events"
    (change_field "max_internal_events"
       (`Int (T.Resource_limits.internal_events + 1))
       document);
  let instrument =
    match document with
    | `Assoc fields -> (
        match List.assoc "instruments" fields with
        | `List (value :: _) -> value
        | _ -> Alcotest.fail "expected scenario instruments")
    | _ -> Alcotest.fail "expected scenario"
  in
  check_limit "$.instruments"
    (change_field "instruments"
       (`List
          (List.init (T.Resource_limits.catalog_instruments + 1) (fun _ ->
               instrument)))
       document);
  let schedule_item, intent =
    match document with
    | `Assoc fields -> (
        match List.assoc "schedule" fields with
        | `List ((`Assoc item_fields as item) :: _) -> (
            match List.assoc "intents" item_fields with
            | `List (intent :: _) -> (item, intent)
            | _ -> Alcotest.fail "expected scheduled intents")
        | _ -> Alcotest.fail "expected scenario schedule")
    | _ -> Alcotest.fail "expected scenario"
  in
  let oversized_item =
    change_field "intents"
      (`List
         (List.init (T.Resource_limits.intents_per_batch + 1) (fun _ -> intent)))
      schedule_item
  in
  check_limit "$.schedule[0].intents"
    (change_field "schedule" (`List [ oversized_item ]) document);
  Alcotest.(check bool)
    "reducer configuration limit" true
    (Result.is_error
       (T.Engine.config ~contract_version:T.Contract.version ~risk:(risk ())
          ~execution_model:(T.Execution_model.find "completed_bar_v1" |> ok)
          ~execution:(execution ())
          ~max_internal_events:(T.Resource_limits.internal_events + 1)))

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
    (T.Scenario.of_yojson duplicate |> diagnostic_message);
  let late_anchor =
    map_root (fun fields ->
        List.map
          (fun (name, json) ->
            if String.equal name "slices" then
              match json with
              | `List (first :: second :: rest) ->
                  ( name,
                    `List
                      (first
                      :: change_field "start_at"
                           (`String "2026-01-02T21:00:01Z") second
                      :: rest) )
              | _ -> Alcotest.fail "demo must contain at least two slices"
            else (name, json))
          fields)
  in
  Alcotest.(check string)
    "late anchor diagnosed"
    "scheduled order intent after slice 1 is received after the next \
     executable market slice starts"
    (T.Scenario.of_yojson late_anchor |> diagnostic_message)

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
            if String.equal name "initial_portfolio" then
              match value with
              | `Assoc portfolio_fields ->
                  let changed =
                    List.map
                      (fun (field, field_value) ->
                        if String.equal field "cash" then
                          match field_value with
                          | `List (`Assoc cash_fields :: rest) ->
                              let cash =
                                `Assoc
                                  (List.map
                                     (fun (cash_field, cash_value) ->
                                       if String.equal cash_field "amount" then
                                         (cash_field, `String "10000.0")
                                       else (cash_field, cash_value))
                                     cash_fields)
                              in
                              (field, `List (cash :: rest))
                          | _ -> (field, field_value)
                        else (field, field_value))
                      portfolio_fields
                  in
                  (name, `Assoc changed)
              | _ -> (name, value)
            else (name, value))
          fields)
  in
  Alcotest.(check bool)
    "noncanonical scalar rejected" true
    (Result.is_error (T.Scenario.of_yojson noncanonical))

let update_initial_portfolio field change =
  map_root (fun fields ->
      List.map
        (fun (name, value) ->
          if String.equal name "initial_portfolio" then
            match value with
            | `Assoc portfolio_fields ->
                ( name,
                  `Assoc
                    (List.map
                       (fun (key, item) ->
                         if String.equal key field then (key, change item)
                         else (key, item))
                       portfolio_fields) )
            | _ -> (name, value)
          else (name, value))
        fields)

let update_first_object_field field value = function
  | `List (`Assoc fields :: rest) ->
      `List
        (`Assoc
           (List.map
              (fun (name, current) ->
                if String.equal name field then (name, value)
                else (name, current))
              fields)
        :: rest)
  | _ -> Alcotest.fail "expected a nonempty object array"

let initial_portfolio_validation () =
  let signed_cash =
    update_initial_portfolio "cash"
      (update_first_object_field "amount" (`String "-1"))
  in
  Alcotest.(check bool)
    "signed cash accepted" true
    (Result.is_ok (T.Scenario.of_yojson signed_cash));
  let wrong_basis =
    update_initial_portfolio "positions"
      (update_first_object_field "cost_basis" (`String "-90"))
  in
  Alcotest.(check bool)
    "basis sign rejected" true
    (Result.is_error (T.Scenario.of_yojson wrong_basis));
  let off_lot =
    update_initial_portfolio "positions"
      (update_first_object_field "quantity" (`String "0.0005"))
  in
  Alcotest.(check bool)
    "off-lot holding rejected" true
    (Result.is_error (T.Scenario.of_yojson off_lot));
  let missing_mark = update_initial_portfolio "marks" (fun _ -> `List []) in
  Alcotest.(check bool)
    "missing initial mark rejected" true
    (Result.is_error (T.Scenario.of_yojson missing_mark));
  let insufficient_margin =
    update_initial_portfolio "cash"
      (update_first_object_field "amount" (`String "-100"))
  in
  Alcotest.(check bool)
    "initial margin enforced" true
    (Result.is_error (T.Scenario.of_yojson insufficient_margin))

let initial_portfolio_is_audited_and_reconciled () =
  let result = T.Replay.run ~scenario_sha256:(demo_hash ()) (demo ()) |> ok in
  match result.audits with
  | _started :: initial :: first_valuation :: _ -> (
      match (initial.event, first_valuation.event) with
      | ( T.Audit.Initial_state { portfolio; valuation = initial_valuation },
          T.Audit.Valuation first_valuation ) ->
          Alcotest.(check int) "one holding" 1 (List.length portfolio.positions);
          Alcotest.check money_testable "initial equity" (money "10100")
            initial_valuation.account.equity;
          Alcotest.check money_testable "first valuation reconciles"
            initial_valuation.account.equity first_valuation.account.equity;
          let position = List.hd initial_valuation.account.positions in
          Alcotest.check money_testable "native basis" (money "90")
            position.cost_basis;
          Alcotest.check money_testable "realized attribution" (money "5")
            position.realized_pnl;
          Alcotest.check money_testable "historical fees" (money "0.75")
            position.total_fees
      | _ -> Alcotest.fail "expected initial_state followed by valuation")
  | _ -> Alcotest.fail "expected initial audit records"

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
    (T.Scenario.of_yojson unsupported |> diagnostic_message);
  let change_configuration change =
    change_execution (map_field "configuration" change)
  in
  let missing_version =
    change_configuration (function
      | `Assoc fields ->
          `Assoc
            (List.filter
               (fun (name, _) -> not (String.equal name "version"))
               fields)
      | _ -> Alcotest.fail "configuration must be an object")
  in
  Alcotest.(check bool)
    "configuration version required" true
    (Result.is_error (T.Scenario.of_yojson missing_version));
  let unsupported_version =
    change_configuration (change_field "version" (`String "99"))
  in
  Alcotest.(check string)
    "unsupported model/version diagnosed"
    "unsupported execution configuration version \"99\" for model \
     \"completed_bar_adverse_touch_v1\""
    (T.Scenario.of_yojson unsupported_version |> diagnostic_message);
  let extra_configuration =
    change_configuration (function
      | `Assoc fields -> `Assoc (("future_parameter", `Int 1) :: fields)
      | _ -> Alcotest.fail "configuration must be an object")
  in
  Alcotest.(check bool)
    "model configuration is strict" true
    (Result.is_error (T.Scenario.of_yojson extra_configuration));
  let unsupported_spread =
    change_configuration
      (map_field "spread_model"
         (change_field "model" (`String "future_spread")))
  in
  Alcotest.(check string)
    "spread model is explicit" "unsupported spread model"
    (T.Scenario.of_yojson unsupported_spread |> diagnostic_message);
  let unsupported_impact =
    change_configuration
      (map_field "impact_model"
         (change_field "model" (`String "future_impact")))
  in
  Alcotest.(check string)
    "impact model is explicit" "unsupported impact model"
    (T.Scenario.of_yojson unsupported_impact |> diagnostic_message);
  let invalid_missing_volume =
    change_configuration
      (map_field "impact_model"
         (change_field "missing_volume_policy" (`String "estimate")))
  in
  Alcotest.(check string)
    "missing-volume policy is explicit"
    "missing_volume_policy must be reject or zero_impact"
    (T.Scenario.of_yojson invalid_missing_volume |> diagnostic_message);
  let zero_impact =
    change_configuration
      (map_field "impact_model"
         (change_field "missing_volume_policy" (`String "zero_impact")))
  in
  Alcotest.(check bool)
    "zero-impact policy parses" true
    (Result.is_ok (T.Scenario.of_yojson zero_impact));
  let invalid_spread_bps =
    change_configuration
      (map_field "spread_model" (change_field "half_spread_bps" (`Int 10_001)))
  in
  Alcotest.(check bool)
    "spread bound enforced" true
    (Result.is_error (T.Scenario.of_yojson invalid_spread_bps));
  let invalid_impact_bps =
    change_configuration
      (map_field "impact_model" (change_field "coefficient_bps" (`Int 10_001)))
  in
  Alcotest.(check bool)
    "impact bound enforced" true
    (Result.is_error (T.Scenario.of_yojson invalid_impact_bps))

let deterministic_replay () =
  let scenario = demo () in
  let hash = demo_hash () in
  let first = T.Replay.run ~scenario_sha256:hash scenario |> ok in
  let second = T.Replay.run ~scenario_sha256:hash scenario |> ok in
  let encode result = List.map T.Codec.audit_to_string result.T.Replay.audits in
  Alcotest.(check (list string))
    "byte-identical event encoding" (encode first) (encode second);
  Alcotest.(check int)
    "initial valuation plus one per slice" 5
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
    (cause_strings (event 10L));
  Alcotest.(check (list string))
    "target order cites slice and target request"
    [ "demo-event-000000000004"; "demo-event-000000000006" ]
    (cause_strings (event 8L));
  Alcotest.(check (list string))
    "price selection cites order creation and executable slice"
    [ "demo-event-000000000008"; "demo-event-000000000010" ]
    (cause_strings (event 12L));
  Alcotest.(check (list string))
    "fill cites price selection"
    [ "demo-event-000000000012" ]
    (cause_strings (event 13L));
  Alcotest.(check (list string))
    "completion cites terminal valuation"
    [ "demo-event-000000000028" ]
    (cause_strings (event 29L));
  match (event 8L).event with
  | T.Audit.Order_accepted order ->
      Alcotest.(check string)
        "order snapshot retains creation event"
        (T.Id.Event.to_string (event 8L).event_id)
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
      Alcotest.(check string)
        "start model" "completed_bar_adverse_touch_v1" execution_model
  | _ -> Alcotest.fail "expected run start");
  match completion.event with
  | T.Audit.Run_completed
      { scenario_sha256 = actual; execution_model; valuation; _ } ->
      Alcotest.(check string) "completion hash" hash actual;
      Alcotest.(check string)
        "completion model" "completed_bar_adverse_touch_v1" execution_model;
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
    In_channel.with_open_bin "../contracts/v15/fixtures/demo.journal.jsonl"
      In_channel.input_all
  in
  Alcotest.(check string) "stable audit contract" expected actual

let v3_replay_matches_frozen_golden_file () =
  let document =
    In_channel.with_open_bin "../contracts/v3/fixtures/demo.scenario.json"
      In_channel.input_all
  in
  let scenario = T.Scenario.of_string document |> ok in
  let result =
    T.Replay.run ~scenario_sha256:(T.Sha256.digest_string document) scenario
    |> ok
  in
  let actual =
    result.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
    |> fun value -> value ^ "\n"
  in
  let expected =
    In_channel.with_open_bin "../contracts/v3/fixtures/demo.journal.jsonl"
      In_channel.input_all
  in
  Alcotest.(check string) "frozen v3 audit contract" expected actual

let fill_clipping_fixture_reconciles () =
  let document =
    In_channel.with_open_bin
      "../contracts/v15/fixtures/fill-clipped.scenario.json"
      In_channel.input_all
  in
  let scenario = T.Scenario.of_string document |> ok in
  let result =
    T.Replay.run ~scenario_sha256:(T.Sha256.digest_string document) scenario
    |> ok
  in
  let actual =
    result.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
    |> fun value -> value ^ "\n"
  in
  let expected =
    In_channel.with_open_bin
      "../contracts/v15/fixtures/fill-clipped.journal.jsonl"
      In_channel.input_all
  in
  Alcotest.(check string) "fill clipping audit reconciliation" expected actual

let quote_trade_replay_is_causal_and_stream_equivalent () =
  let document =
    In_channel.with_open_bin quote_trade_path In_channel.input_all
  in
  let scenario = T.Scenario.of_string document |> ok in
  let batch =
    T.Replay.run ~scenario_sha256:(T.Sha256.digest_string document) scenario
    |> ok
  in
  let batch_journal =
    batch.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
    |> fun value -> value ^ "\n"
  in
  let golden =
    In_channel.with_open_bin
      "../contracts/v15/fixtures/quote-trade.journal.jsonl" In_channel.input_all
  in
  Alcotest.(check string) "quote/trade golden journal" golden batch_journal;
  let fills =
    List.filter_map
      (fun (audit : T.Audit.t) ->
        match audit.event with
        | T.Audit.Fill_applied fill -> Some fill
        | _ -> None)
      batch.audits
  in
  Alcotest.(check (list string))
    "only aggressor-qualified trade liquidity fills"
    [ "4@99@2026-02-03T14:33:00.000000Z"; "6@100@2026-02-03T14:34:00.000000Z" ]
    (List.map
       (fun (fill : T.Fill.t) ->
         Printf.sprintf "%s@%s@%s"
           (T.Scalar.Quantity.to_decimal_string fill.quantity)
           (T.Scalar.Price.to_decimal_string fill.price)
           (T.Codec.ptime_to_string fill.executed_at))
       fills);
  let stream_hash = T.Sha256.digest_file quote_trade_stream_path |> ok in
  let expected =
    T.Replay.run ~scenario_sha256:stream_hash scenario |> ok |> fun result ->
    result.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
    |> fun value -> value ^ "\n"
  in
  let journal = Filename.temp_file "trading-engine-quote-trade" ".jsonl" in
  Sys.remove journal;
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists journal then Sys.remove journal;
      if Sys.file_exists (journal ^ ".partial") then
        Sys.remove (journal ^ ".partial"))
    (fun () ->
      let streamed =
        T.Replay.run_stream ~journal_path:journal quote_trade_stream_path |> ok
      in
      Alcotest.(check int64) "two streamed slices" 2L streamed.slice_count;
      Alcotest.(check string)
        "quote/trade stream and batch journals agree" expected
        (In_channel.with_open_bin journal In_channel.input_all))

let order_book_replay_is_bounded_and_stream_equivalent () =
  let document =
    In_channel.with_open_bin order_book_path In_channel.input_all
  in
  let scenario = T.Scenario.of_string document |> ok in
  let batch =
    T.Replay.run ~scenario_sha256:(T.Sha256.digest_string document) scenario
    |> ok
  in
  let actual =
    batch.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
    |> fun value -> value ^ "\n"
  in
  let golden =
    In_channel.with_open_bin
      "../contracts/v15/fixtures/order-book.journal.jsonl" In_channel.input_all
  in
  Alcotest.(check string) "order-book golden journal" golden actual;
  let fills =
    List.filter_map
      (fun (audit : T.Audit.t) ->
        match audit.event with
        | T.Audit.Fill_applied fill -> Some fill
        | _ -> None)
      batch.audits
  in
  Alcotest.(check (list string))
    "queue reduction precedes deterministic partial maker fills"
    [ "4@100@2026-02-03T14:35:00.000000Z"; "6@100@2026-02-03T14:36:00.000000Z" ]
    (List.map
       (fun (fill : T.Fill.t) ->
         Printf.sprintf "%s@%s@%s"
           (T.Scalar.Quantity.to_decimal_string fill.quantity)
           (T.Scalar.Price.to_decimal_string fill.price)
           (T.Codec.ptime_to_string fill.executed_at))
       fills);
  let stream_hash = T.Sha256.digest_file order_book_stream_path |> ok in
  let expected =
    T.Replay.run ~scenario_sha256:stream_hash scenario |> ok |> fun result ->
    result.audits |> List.map T.Codec.audit_to_string |> String.concat "\n"
    |> fun value -> value ^ "\n"
  in
  let journal = Filename.temp_file "trading-engine-order-book" ".jsonl" in
  Sys.remove journal;
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists journal then Sys.remove journal;
      if Sys.file_exists (journal ^ ".partial") then
        Sys.remove (journal ^ ".partial"))
    (fun () ->
      let streamed =
        T.Replay.run_stream ~journal_path:journal order_book_stream_path |> ok
      in
      Alcotest.(check int64) "two streamed slices" 2L streamed.slice_count;
      Alcotest.(check string)
        "order-book stream and batch journals agree" expected
        (In_channel.with_open_bin journal In_channel.input_all))

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

let invalid_replay_configuration_precedes_artifacts () =
  let path = Filename.temp_file "trading-engine-failure" ".jsonl" in
  Sys.remove path;
  let partial = path ^ ".partial" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists path then Sys.remove path;
      if Sys.file_exists partial then Sys.remove partial)
    (fun () ->
      Alcotest.(check bool)
        "invalid hash is rejected" true
        (Result.is_error
           (T.Replay.run ~scenario_sha256:"bad" ~journal_path:path (demo ())));
      Alcotest.(check bool) "final absent" false (Sys.file_exists path);
      Alcotest.(check bool) "partial absent" false (Sys.file_exists partial))

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
      Alcotest.(check int64) "twenty-nine audits" 29L result.audit_count;
      Alcotest.check money_testable "same equity" (money "10111.979929")
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
            (map_field "market_slice" (fun market_slice ->
                 market_slice
                 |> change_field "start_at" (`String start_at)
                 |> map_field "borrow_observations" (function
                   | `List [ observation ] ->
                       `List
                         [
                           change_field "effective_at" (`String start_at)
                             observation;
                         ]
                   | value -> value)
                 |> map_field "cash_rate_observations" (function
                   | `List [ observation ] ->
                       `List
                         [
                           change_field "effective_at" (`String start_at)
                             observation;
                         ]
                   | value -> value)))
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

let scenario_stream_records_are_bounded () =
  let records = stream_records () in
  let maximum =
    List.fold_left
      (fun current line -> Int.max current (String.length line))
      0 records
  in
  with_stream records (fun path ->
      Alcotest.(check int64)
        "record at exact limit accepted" 4L
        (fold_stream_with_limit maximum path |> ok));
  let longest_line, longest_index =
    records
    |> List.mapi (fun index line -> (line, index + 1))
    |> List.fold_left
         (fun ((current, _) as selected) ((candidate, _) as next) ->
           if String.length candidate > String.length current then next
           else selected)
         ("", 0)
  in
  with_stream records (fun path ->
      let diagnostic = fold_stream_with_limit (maximum - 1) path |> error in
      Alcotest.(check string)
        "oversized record code" "resource.limit"
        (T.Diagnostic.code_to_string diagnostic.code);
      Alcotest.(check (option int))
        "oversized record line" (Some longest_index) diagnostic.context.line;
      Alcotest.(check string)
        "observed and allowed bytes"
        (Printf.sprintf "scenario stream record is %d bytes; limit is %d bytes"
           (String.length longest_line)
           (maximum - 1))
        diagnostic.message);
  with_stream_document (String.concat "\n" records) (fun path ->
      Alcotest.(check int64)
        "newline-free terminal record accepted" 4L
        (fold_stream_with_limit maximum path |> ok));
  let truncated = List.hd records ^ "\n{\"contract_version\"" in
  with_stream_document truncated (fun path ->
      let diagnostic = fold_stream_with_limit maximum path |> error in
      Alcotest.(check string)
        "truncated record remains a JSON error" "scenario.invalid_json"
        (T.Diagnostic.code_to_string diagnostic.code);
      Alcotest.(check (option int))
        "truncated record line" (Some 2) diagnostic.context.line);
  let small_limit = 32 in
  let newline_free = String.make (small_limit + 7) 'x' in
  with_stream_document newline_free (fun path ->
      let diagnostic = fold_stream_with_limit small_limit path |> error in
      Alcotest.(check string)
        "newline-free oversized code" "resource.limit"
        (T.Diagnostic.code_to_string diagnostic.code);
      Alcotest.(check string)
        "newline-free observed bytes"
        "scenario stream record is 39 bytes; limit is 32 bytes"
        diagnostic.message)

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
        (Int64.of_int ((2 * slice_count) + 4))
        result.audit_count;
      Alcotest.(check int) "no orders accumulated" 0 (List.length result.orders))

let tests =
  [
    Alcotest.test_case "demo contract parses" `Quick demo_contract_parses;
    Alcotest.test_case "v12 distributions and lifecycle parse" `Quick
      v12_distributions_and_lifecycle_parse;
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
    Alcotest.test_case "validation layers report precise context" `Quick
      validation_layers_report_precise_context;
    Alcotest.test_case "configured resources are bounded" `Quick
      configured_resources_are_bounded;
    Alcotest.test_case "invalid schedule sequences rejected" `Quick
      invalid_schedule_sequences_are_rejected;
    Alcotest.test_case "dense batch schedule validation" `Slow
      dense_batch_schedule_validation_scales;
    Alcotest.test_case "duplicate slice bars rejected" `Quick
      duplicate_and_incomplete_slice_bars_are_rejected;
    Alcotest.test_case "market slice timeline is non-overlapping" `Quick
      market_slice_timeline_is_non_overlapping;
    Alcotest.test_case "portfolio target validation" `Quick
      portfolio_targets_are_total_and_aligned;
    Alcotest.test_case "initial portfolio validation" `Quick
      initial_portfolio_validation;
    Alcotest.test_case "initial portfolio audit reconciliation" `Quick
      initial_portfolio_is_audited_and_reconciled;
    Alcotest.test_case "execution model required and supported" `Quick
      execution_model_is_required_and_supported;
    Alcotest.test_case "deterministic replay" `Quick deterministic_replay;
    Alcotest.test_case "audit IDs are deterministic and causal" `Quick
      audit_ids_are_deterministic_and_causal;
    Alcotest.test_case "terminal completion summary" `Quick
      replay_ends_with_completion_summary;
    Alcotest.test_case "replay matches golden file" `Quick
      replay_matches_golden_file;
    Alcotest.test_case "v3 replay matches frozen golden file" `Quick
      v3_replay_matches_frozen_golden_file;
    Alcotest.test_case "fill clipping fixture reconciles" `Quick
      fill_clipping_fixture_reconciles;
    Alcotest.test_case "quote/trade replay is causal and stream equivalent"
      `Quick quote_trade_replay_is_causal_and_stream_equivalent;
    Alcotest.test_case "order-book replay is bounded and stream equivalent"
      `Quick order_book_replay_is_bounded_and_stream_equivalent;
    Alcotest.test_case "exclusive journal creation" `Quick
      journal_is_created_exclusively;
    Alcotest.test_case "exclusive journal finalization" `Quick
      journal_finalization_is_exclusive;
    Alcotest.test_case "configuration precedes artifacts" `Quick
      invalid_replay_configuration_precedes_artifacts;
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
    Alcotest.test_case "scenario stream records are bounded" `Quick
      scenario_stream_records_are_bounded;
    Alcotest.test_case "large stream avoids retained audit history" `Slow
      large_stream_replay_does_not_retain_audit_history;
  ]
