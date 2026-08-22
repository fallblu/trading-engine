let version = "15"
let previous_version = "14"
let legacy_journal_version = "3"

let supported_versions =
  [
    version;
    previous_version;
    "13";
    "12";
    "11";
    "10";
    "9";
    "8";
    "7";
    "6";
    "5";
    "4";
    legacy_journal_version;
  ]

let is_supported version = List.mem version supported_versions
let strategy_protocol_version = "13"
let previous_strategy_protocol_version = "12"
let engine_version = "1.0.0"
let strings values = `List (List.map (fun value -> `String value) values)

let capabilities_to_yojson () =
  `Assoc
    [
      ("engine_version", `String engine_version);
      ("scenario_contract_versions", strings supported_versions);
      ("journal_contract_versions", strings supported_versions);
      ("scenario_formats", strings [ "json"; "jsonl" ]);
      ("journal_formats", strings [ "jsonl" ]);
      ("execution_models", strings Execution_model.supported);
      ("execution_model_contracts", Execution_model.capabilities_to_yojson ());
      ( "strategy_protocol_versions",
        strings
          [
            strategy_protocol_version;
            previous_strategy_protocol_version;
            "11";
            "10";
            "9";
            "8";
            "7";
            "6";
            "5";
            "4";
            "3";
          ] );
      ("resource_limits", Resource_limits.to_yojson ());
    ]

let capabilities_to_string () =
  capabilities_to_yojson () |> Yojson.Safe.to_string
