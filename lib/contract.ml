let version = "4"
let previous_version = "3"
let supported_versions = [ version; previous_version ]
let is_supported version = List.mem version supported_versions
let strategy_protocol_version = "3"
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
      ("strategy_protocol_versions", strings [ strategy_protocol_version ]);
    ]

let capabilities_to_string () =
  capabilities_to_yojson () |> Yojson.Safe.to_string
