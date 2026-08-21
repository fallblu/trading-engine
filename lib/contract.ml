let version = "3"
let strategy_protocol_version = "3"
let engine_version = "1.0.0"
let strings values = `List (List.map (fun value -> `String value) values)

let capabilities_to_yojson () =
  `Assoc
    [
      ("engine_version", `String engine_version);
      ("scenario_contract_versions", strings [ version ]);
      ("journal_contract_versions", strings [ version ]);
      ("scenario_formats", strings [ "json"; "jsonl" ]);
      ("journal_formats", strings [ "jsonl" ]);
      ("execution_models", strings Execution_model.supported);
      ("strategy_protocol_versions", strings [ strategy_protocol_version ]);
      ("diagnostic_versions", strings [ Diagnostic.version ]);
    ]

let capabilities_to_string () =
  capabilities_to_yojson () |> Yojson.Safe.to_string
