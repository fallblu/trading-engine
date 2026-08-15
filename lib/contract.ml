let version = "1"
let engine_version = "0.1.0-dev"
let strings values = `List (List.map (fun value -> `String value) values)

let capabilities_to_yojson () =
  `Assoc
    [
      ("engine_version", `String engine_version);
      ("scenario_contract_versions", strings [ version ]);
      ("journal_contract_versions", strings [ version ]);
      ("scenario_formats", strings [ "json"; "jsonl" ]);
      ("journal_formats", strings [ "jsonl" ]);
      ("execution_models", strings [ "completed_bar_v1" ]);
    ]

let capabilities_to_string () =
  capabilities_to_yojson () |> Yojson.Safe.to_string
