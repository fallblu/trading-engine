let version = "1"
let scenario_record_bytes = 1_048_576
let strategy_message_bytes = 1_048_576
let internal_events = 100_000
let catalog_instruments = 4_096
let intents_per_batch = 4_096
let artifact_record_bytes = 2_097_152

let to_yojson () =
  `Assoc
    [
      ("version", `String version);
      ("scenario_record_bytes", `Int scenario_record_bytes);
      ("strategy_message_bytes", `Int strategy_message_bytes);
      ("internal_events", `Int internal_events);
      ("catalog_instruments", `Int catalog_instruments);
      ("intents_per_batch", `Int intents_per_batch);
      ("artifact_record_bytes", `Int artifact_record_bytes);
    ]
