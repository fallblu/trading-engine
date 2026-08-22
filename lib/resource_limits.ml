let version = "1"
let scenario_record_bytes = 1_048_576
let scenario_stream_bytes = 1_073_741_824
let strategy_message_bytes = 1_048_576
let internal_events = 100_000
let catalog_instruments = 4_096
let intents_per_batch = 4_096
let artifact_record_bytes = 2_097_152
let metric_name_bytes = 128
let metric_string_value_bytes = 1_024
let metric_unit_bytes = 64
let metric_dimensions = 16
let metric_dimension_key_bytes = 64
let metric_dimension_value_bytes = 128

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
