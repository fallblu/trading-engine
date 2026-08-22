(** Versioned inclusive limits for boundary and reducer resources.

    Scenario record bytes exclude the line feed. Artifact record bytes include
    it. *)

val version : string
val scenario_record_bytes : int
val strategy_message_bytes : int
val internal_events : int
val catalog_instruments : int
val intents_per_batch : int
val artifact_record_bytes : int
val metric_name_bytes : int
val metric_string_value_bytes : int
val metric_unit_bytes : int
val metric_dimensions : int
val metric_dimension_key_bytes : int
val metric_dimension_value_bytes : int
val to_yojson : unit -> Yojson.Safe.t
