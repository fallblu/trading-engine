(** Version and capability identifiers for the process/file boundary. *)

val version : string
val previous_version : string
val legacy_journal_version : string
val supported_versions : string list
val is_supported : string -> bool
val strategy_protocol_version : string
val previous_strategy_protocol_version : string
val engine_version : string
val capabilities_to_yojson : unit -> Yojson.Safe.t
val capabilities_to_string : unit -> string
