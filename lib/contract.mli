(** Version and capability identifiers for the process/file boundary. *)

val version : string
val strategy_protocol_version : string
val engine_version : string
val capabilities_to_yojson : unit -> Yojson.Safe.t
val capabilities_to_string : unit -> string
