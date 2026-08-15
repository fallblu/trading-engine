(** Stable JSON codecs for public files and audit events. *)

val ptime_to_string : Ptime.t -> string
val ptime_of_string : string -> (Ptime.t, string) result
val market_slice_to_yojson : Market_slice.t -> Yojson.Safe.t
val audit_to_yojson : Audit.t -> Yojson.Safe.t
val audit_to_string : Audit.t -> string
