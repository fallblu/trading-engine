(** Append-only external-strategy protocol transcript. *)

type t

val create : string -> (t, Diagnostic.t) result

val append :
  t ->
  direction:Strategy_protocol.direction ->
  Yojson.Safe.t ->
  (unit, Diagnostic.t) result

val close_preserving_partial : t -> unit
val commit : t -> (unit, Diagnostic.t) result
