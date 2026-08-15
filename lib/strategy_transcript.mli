(** Append-only external-strategy protocol transcript. *)

type t

val create : string -> (t, string) result

val append :
  t ->
  direction:Strategy_protocol.direction ->
  Yojson.Safe.t ->
  (unit, string) result

val close_preserving_partial : t -> unit
val commit : t -> (unit, string) result
