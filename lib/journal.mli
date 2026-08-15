(** Append-only JSON Lines writer with atomic successful finalization. *)

type t

val create : string -> (t, string) result
val append : t -> Audit.t -> (unit, string) result
val close_preserving_partial : t -> unit
val commit : t -> (unit, string) result
