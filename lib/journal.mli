(** Append-only JSON Lines writer with atomic successful finalization. *)

type t

val create : string -> (t, Diagnostic.t) result
val append : t -> Audit.t -> (unit, Diagnostic.t) result
val close_preserving_partial : t -> unit
val commit : t -> (unit, Diagnostic.t) result
