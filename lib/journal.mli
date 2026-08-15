(** Append-only JSON Lines audit writer. This prototype flushes but does not
    fsync each event. *)

type t

val create : string -> (t, string) result
val append : t -> Audit.t -> (unit, string) result
val close : t -> unit
