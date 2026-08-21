(** Exclusive staged-file writer with a typed internal lifecycle. *)

type t

val create :
  ?effects:Boundary_effects.t ->
  label:string ->
  string ->
  (t, Diagnostic.t) result

val append : t -> string -> (unit, Diagnostic.t) result
val close_preserving_partial : t -> unit

val commit : t list -> (unit, Diagnostic.t) result
(** Close, publish, and clean up every staged writer as one operation. *)
