(** Append-only external-strategy protocol transcript. *)

type t

val max_rejection_prefix_bytes : int

val create :
  ?effects:Boundary_effects.t ->
  ?durability:Artifact_writer.durability ->
  string ->
  (t, Diagnostic.t) result

val append :
  t ->
  direction:Strategy_protocol.direction ->
  Yojson.Safe.t ->
  (unit, Diagnostic.t) result

val append_rejection :
  t ->
  expected_sequence:int64 ->
  diagnostic:Diagnostic.t ->
  raw_prefix:string ->
  observed_bytes:int ->
  truncated:bool ->
  (unit, Diagnostic.t) result

val close_preserving_partial : t -> unit
val commit : t -> (unit, Diagnostic.t) result
val artifact : t -> Artifact_writer.t
