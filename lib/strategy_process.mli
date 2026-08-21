(** Supervised stdio session for one external strategy process. *)

type t

val with_staged_session :
  ?effects:Boundary_effects.t ->
  env:Eio_unix.Stdenv.base ->
  command:string list ->
  timeout:float ->
  transcript:Strategy_transcript.t ->
  initialization:Strategy_protocol.initialization ->
  (t -> ('a, Diagnostic.t) result) ->
  ('a * Strategy_protocol.identity, Diagnostic.t) result

val with_session :
  ?effects:Boundary_effects.t ->
  ?durability:Artifact_writer.durability ->
  env:Eio_unix.Stdenv.base ->
  command:string list ->
  timeout:float ->
  transcript_path:string ->
  initialization:Strategy_protocol.initialization ->
  (t -> ('a, Diagnostic.t) result) ->
  ('a * Strategy_protocol.identity, Diagnostic.t) result

val on_event :
  t ->
  Strategy.context ->
  Strategy.event ->
  (Strategy.intent list, Diagnostic.t) result
