(** Supervised stdio session for one external strategy process. *)

type t

val with_session :
  env:Eio_unix.Stdenv.base ->
  command:string list ->
  timeout:float ->
  transcript_path:string ->
  initialization:Strategy_protocol.initialization ->
  (t -> ('a, string) result) ->
  ('a * Strategy_protocol.identity, string) result

val on_event :
  t ->
  Strategy.context ->
  Strategy.event ->
  (Strategy.intent list, string) result
