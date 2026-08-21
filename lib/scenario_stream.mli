(** Bounded-memory reader for versioned JSON Lines replay scenarios. *)

val fold_channel :
  in_channel ->
  init:(Scenario.stream_header -> ('state, Diagnostic.t) result) ->
  step:('state -> Scenario.stream_item -> ('state, Diagnostic.t) result) ->
  finish:('state -> slice_count:int64 -> ('result, Diagnostic.t) result) ->
  ('result, Diagnostic.t) result

val fold_file :
  string ->
  init:(Scenario.stream_header -> ('state, Diagnostic.t) result) ->
  step:('state -> Scenario.stream_item -> ('state, Diagnostic.t) result) ->
  finish:('state -> slice_count:int64 -> ('result, Diagnostic.t) result) ->
  ('result, Diagnostic.t) result
