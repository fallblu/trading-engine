(** Bounded-memory reader for versioned JSON Lines replay scenarios. *)

val fold_channel :
  in_channel ->
  init:(Scenario.stream_header -> ('state, string) result) ->
  step:('state -> Scenario.stream_item -> ('state, string) result) ->
  finish:('state -> slice_count:int64 -> ('result, string) result) ->
  ('result, string) result

val fold_file :
  string ->
  init:(Scenario.stream_header -> ('state, string) result) ->
  step:('state -> Scenario.stream_item -> ('state, string) result) ->
  finish:('state -> slice_count:int64 -> ('result, string) result) ->
  ('result, string) result
