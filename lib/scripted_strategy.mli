(** A deterministic strategy driven by intents scheduled after market slices. *)

type state

val create : (int64 * Strategy.intent list) list -> (state, string) result
val name : string

val on_event :
  state -> Strategy.context -> Strategy.event -> state * Strategy.intent list
