(** Contract-selected resumable execution model modules.

    Models are compiled into an engine binary and selected by their stable
    contract name. A model returns an immutable matching cursor so the reducer
    can stop at strategy callbacks without losing priority or capacity state.
    Embedders can supply another module explicitly without changing the reducer.
*)

module type S = sig
  val name : string

  val start_slice :
    Execution.t ->
    instruments:Instrument.t list ->
    oms:Oms.t ->
    Market_slice.t ->
    (Execution.cursor, string) result
end

type t

val of_module : (module S) -> t
val name : t -> string
val find : string -> (t, string) result
val supported : string list

val start_slice :
  t ->
  Execution.t ->
  instruments:Instrument.t list ->
  oms:Oms.t ->
  Market_slice.t ->
  (Execution.cursor, string) result
