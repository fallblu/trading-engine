(** Contract-selected execution model modules.

    Models are compiled into an engine binary and selected by their stable
    contract name. Embedders can supply another module explicitly without
    changing the reducer. *)

module type S = sig
  val name : string

  val fold_slice :
    Execution.t ->
    instruments:Instrument.t list ->
    oms:Oms.t ->
    Market_slice.t ->
    init:'a ->
    apply:
      ('a -> Execution.proposed_fill -> ('a * Scalar.Quantity.t, string) result) ->
    ('a * Id.Order.t list, string) result
end

type t

val of_module : (module S) -> t
val name : t -> string
val find : string -> (t, string) result
val supported : string list

val fold_slice :
  t ->
  Execution.t ->
  instruments:Instrument.t list ->
  oms:Oms.t ->
  Market_slice.t ->
  init:'a ->
  apply:
    ('a -> Execution.proposed_fill -> ('a * Scalar.Quantity.t, string) result) ->
  ('a * Id.Order.t list, string) result
