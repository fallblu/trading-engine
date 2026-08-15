(** Deterministic long-only pre-trade checks. *)

type t

val create :
  base_currency:string ->
  instruments:Instrument.t list ->
  max_order_quantity:Scalar.Quantity.t ->
  max_position:Scalar.Quantity.t ->
  (t, string) result

val base_currency : t -> string
val instruments : t -> Instrument.t list
val instrument : t -> Id.Instrument.t -> Instrument.t option

val check :
  t -> account:Account.t -> oms:Oms.t -> Order.request -> (unit, string) result
