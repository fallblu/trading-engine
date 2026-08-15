(** One immutable execution report accepted by the order manager. *)

type t = private {
  id : Id.Fill.t;
  order_id : Id.Order.t;
  instrument_id : Id.Instrument.t;
  side : Order.side;
  quantity : Scalar.Quantity.t;
  price : Scalar.Price.t;
  notional : Scalar.Money.t;
  fee : Scalar.Money.t;
  executed_at : Ptime.t;
  bar_sequence : int64;
}

val create :
  id:Id.Fill.t ->
  order_id:Id.Order.t ->
  instrument_id:Id.Instrument.t ->
  side:Order.side ->
  quantity:Scalar.Quantity.t ->
  price:Scalar.Price.t ->
  fee:Scalar.Money.t ->
  executed_at:Ptime.t ->
  bar_sequence:int64 ->
  (t, string) result

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
