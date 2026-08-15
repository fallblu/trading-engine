(** Deterministic completed-bar execution simulation. *)

type t

type proposed_fill = private {
  order_id : Id.Order.t;
  quantity : Scalar.Quantity.t;
  price : Scalar.Price.t;
  fee : Scalar.Money.t;
  executed_at : Ptime.t;
}

type match_result = private {
  fills : proposed_fill list;
  market_ioc_orders : Id.Order.t list;
}

val create :
  participation_bps:int ->
  fixed_fee:Scalar.Money.t ->
  fee_bps:int ->
  (t, string) result

val participation_bps : t -> int
val fixed_fee : t -> Scalar.Money.t
val fee_bps : t -> int

val match_bar :
  t ->
  instrument:Instrument.t ->
  oms:Oms.t ->
  Bar.t ->
  (match_result, string) result
