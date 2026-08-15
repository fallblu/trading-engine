(** Convert target positions into deterministic market-order plans. *)

type plan = private {
  cancel_orders : Id.Order.t list;
  submit_order : Order.request option;
}

val target_position :
  account:Account.t ->
  oms:Oms.t ->
  instrument_id:Id.Instrument.t ->
  target:Scalar.Quantity.t ->
  (plan, string) result
