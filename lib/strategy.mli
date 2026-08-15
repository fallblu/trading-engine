(** Pure strategy callback contract. *)

type context

type event =
  | Market_slice_closed of Market_slice.t
  | Fill_received of Fill.t
  | Order_updated of Order.t
  | Intent_rejected of string

type weight_target = {
  instrument_id : Id.Instrument.t;
  weight : Scalar.Weight.t;
}

type quantity_target = {
  instrument_id : Id.Instrument.t;
  quantity : Scalar.Quantity.t;
}

type intent =
  | Target_weights of weight_target list
  | Target_quantities of quantity_target list
  | Submit_order of Order.request
  | Cancel_order of Id.Order.t
  | Emit_metric of { name : string; value : string }

val context :
  now:Ptime.t ->
  account:Account.t ->
  working_orders:Order.t list ->
  latest_bars:Bar.t list ->
  context

val now : context -> Ptime.t
val cash : context -> Scalar.Money.t
val cash_balances : context -> (string * Scalar.Money.t) list
val position : context -> Id.Instrument.t -> Scalar.Quantity.t
val working_orders : context -> Order.t list
val latest_bar : context -> Id.Instrument.t -> Bar.t option

module type S = sig
  type state

  val name : string
  val on_event : state -> context -> event -> state * intent list
end
