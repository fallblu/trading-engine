(** Pure strategy callback contract. *)

type context

type marked_position = private {
  instrument_id : Id.Instrument.t;
  quantity : Scalar.Quantity.t;
  settled_quantity : Scalar.Quantity.t;
  unsettled_quantity : Scalar.Quantity.t;
  mark : Scalar.Price.t;
  base_market_value : Scalar.Money.t;
  weight : Scalar.Weight.t option;
}

type portfolio = private {
  base_currency : string;
  cash : Scalar.Money.t;
  net_market_value : Scalar.Money.t;
  long_market_value : Scalar.Money.t;
  short_market_value : Scalar.Money.t;
  gross_exposure : Scalar.Money.t;
  equity : Scalar.Money.t;
  cash_weight : Scalar.Weight.t option;
  cash_balances : Account.cash_attribution list;
  positions : marked_position list;
  group_exposures : Risk.group_exposure list;
}

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
  | Emit_metric of Metric.t

val context :
  now:Ptime.t ->
  valuation:Account.valuation ->
  group_exposures:Risk.group_exposure list ->
  working_orders:Order.t list ->
  latest_bars:Bar.t list ->
  (context, string) result

val now : context -> Ptime.t
val portfolio : context -> portfolio
val cash : context -> Scalar.Money.t
val cash_balances : context -> (string * Scalar.Money.t) list
val position : context -> Id.Instrument.t -> Scalar.Quantity.t
val working_orders : context -> Order.t list
val latest_bar : context -> Id.Instrument.t -> Bar.t option
val group_exposures : context -> Risk.group_exposure list

module type S = sig
  type state

  val name : string
  val on_event : state -> context -> event -> state * intent list
end
