type context = {
  now : Ptime.t;
  account : Account.t;
  working_orders : Order.t list;
  latest_bars : Bar.t Id.Instrument.Map.t;
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
  | Emit_metric of { name : string; value : string }

let context ~now ~account ~working_orders ~latest_bars =
  let latest_bars =
    List.fold_left
      (fun result bar -> Id.Instrument.Map.add bar.Bar.instrument_id bar result)
      Id.Instrument.Map.empty latest_bars
  in
  { now; account; working_orders; latest_bars }

let now context = context.now
let cash context = Account.cash context.account

let position context instrument_id =
  Account.position_quantity context.account instrument_id

let working_orders context = context.working_orders

let latest_bar context instrument_id =
  Id.Instrument.Map.find_opt instrument_id context.latest_bars

module type S = sig
  type state

  val name : string
  val on_event : state -> context -> event -> state * intent list
end
