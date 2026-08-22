type context = {
  now : Ptime.t;
  portfolio : portfolio;
  working_orders : Order.t list;
  latest_bars : Bar.t Id.Instrument.Map.t;
}

and marked_position = {
  instrument_id : Id.Instrument.t;
  quantity : Scalar.Quantity.t;
  settled_quantity : Scalar.Quantity.t;
  unsettled_quantity : Scalar.Quantity.t;
  mark : Scalar.Price.t;
  base_market_value : Scalar.Money.t;
  weight : Scalar.Weight.t option;
}

and portfolio = {
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

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let weight ~equity value =
  if Scalar.Money.compare equity Scalar.Money.zero <= 0 then Ok None
  else Scalar.Money.weight_toward_zero value ~equity |> Result.map Option.some

let context ~now ~(valuation : Account.valuation) ~group_exposures
    ~working_orders ~latest_bars =
  let* cash_weight = weight ~equity:valuation.equity valuation.cash in
  let* positions =
    List.fold_left
      (fun result (position : Account.position_attribution) ->
        let* positions = result in
        let* position_weight =
          weight ~equity:valuation.equity position.base_market_value
        in
        Ok
          ({
             instrument_id = position.instrument_id;
             quantity = position.quantity;
             settled_quantity = position.settled_quantity;
             unsettled_quantity = position.unsettled_quantity;
             mark = position.mark;
             base_market_value = position.base_market_value;
             weight = position_weight;
           }
          :: positions))
      (Ok []) valuation.positions
    |> Result.map List.rev
  in
  let latest_bars =
    List.fold_left
      (fun result bar -> Id.Instrument.Map.add bar.Bar.instrument_id bar result)
      Id.Instrument.Map.empty latest_bars
  in
  let portfolio =
    {
      base_currency = valuation.base_currency;
      cash = valuation.cash;
      net_market_value = valuation.net_market_value;
      long_market_value = valuation.long_market_value;
      short_market_value = valuation.short_market_value;
      gross_exposure = valuation.gross_exposure;
      equity = valuation.equity;
      cash_weight;
      cash_balances = valuation.cash_balances;
      positions;
      group_exposures;
    }
  in
  Ok { now; portfolio; working_orders; latest_bars }

let now context = context.now
let portfolio context = context.portfolio
let cash context = context.portfolio.cash

let cash_balances context =
  List.map
    (fun (balance : Account.cash_attribution) ->
      (balance.currency, balance.amount))
    context.portfolio.cash_balances

let position context instrument_id =
  List.find_opt
    (fun (position : marked_position) ->
      Id.Instrument.equal position.instrument_id instrument_id)
    context.portfolio.positions
  |> Option.map (fun (position : marked_position) -> position.quantity)
  |> Option.value ~default:Scalar.Quantity.zero

let working_orders context = context.working_orders

let latest_bar context instrument_id =
  Id.Instrument.Map.find_opt instrument_id context.latest_bars

let group_exposures context = context.portfolio.group_exposures

module type S = sig
  type state

  val name : string
  val on_event : state -> context -> event -> state * intent list
end
