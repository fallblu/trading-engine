(** Exact multi-currency cash, signed-position, cost-basis, and P&L accounting.
*)

type execution_fee_component = private {
  name : string;
  kind : string;
  currency : string;
  amount : Scalar.Money.t;
  quote_amount : Scalar.Money.t;
}

type execution_fee_component_attribution = private {
  name : string;
  kind : string;
  currency : string;
  amount : Scalar.Money.t;
  quote_currency : string;
  quote_amount : Scalar.Money.t;
  base_amount : Scalar.Money.t;
}

type position = private {
  quantity : Scalar.Quantity.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  dividend_pnl : Scalar.Money.t;
  execution_fees : Scalar.Money.t;
  borrow_fees : Scalar.Money.t;
  execution_fee_components : execution_fee_component list;
}

type cash_attribution = private {
  currency : string;
  amount : Scalar.Money.t;
  settled_amount : Scalar.Money.t;
  unsettled_amount : Scalar.Money.t;
  fx_rate : Scalar.Price.t;
  base_value : Scalar.Money.t;
  base_settled_value : Scalar.Money.t;
  base_unsettled_value : Scalar.Money.t;
  interest : Scalar.Money.t;
  base_interest : Scalar.Money.t;
}

type position_attribution = private {
  instrument_id : Id.Instrument.t;
  quote_currency : string;
  quantity : Scalar.Quantity.t;
  settled_quantity : Scalar.Quantity.t;
  unsettled_quantity : Scalar.Quantity.t;
  mark : Scalar.Price.t;
  fx_rate : Scalar.Price.t;
  market_value : Scalar.Money.t;
  base_market_value : Scalar.Money.t;
  cost_basis : Scalar.Money.t;
  base_cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  base_realized_pnl : Scalar.Money.t;
  unrealized_pnl : Scalar.Money.t;
  base_unrealized_pnl : Scalar.Money.t;
  dividend_pnl : Scalar.Money.t;
  base_dividend_pnl : Scalar.Money.t;
  execution_fees : Scalar.Money.t;
  base_execution_fees : Scalar.Money.t;
  borrow_fees : Scalar.Money.t;
  base_borrow_fees : Scalar.Money.t;
  total_fees : Scalar.Money.t;
  base_total_fees : Scalar.Money.t;
  execution_fee_components : execution_fee_component_attribution list;
}

type t

type valuation = private {
  base_currency : string;
  cash : Scalar.Money.t;
  settled_cash : Scalar.Money.t;
  unsettled_cash : Scalar.Money.t;
  net_market_value : Scalar.Money.t;
  long_market_value : Scalar.Money.t;
  short_market_value : Scalar.Money.t;
  gross_exposure : Scalar.Money.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  unrealized_pnl : Scalar.Money.t;
  equity : Scalar.Money.t;
  dividend_pnl : Scalar.Money.t;
  execution_fees : Scalar.Money.t;
  borrow_fees : Scalar.Money.t;
  cash_interest : Scalar.Money.t;
  total_fees : Scalar.Money.t;
  cash_balances : cash_attribution list;
  positions : position_attribution list;
  execution_fee_components : execution_fee_component_attribution list;
}

val create :
  base_currency:string ->
  initial_cash:(string * Scalar.Money.t) list ->
  (t, string) result

val of_initial_portfolio : Initial_portfolio.t -> (t, string) result
val base_currency : t -> string
val initial_cash : t -> (string * Scalar.Money.t) list
val cash_balances : t -> (string * Scalar.Money.t) list
val cash : t -> string -> Scalar.Money.t option
val settled_cash : t -> string -> Scalar.Money.t option
val position : t -> Id.Instrument.t -> position
val position_quantity : t -> Id.Instrument.t -> Scalar.Quantity.t
val settled_position_quantity : t -> Id.Instrument.t -> Scalar.Quantity.t
val positions : t -> (Id.Instrument.t * position) list
val apply_fill : t -> Fill.t -> (t, string) result
val apply_unsettled_fill : t -> Fill.t -> (t, string) result
val apply_settlement : t -> Settlement.instruction -> (t, string) result

val apply_split :
  t ->
  instrument_id:Id.Instrument.t ->
  numerator:int64 ->
  denominator:int64 ->
  (t, string) result

val apply_cash_dividend :
  t ->
  instrument_id:Id.Instrument.t ->
  quote_currency:string ->
  amount_per_unit:Scalar.Money.t ->
  (t, string) result

type distribution_result = {
  source_quantity : Scalar.Quantity.t;
  destination_quantity : Scalar.Quantity.t;
  fractional_quantity : Scalar.Quantity.t;
  allocated_basis : Scalar.Money.t;
  fractional_basis : Scalar.Money.t;
  cash_in_lieu : Scalar.Money.t;
}

val apply_distribution :
  t ->
  source_instrument_id:Id.Instrument.t ->
  destination_instrument_id:Id.Instrument.t ->
  destination_lot_size:Scalar.Quantity.t ->
  numerator:int64 ->
  denominator:int64 ->
  basis_allocation_bps:int ->
  fractional_policy:Corporate_action.fractional_policy ->
  (t * distribution_result, string) result

val cash_out_position :
  t ->
  instrument_id:Id.Instrument.t ->
  currency:string ->
  price:Scalar.Price.t ->
  (t * Scalar.Quantity.t * Scalar.Money.t, string) result

val apply_borrow_fee :
  t ->
  instrument_id:Id.Instrument.t ->
  quote_currency:string ->
  fee:Scalar.Money.t ->
  (t, string) result

val apply_cash_interest :
  t -> currency:string -> interest:Scalar.Money.t -> (t, string) result

val value :
  t ->
  instruments:Instrument.t list ->
  marks:(Id.Instrument.t * Scalar.Price.t) list ->
  fx_rates:(string * Scalar.Price.t) list ->
  (valuation, string) result
(** [value state ~instruments ~marks ~fx_rates] attributes every supplied mark
    and retained account position. Marks must cover positions with nonzero
    quantity. A retained flat position may omit its mark; its attribution then
    uses the canonical mark one. *)

val pp_valuation : Format.formatter -> valuation -> unit
