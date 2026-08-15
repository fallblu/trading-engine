(** Exact multi-currency cash, signed-position, cost-basis, and P&L accounting.
*)

type position = private {
  quantity : Scalar.Quantity.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  dividend_pnl : Scalar.Money.t;
  execution_fees : Scalar.Money.t;
  borrow_fees : Scalar.Money.t;
}

type cash_attribution = private {
  currency : string;
  amount : Scalar.Money.t;
  fx_rate : Scalar.Price.t;
  base_value : Scalar.Money.t;
}

type position_attribution = private {
  instrument_id : Id.Instrument.t;
  quote_currency : string;
  quantity : Scalar.Quantity.t;
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
}

type t

type valuation = private {
  base_currency : string;
  cash : Scalar.Money.t;
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
  total_fees : Scalar.Money.t;
  cash_balances : cash_attribution list;
  positions : position_attribution list;
}

val create :
  base_currency:string ->
  initial_cash:(string * Scalar.Money.t) list ->
  (t, string) result

val base_currency : t -> string
val initial_cash : t -> (string * Scalar.Money.t) list
val cash_balances : t -> (string * Scalar.Money.t) list
val cash : t -> string -> Scalar.Money.t option
val position : t -> Id.Instrument.t -> position
val position_quantity : t -> Id.Instrument.t -> Scalar.Quantity.t
val positions : t -> (Id.Instrument.t * position) list
val apply_fill : t -> Fill.t -> (t, string) result

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

val apply_borrow_fee :
  t ->
  instrument_id:Id.Instrument.t ->
  quote_currency:string ->
  fee:Scalar.Money.t ->
  (t, string) result

val value :
  t ->
  instruments:Instrument.t list ->
  marks:(Id.Instrument.t * Scalar.Price.t) list ->
  fx_rates:(string * Scalar.Price.t) list ->
  (valuation, string) result

val pp_valuation : Format.formatter -> valuation -> unit
