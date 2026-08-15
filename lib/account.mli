(** Exact long-only cash, position, cost-basis, and P&L accounting. *)

type position = private {
  quantity : Scalar.Quantity.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  total_fees : Scalar.Money.t;
}

type position_attribution = private {
  instrument_id : Id.Instrument.t;
  quantity : Scalar.Quantity.t;
  mark : Scalar.Price.t;
  market_value : Scalar.Money.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  unrealized_pnl : Scalar.Money.t;
  total_fees : Scalar.Money.t;
}

type t

type valuation = private {
  cash : Scalar.Money.t;
  market_value : Scalar.Money.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  unrealized_pnl : Scalar.Money.t;
  equity : Scalar.Money.t;
  total_fees : Scalar.Money.t;
  positions : position_attribution list;
}

val create : initial_cash:Scalar.Money.t -> t
val initial_cash : t -> Scalar.Money.t
val cash : t -> Scalar.Money.t
val realized_pnl : t -> Scalar.Money.t
val total_fees : t -> Scalar.Money.t
val position : t -> Id.Instrument.t -> position
val position_quantity : t -> Id.Instrument.t -> Scalar.Quantity.t
val positions : t -> (Id.Instrument.t * position) list
val apply_fill : t -> Fill.t -> (t, string) result

val value :
  t ->
  marks:(Id.Instrument.t * Scalar.Price.t) list ->
  (valuation, string) result

val pp_valuation : Format.formatter -> valuation -> unit
