(** Immutable point-in-time portfolio state used to start a replay. *)

type position = private {
  instrument_id : Id.Instrument.t;
  quantity : Scalar.Quantity.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  dividend_pnl : Scalar.Money.t;
  execution_fees : Scalar.Money.t;
  borrow_fees : Scalar.Money.t;
}

type t = private {
  base_currency : string;
  cash : (string * Scalar.Money.t) list;
  positions : position list;
  marks : (Id.Instrument.t * Scalar.Price.t) list;
  fx_rates : (string * Scalar.Price.t) list;
}

val position :
  instrument_id:Id.Instrument.t ->
  quantity:Scalar.Quantity.t ->
  cost_basis:Scalar.Money.t ->
  realized_pnl:Scalar.Money.t ->
  dividend_pnl:Scalar.Money.t ->
  execution_fees:Scalar.Money.t ->
  borrow_fees:Scalar.Money.t ->
  (position, string) result

val create :
  base_currency:string ->
  cash:(string * Scalar.Money.t) list ->
  positions:position list ->
  marks:(Id.Instrument.t * Scalar.Price.t) list ->
  fx_rates:(string * Scalar.Price.t) list ->
  (t, string) result

val cash_only :
  base_currency:string ->
  cash:(string * Scalar.Money.t) list ->
  (t, string) result
