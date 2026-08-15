(** Deterministic signed-position, exposure, leverage, and margin checks. *)

type t

type margin_snapshot = private {
  initial_requirement : Scalar.Money.t;
  maintenance_requirement : Scalar.Money.t;
  initial_excess : Scalar.Money.t;
  maintenance_excess : Scalar.Money.t;
  margin_call : bool;
}

val create :
  base_currency:string ->
  instruments:Instrument.t list ->
  max_order_quantity:Scalar.Quantity.t ->
  max_long_position:Scalar.Quantity.t ->
  max_short_position:Scalar.Quantity.t ->
  max_gross_exposure:Scalar.Money.t ->
  max_leverage:Scalar.Ratio.t ->
  initial_margin_bps:int ->
  maintenance_margin_bps:int ->
  short_borrow_bps:int ->
  (t, string) result

val base_currency : t -> string
val instruments : t -> Instrument.t list
val instrument : t -> Id.Instrument.t -> Instrument.t option
val max_order_quantity : t -> Scalar.Quantity.t
val max_long_position : t -> Scalar.Quantity.t
val max_short_position : t -> Scalar.Quantity.t
val max_gross_exposure : t -> Scalar.Money.t
val max_leverage : t -> Scalar.Ratio.t
val initial_margin_bps : t -> int
val maintenance_margin_bps : t -> int
val short_borrow_bps : t -> int
val check_position : t -> Scalar.Quantity.t -> (unit, string) result
val margin_snapshot : t -> Account.valuation -> (margin_snapshot, string) result
val check_initial : t -> Account.valuation -> (unit, string) result

val check_post_fill :
  t ->
  before:Account.valuation ->
  after:Account.valuation ->
  (unit, string) result

val check :
  t ->
  account:Account.t ->
  oms:Oms.t ->
  marks:(Id.Instrument.t * Scalar.Price.t) list ->
  fx_rates:(string * Scalar.Price.t) list ->
  Order.request ->
  (unit, string) result
