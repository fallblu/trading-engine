(** Deterministic signed-position, exposure, leverage, and margin checks. *)

type t

type instrument_policy = private {
  instrument_id : Id.Instrument.t;
  max_order_quantity : Scalar.Quantity.t;
  max_long_position : Scalar.Quantity.t;
  max_short_position : Scalar.Quantity.t;
  max_notional_exposure : Scalar.Money.t option;
  initial_margin_bps : int;
  maintenance_margin_bps : int;
  shorting_allowed : bool;
}

type group_kind = Issuer | Sector | Currency | Country | Asset_class | Custom

type group_limits = private {
  max_gross_exposure : Scalar.Money.t option;
  max_long_exposure : Scalar.Money.t option;
  max_short_exposure : Scalar.Money.t option;
  max_absolute_net_exposure : Scalar.Money.t option;
  max_concentration : Scalar.Ratio.t option;
}

type group = private {
  group_id : Id.Risk_group.t;
  group_kind : group_kind;
  instrument_ids : Id.Instrument.t list;
  limits : group_limits;
}

type group_exposure = private {
  group_id : Id.Risk_group.t;
  gross_exposure : Scalar.Money.t;
  net_exposure : Scalar.Money.t;
  long_exposure : Scalar.Money.t;
  short_exposure : Scalar.Money.t;
  concentration : Scalar.Weight.t option;
}

type margin_snapshot = private {
  initial_requirement : Scalar.Money.t;
  maintenance_requirement : Scalar.Money.t;
  initial_excess : Scalar.Money.t;
  maintenance_excess : Scalar.Money.t;
  margin_call : bool;
  group_exposures : group_exposure list;
}

type fill_limit =
  | Maximum_order_quantity of Scalar.Quantity.t
  | Maximum_long_position of Scalar.Quantity.t
  | Maximum_short_position of Scalar.Quantity.t
  | Maximum_gross_exposure of Scalar.Money.t
  | Maximum_leverage of Scalar.Ratio.t
  | Initial_margin of int
  | Instrument_maximum_long_position of Id.Instrument.t * Scalar.Quantity.t
  | Instrument_maximum_short_position of Id.Instrument.t * Scalar.Quantity.t
  | Instrument_maximum_notional of Id.Instrument.t * Scalar.Money.t
  | Instrument_shorting_disabled of Id.Instrument.t
  | Instrument_borrow_availability of Id.Instrument.t * Scalar.Quantity.t
  | Settlement_cash_buying_power of string * Scalar.Money.t
  | Settlement_position_availability of Id.Instrument.t * Scalar.Quantity.t
  | Instrument_initial_margin of Id.Instrument.t * int
  | Group_maximum_gross of Id.Risk_group.t * Scalar.Money.t
  | Group_maximum_long of Id.Risk_group.t * Scalar.Money.t
  | Group_maximum_short of Id.Risk_group.t * Scalar.Money.t
  | Group_maximum_absolute_net of Id.Risk_group.t * Scalar.Money.t
  | Group_maximum_concentration of Id.Risk_group.t * Scalar.Ratio.t

type fill_check_error = Limit of fill_limit | Invalid of string

val create :
  base_currency:string ->
  instruments:Instrument.t list ->
  instrument_policies:instrument_policy list ->
  groups:group list ->
  max_gross_exposure:Scalar.Money.t ->
  max_leverage:Scalar.Ratio.t ->
  (t, string) result

val create_instrument_policy :
  instrument:Instrument.t ->
  max_order_quantity:Scalar.Quantity.t ->
  max_long_position:Scalar.Quantity.t ->
  max_short_position:Scalar.Quantity.t ->
  max_notional_exposure:Scalar.Money.t option ->
  initial_margin_bps:int ->
  maintenance_margin_bps:int ->
  shorting_allowed:bool ->
  (instrument_policy, string) result

val create_group_limits :
  max_gross_exposure:Scalar.Money.t option ->
  max_long_exposure:Scalar.Money.t option ->
  max_short_exposure:Scalar.Money.t option ->
  max_absolute_net_exposure:Scalar.Money.t option ->
  max_concentration:Scalar.Ratio.t option ->
  (group_limits, string) result

val create_group :
  group_id:Id.Risk_group.t ->
  group_kind:group_kind ->
  instrument_ids:Id.Instrument.t list ->
  limits:group_limits ->
  (group, string) result

val base_currency : t -> string
val instruments : t -> Instrument.t list
val instrument : t -> Id.Instrument.t -> Instrument.t option
val instrument_policies : t -> instrument_policy list
val instrument_policy : t -> Id.Instrument.t -> instrument_policy option
val groups : t -> group list
val max_order_quantity : t -> Scalar.Quantity.t
val max_long_position : t -> Scalar.Quantity.t
val max_short_position : t -> Scalar.Quantity.t
val max_gross_exposure : t -> Scalar.Money.t
val max_leverage : t -> Scalar.Ratio.t
val initial_margin_bps : t -> int
val maintenance_margin_bps : t -> int
val max_order_quantity_for : t -> Id.Instrument.t -> Scalar.Quantity.t option
val check_position : t -> Scalar.Quantity.t -> (unit, string) result

val check_position_for :
  t -> Id.Instrument.t -> Scalar.Quantity.t -> (unit, string) result

val margin_snapshot : t -> Account.valuation -> (margin_snapshot, string) result

val group_exposures :
  t -> Account.valuation -> (group_exposure list, string) result

val check_initial : t -> Account.valuation -> (unit, string) result

val check_post_fill :
  t ->
  before_position:Scalar.Quantity.t ->
  after_position:Scalar.Quantity.t ->
  before:Account.valuation ->
  after:Account.valuation ->
  (unit, fill_check_error) result

val check_post_fill_for :
  t ->
  instrument_id:Id.Instrument.t ->
  before_position:Scalar.Quantity.t ->
  after_position:Scalar.Quantity.t ->
  before:Account.valuation ->
  after:Account.valuation ->
  (unit, fill_check_error) result

val check_reserved_fill :
  t ->
  account:Account.t ->
  oms:Oms.t ->
  marks:(Id.Instrument.t * Scalar.Price.t) list ->
  fx_rates:(string * Scalar.Price.t) list ->
  order:Order.t ->
  filled_quantity:Scalar.Quantity.t ->
  after:Account.valuation ->
  (unit, fill_check_error) result

val check :
  t ->
  account:Account.t ->
  oms:Oms.t ->
  marks:(Id.Instrument.t * Scalar.Price.t) list ->
  fx_rates:(string * Scalar.Price.t) list ->
  Order.request ->
  (unit, string) result
