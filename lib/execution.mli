(** Deterministic synchronized-slice execution simulation. *)

type t
type missing_volume_policy = Reject_missing_volume | Zero_impact

type cost_model = private {
  half_spread_bps : int;
  impact_coefficient_bps : int;
  missing_volume_policy : missing_volume_policy;
}

type price_attribution = private {
  reference_price : Scalar.Price.t;
  spread_adjustment : Scalar.Money.t;
  impact_adjustment : Scalar.Money.t;
  final_price : Scalar.Price.t;
}

type proposed_fill = private {
  order_id : Id.Order.t;
  quantity : Scalar.Quantity.t;
  price : Scalar.Price.t;
  fee : Scalar.Money.t;
  fee_components : Fee_schedule.calculated_component list;
  liquidity : Fee_schedule.liquidity;
  executed_at : Ptime.t;
  price_attribution : price_attribution option;
}

type match_result = private {
  fills : proposed_fill list;
  triggers : (Id.Order.t * Ptime.t * int64) list;
  market_ioc_orders : Id.Order.t list;
}

type cursor

type step =
  | Finished of Id.Order.t list
  | Triggered of Id.Order.t * Ptime.t * int64 * cursor
  | Proposed of proposed_fill * (Scalar.Quantity.t -> (cursor, string) result)

val cursor : (oms:Oms.t -> (step, string) result) -> cursor
(** Build an immutable cursor from one matching-step function. *)

val create :
  participation_bps:int ->
  fixed_fee:Scalar.Money.t ->
  fee_bps:int ->
  (t, string) result

val create_v2 :
  participation_bps:int ->
  fee_schedules:Fee_schedule.t list ->
  (t, string) result

val create_conservative :
  participation_bps:int ->
  fee_schedules:Fee_schedule.t list ->
  half_spread_bps:int ->
  impact_coefficient_bps:int ->
  missing_volume_policy:missing_volume_policy ->
  (t, string) result

val participation_bps : t -> int
val fixed_fee : t -> Scalar.Money.t
val fee_bps : t -> int
val fee_schedules : t -> Fee_schedule.t list
val cost_model : t -> cost_model option

val calculate_fee :
  t ->
  instrument:Instrument.t ->
  notional:Scalar.Money.t ->
  quantity:Scalar.Quantity.t ->
  liquidity:Fee_schedule.liquidity ->
  fx_rates:(string * Scalar.Price.t) list ->
  (Fee_schedule.calculated_component list * Scalar.Money.t, string) result

val start_slice :
  t ->
  instruments:Instrument.t list ->
  oms:Oms.t ->
  Market_slice.t ->
  (cursor, string) result
(** Start an immutable matching cursor from the orders eligible at the slice
    boundary. *)

val start_slice_next_open :
  t ->
  instruments:Instrument.t list ->
  oms:Oms.t ->
  Market_slice.t ->
  (cursor, string) result

val start_slice_adverse_touch :
  t ->
  instruments:Instrument.t list ->
  oms:Oms.t ->
  Market_slice.t ->
  (cursor, string) result

val finished : Id.Order.t list -> cursor
(** Build a cursor that immediately finishes. This supports execution models
    that intentionally produce no proposals. *)

val next : cursor -> oms:Oms.t -> (step, string) result
(** Produce the next proposal from the current OMS. Continuing with the applied
    quantity preserves remaining capacity and advances the eligible-order
    cursor. *)

val fold_slice :
  t ->
  instruments:Instrument.t list ->
  oms:Oms.t ->
  Market_slice.t ->
  init:'a ->
  apply:('a -> proposed_fill -> ('a * Scalar.Quantity.t, string) result) ->
  ('a * Id.Order.t list, string) result
(** Fold executable orders in liquidation-first, then sell-before-buy/FIFO
    order. The callback returns the quantity it actually applied; only that
    quantity consumes the shared per-instrument slice capacity. Returns an error
    when a dormant stop triggers because this compatibility helper has no
    callback through which to persist trigger state. *)

val match_slice :
  t ->
  instruments:Instrument.t list ->
  oms:Oms.t ->
  Market_slice.t ->
  (match_result, string) result
(** Pure deterministic matching. Conditional activations are returned in
    [triggers] and cannot fill until a later slice. *)
