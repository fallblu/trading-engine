(** Deterministic synchronized-slice execution simulation. *)

type t

type proposed_fill = private {
  order_id : Id.Order.t;
  quantity : Scalar.Quantity.t;
  price : Scalar.Price.t;
  fee : Scalar.Money.t;
  executed_at : Ptime.t;
}

type match_result = private {
  fills : proposed_fill list;
  market_ioc_orders : Id.Order.t list;
}

type cursor

type step =
  | Finished of Id.Order.t list
  | Proposed of proposed_fill * (Scalar.Quantity.t -> (cursor, string) result)

val cursor : (oms:Oms.t -> (step, string) result) -> cursor
(** Build an immutable cursor from one matching-step function. *)

val create :
  participation_bps:int ->
  fixed_fee:Scalar.Money.t ->
  fee_bps:int ->
  (t, string) result

val participation_bps : t -> int
val fixed_fee : t -> Scalar.Money.t
val fee_bps : t -> int

val start_slice :
  t ->
  instruments:Instrument.t list ->
  oms:Oms.t ->
  Market_slice.t ->
  (cursor, string) result
(** Start an immutable matching cursor from the orders eligible at the slice
    boundary. *)

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
    quantity consumes the shared per-instrument slice capacity. *)

val match_slice :
  t ->
  instruments:Instrument.t list ->
  oms:Oms.t ->
  Market_slice.t ->
  (match_result, string) result
