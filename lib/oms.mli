(** Deterministic order-management state. *)

type t
type fill_outcome = Applied of Order.t | Duplicate

val empty : t
val find : t -> Id.Order.t -> Order.t option
val orders : t -> Order.t list
val active_orders : t -> Order.t list
val active_for_instrument : t -> Id.Instrument.t -> Order.t list

val accept :
  t ->
  id:Id.Order.t ->
  accepted_sequence:int64 ->
  created_at:Ptime.t ->
  eligible_after_slice_sequence:int64 ->
  Order.request ->
  (t * Order.t, string) result

val reject :
  t ->
  id:Id.Order.t ->
  rejected_sequence:int64 ->
  created_at:Ptime.t ->
  eligible_after_slice_sequence:int64 ->
  Order.request ->
  reason:string ->
  (t * Order.t, string) result

val cancel : t -> Id.Order.t -> (t * Order.t, string) result
val apply_fill : t -> Fill.t -> (t * fill_outcome, string) result
