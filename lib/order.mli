(** Immutable orders and their legal state transitions. *)

type side = Buy | Sell
type kind = Market | Limit of Scalar.Price.t
type origin = Direct | Target_rebalance

type request = private {
  instrument_id : Id.Instrument.t;
  side : side;
  quantity : Scalar.Quantity.t;
  kind : kind;
  origin : origin;
}

type status =
  | Working
  | Partially_filled
  | Filled
  | Cancelled
  | Rejected of string

type t = private {
  id : Id.Order.t;
  request : request;
  created_sequence : int64;
  eligible_after_bar_sequence : int64;
  filled_quantity : Scalar.Quantity.t;
  filled_notional : Scalar.Money.t;
  status : status;
}

val request :
  instrument_id:Id.Instrument.t ->
  side:side ->
  quantity:Scalar.Quantity.t ->
  kind:kind ->
  origin:origin ->
  (request, string) result

val accept :
  id:Id.Order.t ->
  accepted_sequence:int64 ->
  eligible_after_bar_sequence:int64 ->
  request ->
  (t, string) result

val reject :
  id:Id.Order.t ->
  rejected_sequence:int64 ->
  eligible_after_bar_sequence:int64 ->
  request ->
  reason:string ->
  (t, string) result

val remaining_quantity : t -> Scalar.Quantity.t
val is_active : t -> bool
val is_terminal : t -> bool
val is_market : t -> bool

val apply_fill :
  t ->
  quantity:Scalar.Quantity.t ->
  notional:Scalar.Money.t ->
  (t, string) result

val cancel : t -> (t, string) result
val side_to_string : side -> string
val kind_to_string : kind -> string
val origin_to_string : origin -> string
val status_to_string : status -> string
val pp : Format.formatter -> t -> unit
