(** Immutable orders and their legal state transitions. *)

type side = Buy | Sell

type kind =
  | Market
  | Limit of Scalar.Price.t
  | Stop of Scalar.Price.t
  | Stop_limit of {
      trigger_price : Scalar.Price.t;
      limit_price : Scalar.Price.t;
    }

type time_in_force =
  | Gtc
  | Ioc
  | Fok
  | Day of { venue_id : Id.Venue.t; calendar_id : Id.Venue_calendar.t }
  | Gtd of Ptime.t

type origin = Direct | Target_rebalance | Margin_liquidation | Borrow_recall

type request = private {
  instrument_id : Id.Instrument.t;
  side : side;
  quantity : Scalar.Quantity.t;
  kind : kind;
  time_in_force : time_in_force;
  origin : origin;
}

type trigger_state =
  | Dormant
  | Triggered of { triggered_at : Ptime.t; triggered_slice_sequence : int64 }

type status =
  | Working
  | Partially_filled
  | Filled
  | Cancelled
  | Rejected of string

type t = private {
  id : Id.Order.t;
  request : request;
  created_event_id : Id.Event.t;
  updated_event_id : Id.Event.t;
  created_sequence : int64;
  created_at : Ptime.t;
  eligible_after_slice_sequence : int64;
  filled_quantity : Scalar.Quantity.t;
  filled_notional : Scalar.Money.t;
  trigger_state : trigger_state option;
  status : status;
}

val default_time_in_force : kind -> time_in_force

val request :
  instrument_id:Id.Instrument.t ->
  side:side ->
  quantity:Scalar.Quantity.t ->
  kind:kind ->
  time_in_force:time_in_force ->
  origin:origin ->
  (request, string) result

val accept :
  id:Id.Order.t ->
  created_event_id:Id.Event.t ->
  accepted_sequence:int64 ->
  created_at:Ptime.t ->
  eligible_after_slice_sequence:int64 ->
  request ->
  (t, string) result

val reject :
  id:Id.Order.t ->
  created_event_id:Id.Event.t ->
  rejected_sequence:int64 ->
  created_at:Ptime.t ->
  eligible_after_slice_sequence:int64 ->
  request ->
  reason:string ->
  (t, string) result

val remaining_quantity : t -> Scalar.Quantity.t
val is_active : t -> bool
val is_terminal : t -> bool
val is_market : t -> bool
val is_ioc : t -> bool
val is_fok : t -> bool
val is_dormant_stop : t -> bool
val effective_kind : t -> kind option

val trigger :
  t ->
  updated_event_id:Id.Event.t ->
  triggered_at:Ptime.t ->
  triggered_slice_sequence:int64 ->
  (t, string) result

val apply_fill :
  t ->
  quantity:Scalar.Quantity.t ->
  notional:Scalar.Money.t ->
  (t, string) result

val cancel : t -> (t, string) result

val adjust_for_split :
  t ->
  updated_event_id:Id.Event.t ->
  numerator:int64 ->
  denominator:int64 ->
  (t, string) result

val side_to_string : side -> string
val kind_to_string : kind -> string
val time_in_force_to_string : time_in_force -> string
val origin_to_string : origin -> string
val status_to_string : status -> string
val pp : Format.formatter -> t -> unit
