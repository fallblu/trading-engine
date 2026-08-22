(** Effective-time borrow availability and multi-currency financing policy. *)

type day_count = Actual_365 | Actual_360
type compounding = Simple | Daily
type missing_data = Reject | Zero
type locate_policy = Reject_order | Clip_fill
type recall_policy = Reject_new_shorts | Close_out

type policy = private {
  day_count : day_count;
  compounding : compounding;
  borrow_missing_data : missing_data;
  cash_missing_data : missing_data;
  locate_policy : locate_policy;
  recall_policy : recall_policy;
}

type borrow_observation = private {
  instrument_id : Id.Instrument.t;
  effective_at : Ptime.t;
  available_quantity : Scalar.Quantity.t;
  annual_rate_bps : int;
  recalled : bool;
}

type cash_rate_observation = private {
  currency : string;
  effective_at : Ptime.t;
  credit_rate_bps : int;
  debit_rate_bps : int;
}

val policy :
  day_count:day_count ->
  compounding:compounding ->
  borrow_missing_data:missing_data ->
  cash_missing_data:missing_data ->
  locate_policy:locate_policy ->
  recall_policy:recall_policy ->
  policy

val legacy_policy : policy

val borrow_observation :
  instrument_id:Id.Instrument.t ->
  effective_at:Ptime.t ->
  available_quantity:Scalar.Quantity.t ->
  annual_rate_bps:int ->
  recalled:bool ->
  (borrow_observation, string) result

val cash_rate_observation :
  currency:string ->
  effective_at:Ptime.t ->
  credit_rate_bps:int ->
  debit_rate_bps:int ->
  (cash_rate_observation, string) result

val accrue :
  policy ->
  principal:Scalar.Money.t ->
  annual_rate_bps:int ->
  Ptime.Span.t ->
  (Scalar.Money.t, string) result
(** [accrue] returns signed interest. Rounding is nearest micro-unit with ties
    away from zero. Daily compounding rounds and capitalizes after every full
    day, then accrues a simple fractional-day remainder. *)

val day_count_to_string : day_count -> string
val compounding_to_string : compounding -> string
val missing_data_to_string : missing_data -> string
val locate_policy_to_string : locate_policy -> string
val recall_policy_to_string : recall_policy -> string
