(** Deterministic trade-settlement calendars, policies, and instructions. *)

type cash_buying_power = Total_cash | Settled_cash
type position_availability = Total_positions | Settled_positions

type calendar = private {
  calendar_id : string;
  version : string;
  business_dates : string list;
}

type rule = private {
  instrument_id : Id.Instrument.t;
  calendar_id : string;
  lag_business_days : int;
}

type policy = private {
  cash_buying_power : cash_buying_power;
  position_availability : position_availability;
  calendars : calendar list;
  rules : rule list;
}

type status =
  | Pending
  | Settled of Ptime.t
  | Failed of { failed_at : Ptime.t; reason : string }

type instruction = private {
  instruction_id : string;
  fill_id : Id.Fill.t;
  instrument_id : Id.Instrument.t;
  currency : string;
  cash_movement : Scalar.Money.t;
  position_movement : Scalar.Quantity.t;
  trade_date : string;
  due_date : string;
  status : status;
}

type failure = private { instruction_id : string; reason : string }

val calendar :
  calendar_id:string ->
  version:string ->
  business_dates:string list ->
  (calendar, string) result

val rule :
  instrument_id:Id.Instrument.t ->
  calendar_id:string ->
  lag_business_days:int ->
  (rule, string) result

val policy :
  cash_buying_power:cash_buying_power ->
  position_availability:position_availability ->
  calendars:calendar list ->
  rules:rule list ->
  (policy, string) result

val instruction : policy -> Fill.t -> (instruction, string) result
val failure : instruction_id:string -> reason:string -> (failure, string) result
val settle : instruction -> settled_at:Ptime.t -> (instruction, string) result

val fail :
  instruction ->
  failed_at:Ptime.t ->
  reason:string ->
  (instruction, string) result

val date_of_timestamp : Ptime.t -> string
val is_due : instruction -> Ptime.t -> bool
val cash_buying_power_to_string : cash_buying_power -> string
val position_availability_to_string : position_availability -> string
val status_to_string : status -> string
