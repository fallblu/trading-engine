(** One synchronized completed-bar observation, complete FX snapshot, and
    pre-match corporate-action batch for the configured market. *)

type fx_mark = private { currency : string; rate : Scalar.Price.t }

val fx_mark : currency:string -> rate:Scalar.Price.t -> (fx_mark, string) result

type t = private {
  slice_sequence : int64;
  start_at : Ptime.t;
  end_at : Ptime.t;
  available_at : Ptime.t;
  received_at : Ptime.t;
  bars : Bar.t list;
  market_events : Market_event.t list;
  order_book_events : Order_book_event.t list;
  fx_rates : fx_mark list;
  corporate_actions : Corporate_action.t list;
  lifecycle_events : Instrument_lifecycle.event list;
  borrow_observations : Financing.borrow_observation list;
  cash_rate_observations : Financing.cash_rate_observation list;
  settlement_failures : Settlement.failure list;
}

val create :
  slice_sequence:int64 ->
  start_at:Ptime.t ->
  end_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  bars:Bar.t list ->
  fx_rates:fx_mark list ->
  corporate_actions:Corporate_action.t list ->
  (t, string) result

val create_v10 :
  slice_sequence:int64 ->
  start_at:Ptime.t ->
  end_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  bars:Bar.t list ->
  fx_rates:fx_mark list ->
  corporate_actions:Corporate_action.t list ->
  borrow_observations:Financing.borrow_observation list ->
  cash_rate_observations:Financing.cash_rate_observation list ->
  (t, string) result

val create_v11 :
  slice_sequence:int64 ->
  start_at:Ptime.t ->
  end_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  bars:Bar.t list ->
  fx_rates:fx_mark list ->
  corporate_actions:Corporate_action.t list ->
  borrow_observations:Financing.borrow_observation list ->
  cash_rate_observations:Financing.cash_rate_observation list ->
  settlement_failures:Settlement.failure list ->
  (t, string) result

val create_v12 :
  slice_sequence:int64 ->
  start_at:Ptime.t ->
  end_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  bars:Bar.t list ->
  fx_rates:fx_mark list ->
  corporate_actions:Corporate_action.t list ->
  borrow_observations:Financing.borrow_observation list ->
  cash_rate_observations:Financing.cash_rate_observation list ->
  settlement_failures:Settlement.failure list ->
  lifecycle_events:Instrument_lifecycle.event list ->
  (t, string) result

val create_v13 :
  slice_sequence:int64 ->
  start_at:Ptime.t ->
  end_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  bars:Bar.t list ->
  fx_rates:fx_mark list ->
  corporate_actions:Corporate_action.t list ->
  borrow_observations:Financing.borrow_observation list ->
  cash_rate_observations:Financing.cash_rate_observation list ->
  settlement_failures:Settlement.failure list ->
  lifecycle_events:Instrument_lifecycle.event list ->
  (t, string) result

val create_v14 :
  slice_sequence:int64 ->
  start_at:Ptime.t ->
  end_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  bars:Bar.t list ->
  fx_rates:fx_mark list ->
  corporate_actions:Corporate_action.t list ->
  borrow_observations:Financing.borrow_observation list ->
  cash_rate_observations:Financing.cash_rate_observation list ->
  settlement_failures:Settlement.failure list ->
  lifecycle_events:Instrument_lifecycle.event list ->
  market_events:Market_event.t list ->
  (t, string) result

val create_v15 :
  slice_sequence:int64 ->
  start_at:Ptime.t ->
  end_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  bars:Bar.t list ->
  fx_rates:fx_mark list ->
  corporate_actions:Corporate_action.t list ->
  borrow_observations:Financing.borrow_observation list ->
  cash_rate_observations:Financing.cash_rate_observation list ->
  settlement_failures:Settlement.failure list ->
  lifecycle_events:Instrument_lifecycle.event list ->
  market_events:Market_event.t list ->
  order_book_events:Order_book_event.t list ->
  (t, string) result

val bar : t -> Id.Instrument.t -> Bar.t option
val fx_rate : t -> string -> Scalar.Price.t option
val compare_replay_order : t -> t -> int
val pp : Format.formatter -> t -> unit
