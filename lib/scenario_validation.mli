(** Cross-field and cross-record scenario semantics. *)

val header :
  root:string ->
  contract_version:string ->
  base_currency:string ->
  initial_cash:(string * Scalar.Money.t) list ->
  instruments:Instrument.t list ->
  venue_calendars:Venue_calendar.t list ->
  max_internal_events:int ->
  (string list * Id.Instrument.Set.t, Scenario_shape.error) result

val initial_portfolio :
  root:string ->
  currencies:string list ->
  catalog:Id.Instrument.Set.t ->
  instruments:Instrument.t list ->
  risk:Risk.t ->
  Initial_portfolio.t ->
  (unit, Scenario_shape.error) result

val batch :
  root:string ->
  base_currency:string ->
  currencies:string list ->
  instruments:Instrument.t list ->
  risk:Risk.t ->
  catalog:Id.Instrument.Set.t ->
  schedule:(int64 * Strategy.intent list) list ->
  slices:Market_slice.t list ->
  (unit, Scenario_shape.error) result

val stream_item :
  root:string ->
  base_currency:string ->
  instruments:Instrument.t list ->
  risk:Risk.t ->
  previous_slice:Market_slice.t option ->
  previous_intents:Strategy.intent list ->
  prior_action_ids:Id.Corporate_action.Set.t ->
  market_slice:Market_slice.t ->
  intents:Strategy.intent list ->
  (Id.Corporate_action.Set.t, Scenario_shape.error) result
