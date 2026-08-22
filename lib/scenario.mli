(** Strict replay scenario input contract. *)

type t = private {
  contract_version : string;
  metadata : Yojson.Safe.t;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : (string * Scalar.Money.t) list;
  initial_portfolio : Initial_portfolio.t option;
  instruments : Instrument.t list;
  venue_calendars : Venue_calendar.t list;
  risk : Risk.t;
  execution_model : Execution_model.t;
  execution : Execution.t;
  financing : Financing.policy option;
  settlement : Settlement.policy option;
  max_internal_events : int;
  schedule : (int64 * Strategy.intent list) list;
  slices : Market_slice.t list;
}

type stream_header = private {
  contract_version : string;
  metadata : Yojson.Safe.t;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : (string * Scalar.Money.t) list;
  initial_portfolio : Initial_portfolio.t option;
  instruments : Instrument.t list;
  venue_calendars : Venue_calendar.t list;
  risk : Risk.t;
  execution_model : Execution_model.t;
  execution : Execution.t;
  financing : Financing.policy option;
  settlement : Settlement.policy option;
  max_internal_events : int;
}

type stream_item = private {
  market_slice : Market_slice.t;
  intents : Strategy.intent list;
  action_ids : Id.Corporate_action.Set.t;
}

val of_yojson : Yojson.Safe.t -> (t, Diagnostic.t) result
val of_string : string -> (t, Diagnostic.t) result
val read_file : string -> (t, Diagnostic.t) result

val intent_of_yojson :
  ?contract_version:string ->
  Yojson.Safe.t ->
  (Strategy.intent, Diagnostic.t) result

val stream_header_of_yojson :
  contract_version:string ->
  Yojson.Safe.t ->
  (stream_header, Diagnostic.t) result

val stream_item_of_yojson :
  stream_header ->
  previous:stream_item option ->
  Yojson.Safe.t ->
  (stream_item, Diagnostic.t) result
