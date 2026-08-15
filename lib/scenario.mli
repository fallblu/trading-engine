(** Strict replay scenario input contract. *)

type t = private {
  contract_version : string;
  metadata : Yojson.Safe.t;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : (string * Scalar.Money.t) list;
  instruments : Instrument.t list;
  risk : Risk.t;
  execution_model : Execution_model.t;
  execution : Execution.t;
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
  instruments : Instrument.t list;
  risk : Risk.t;
  execution_model : Execution_model.t;
  execution : Execution.t;
  max_internal_events : int;
}

type stream_item = private {
  market_slice : Market_slice.t;
  intents : Strategy.intent list;
  action_ids : Id.Corporate_action.Set.t;
}

val of_yojson : Yojson.Safe.t -> (t, string) result
val of_string : string -> (t, string) result
val read_file : string -> (t, string) result

val stream_header_of_yojson :
  contract_version:string -> Yojson.Safe.t -> (stream_header, string) result

val stream_item_of_yojson :
  stream_header ->
  previous:stream_item option ->
  Yojson.Safe.t ->
  (stream_item, string) result
