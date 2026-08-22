(** Strict top-level JSON shapes for batch and streamed scenarios. *)

type error = { json_path : string; message : string }

type common = {
  metadata : Yojson.Safe.t;
  run_id : Yojson.Safe.t;
  base_currency : Yojson.Safe.t;
  initial_state : Yojson.Safe.t;
  instruments : Yojson.Safe.t;
  venue_calendars : Yojson.Safe.t option;
  risk : Yojson.Safe.t;
  execution : Yojson.Safe.t;
  max_internal_events : Yojson.Safe.t;
}

type batch = {
  contract_version : Yojson.Safe.t;
  common : common;
  schedule : Yojson.Safe.t;
  slices : Yojson.Safe.t;
}

type stream_item = { market_slice : Yojson.Safe.t; intents : Yojson.Safe.t }

val error : json_path:string -> string -> error
val batch : Yojson.Safe.t -> (batch, error) result

val stream_header :
  contract_version:string -> Yojson.Safe.t -> (common, error) result

val stream_item : Yojson.Safe.t -> (stream_item, error) result
