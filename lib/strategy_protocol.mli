(** Versioned JSON Lines protocol for an out-of-process strategy. *)

val version : string
val max_message_bytes : int

type initialization = {
  scenario_contract_version : string;
  scenario_sha256 : string;
  metadata : Yojson.Safe.t;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : (string * Scalar.Money.t) list;
  instruments : Instrument.t list;
  risk : Risk.t;
  execution_model : Execution_model.t;
  execution : Execution.t;
}

type identity = private { name : Id.Strategy.t; version : string option }

type response =
  | Ready of identity
  | Intents of Strategy.intent list
  | Stopped
  | Failed of string

type direction = Engine_to_strategy | Strategy_to_engine

val initialize_message : sequence:int64 -> initialization -> Yojson.Safe.t

val event_message :
  sequence:int64 -> Strategy.context -> Strategy.event -> Yojson.Safe.t

val shutdown_message : sequence:int64 -> Yojson.Safe.t

val response_of_yojson :
  expected_sequence:int64 -> Yojson.Safe.t -> (response, string) result

val response_of_string :
  expected_sequence:int64 -> string -> (response * Yojson.Safe.t, string) result

val transcript_record :
  transcript_sequence:int64 ->
  direction:direction ->
  message:Yojson.Safe.t ->
  Yojson.Safe.t

val message_to_string : Yojson.Safe.t -> string
