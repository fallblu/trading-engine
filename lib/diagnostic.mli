(** Stable structured errors for process and file boundaries. *)

val version : string

type code =
  | Cli_invalid_arguments
  | Input_io
  | Scenario_invalid_json
  | Scenario_invalid
  | Scenario_unsupported_contract
  | Scenario_stream_invalid
  | Scenario_stream_changed
  | Resource_limit
  | Replay_failed
  | Reducer_failed
  | Strategy_invalid_configuration
  | Strategy_protocol
  | Strategy_timeout
  | Strategy_process
  | Strategy_exit
  | Artifact_exists
  | Artifact_io
  | Artifact_state

type phase = Cli | Input | Validation | Replay | Reducer | Strategy | Artifact

type cause = private {
  kind : string;
  message : string;
  operation : string option;
  target : string option;
}

type context = private {
  json_path : string option;
  line : int option;
  sequence : int64 option;
  event_id : string option;
  order_id : string option;
  causation_ids : string list;
}

type t = private {
  code : code;
  phase : phase;
  message : string;
  context : context;
  cause : cause option;
}

val cause_of_exception : exn -> cause

val make :
  ?json_path:string ->
  ?line:int ->
  ?sequence:int64 ->
  ?event_id:string ->
  ?order_id:string ->
  ?causation_ids:string list ->
  ?cause:cause ->
  code:code ->
  phase:phase ->
  string ->
  t

val of_exception :
  ?json_path:string ->
  ?line:int ->
  ?sequence:int64 ->
  code:code ->
  phase:phase ->
  message:string ->
  exn ->
  t

val annotate :
  ?json_path:string ->
  ?line:int ->
  ?sequence:int64 ->
  ?event_id:string ->
  ?order_id:string ->
  ?causation_ids:string list ->
  t ->
  t

val combine : t -> t -> t
(** [combine primary secondary] preserves the primary identity and context while
    adding the secondary message and an underlying cause when needed. *)

val code_to_string : code -> string
val phase_to_string : phase -> string
val to_yojson : t -> Yojson.Safe.t
val to_json : t -> string
val to_human : t -> string
val pp : t Fmt.t
