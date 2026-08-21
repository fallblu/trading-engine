let version = "1"

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

type cause = {
  kind : string;
  message : string;
  operation : string option;
  target : string option;
}

type context = {
  json_path : string option;
  line : int option;
  sequence : int64 option;
  event_id : string option;
  order_id : string option;
  causation_ids : string list;
}

type t = {
  code : code;
  phase : phase;
  message : string;
  context : context;
  cause : cause option;
}

let code_to_string = function
  | Cli_invalid_arguments -> "cli.invalid_arguments"
  | Input_io -> "input.io"
  | Scenario_invalid_json -> "scenario.invalid_json"
  | Scenario_invalid -> "scenario.invalid"
  | Scenario_unsupported_contract -> "scenario.unsupported_contract"
  | Scenario_stream_invalid -> "scenario_stream.invalid"
  | Scenario_stream_changed -> "scenario_stream.changed"
  | Resource_limit -> "resource.limit"
  | Replay_failed -> "replay.failed"
  | Reducer_failed -> "reducer.failed"
  | Strategy_invalid_configuration -> "strategy.invalid_configuration"
  | Strategy_protocol -> "strategy.protocol"
  | Strategy_timeout -> "strategy.timeout"
  | Strategy_process -> "strategy.process"
  | Strategy_exit -> "strategy.exit"
  | Artifact_exists -> "artifact.exists"
  | Artifact_io -> "artifact.io"
  | Artifact_state -> "artifact.state"

let phase_to_string = function
  | Cli -> "cli"
  | Input -> "input"
  | Validation -> "validation"
  | Replay -> "replay"
  | Reducer -> "reducer"
  | Strategy -> "strategy"
  | Artifact -> "artifact"

let cause_of_exception = function
  | Unix.Unix_error (code, operation, target) ->
      {
        kind = "unix_error";
        message = Unix.error_message code;
        operation = Some operation;
        target = Some target;
      }
  | Sys_error message ->
      { kind = "system_error"; message; operation = None; target = None }
  | exception_ ->
      {
        kind = "exception";
        message = Printexc.to_string exception_;
        operation = None;
        target = None;
      }

let context ?json_path ?line ?sequence ?event_id ?order_id ?(causation_ids = [])
    () =
  { json_path; line; sequence; event_id; order_id; causation_ids }

let make ?json_path ?line ?sequence ?event_id ?order_id ?causation_ids ?cause
    ~code ~phase message =
  {
    code;
    phase;
    message;
    context =
      context ?json_path ?line ?sequence ?event_id ?order_id ?causation_ids ();
    cause;
  }

let of_exception ?json_path ?line ?sequence ~code ~phase ~message exception_ =
  make ?json_path ?line ?sequence
    ~cause:(cause_of_exception exception_)
    ~code ~phase message

let annotate ?json_path ?line ?sequence ?event_id ?order_id ?causation_ids
    diagnostic =
  let choose supplied existing =
    match existing with Some _ -> existing | None -> supplied
  in
  let context = diagnostic.context in
  {
    diagnostic with
    context =
      {
        json_path = choose json_path context.json_path;
        line = choose line context.line;
        sequence = choose sequence context.sequence;
        event_id = choose event_id context.event_id;
        order_id = choose order_id context.order_id;
        causation_ids =
          Option.value causation_ids ~default:context.causation_ids;
      };
  }

let combine primary secondary =
  {
    primary with
    message = primary.message ^ "; " ^ secondary.message;
    cause =
      (match primary.cause with
      | Some _ as cause -> cause
      | None -> secondary.cause);
  }

let optional name value encode =
  match value with None -> [] | Some value -> [ (name, encode value) ]

let context_to_yojson context =
  `Assoc
    (optional "json_path" context.json_path (fun value -> `String value)
    @ optional "line" context.line (fun value -> `Int value)
    @ optional "sequence" context.sequence (fun value ->
        `String (Int64.to_string value))
    @ optional "event_id" context.event_id (fun value -> `String value)
    @ optional "order_id" context.order_id (fun value -> `String value)
    @
    if context.causation_ids = [] then []
    else
      [
        ( "causation_ids",
          `List (List.map (fun value -> `String value) context.causation_ids) );
      ])

let cause_to_yojson cause =
  `Assoc
    ([ ("kind", `String cause.kind); ("message", `String cause.message) ]
    @ optional "operation" cause.operation (fun value -> `String value)
    @ optional "target" cause.target (fun value -> `String value))

let to_yojson diagnostic =
  `Assoc
    [
      ("diagnostic_version", `String version);
      ("code", `String (code_to_string diagnostic.code));
      ("phase", `String (phase_to_string diagnostic.phase));
      ("message", `String diagnostic.message);
      ("context", context_to_yojson diagnostic.context);
      ("cause", Option.fold ~none:`Null ~some:cause_to_yojson diagnostic.cause);
    ]

let to_json diagnostic = to_yojson diagnostic |> Yojson.Safe.to_string
let to_human diagnostic = diagnostic.message
let pp formatter diagnostic = Fmt.string formatter (to_human diagnostic)
