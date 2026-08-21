(** Replay runners driven by one supervised external strategy process. *)

type result = private {
  account : Account.t;
  orders : Order.t list;
  valuation : Account.valuation;
  audits : Audit.t list;
  strategy : Strategy_protocol.identity;
}

type streamed_result = private {
  run_id : Id.Run.t;
  scenario_sha256 : string;
  instrument_count : int;
  account : Account.t;
  orders : Order.t list;
  valuation : Account.valuation;
  audit_count : int64;
  slice_count : int64;
  strategy : Strategy_protocol.identity;
}

val run :
  env:Eio_unix.Stdenv.base ->
  scenario_sha256:string ->
  journal_path:string ->
  transcript_path:string ->
  strategy_command:string list ->
  strategy_timeout:float ->
  Scenario.t ->
  (result, Diagnostic.t) Stdlib.result

val run_stream :
  env:Eio_unix.Stdenv.base ->
  journal_path:string ->
  transcript_path:string ->
  strategy_command:string list ->
  strategy_timeout:float ->
  string ->
  (streamed_result, Diagnostic.t) Stdlib.result
