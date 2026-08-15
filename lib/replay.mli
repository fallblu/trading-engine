(** Run a complete deterministic scenario and optionally persist its audit
    events. *)

type result = private {
  account : Account.t;
  orders : Order.t list;
  valuation : Account.valuation;
  audits : Audit.t list;
}

type streamed_result = private {
  run_id : Id.Run.t;
  scenario_sha256 : string;
  instrument_count : int;
  schedule_count : int64;
  account : Account.t;
  orders : Order.t list;
  valuation : Account.valuation;
  audit_count : int64;
  slice_count : int64;
}

val run :
  scenario_sha256:string ->
  ?journal_path:string ->
  Scenario.t ->
  (result, string) Stdlib.result

val run_stream :
  ?journal_path:string -> string -> (streamed_result, string) Stdlib.result
