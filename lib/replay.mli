(** Run a complete deterministic scenario and optionally persist its audit
    events. *)

type result = private {
  account : Account.t;
  orders : Order.t list;
  valuation : Account.valuation;
  audits : Audit.t list;
}

val run : ?journal_path:string -> Scenario.t -> (result, string) Stdlib.result
