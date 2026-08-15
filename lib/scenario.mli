(** Version 1 replay-scenario input contract. *)

type t = private {
  schema_version : int;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : Scalar.Money.t;
  instruments : Instrument.t list;
  risk : Risk.t;
  execution : Execution.t;
  max_internal_events : int;
  schedule : (int64 * Strategy.intent list) list;
  bars : Bar.t list;
}

val of_yojson : Yojson.Safe.t -> (t, string) result
val of_string : string -> (t, string) result
val read_file : string -> (t, string) result
