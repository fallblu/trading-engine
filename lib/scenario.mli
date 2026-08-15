(** Strict replay scenario input contract. *)

type t = private {
  metadata : Yojson.Safe.t;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : Scalar.Money.t;
  instruments : Instrument.t list;
  risk : Risk.t;
  execution : Execution.t;
  max_internal_events : int;
  schedule : (int64 * Strategy.intent list) list;
  slices : Market_slice.t list;
}

val of_yojson : Yojson.Safe.t -> (t, string) result
val of_string : string -> (t, string) result
val read_file : string -> (t, string) result
