(** One synchronized completed-bar observation for the configured market. *)

type t = private {
  slice_sequence : int64;
  start_at : Ptime.t;
  end_at : Ptime.t;
  available_at : Ptime.t;
  received_at : Ptime.t;
  bars : Bar.t list;
}

val create :
  slice_sequence:int64 ->
  start_at:Ptime.t ->
  end_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  bars:Bar.t list ->
  (t, string) result

val bar : t -> Id.Instrument.t -> Bar.t option
val compare_replay_order : t -> t -> int
val pp : Format.formatter -> t -> unit
