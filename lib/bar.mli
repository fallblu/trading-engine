(** One completed OHLCV bar and its replay availability. *)

type t = private {
  source_sequence : int64;
  instrument_id : Id.Instrument.t;
  start_at : Ptime.t;
  end_at : Ptime.t;
  available_at : Ptime.t;
  received_at : Ptime.t;
  open_price : Scalar.Price.t;
  high_price : Scalar.Price.t;
  low_price : Scalar.Price.t;
  close_price : Scalar.Price.t;
  volume : Scalar.Quantity.t option;
}

val create :
  source_sequence:int64 ->
  instrument_id:Id.Instrument.t ->
  start_at:Ptime.t ->
  end_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  open_price:Scalar.Price.t ->
  high_price:Scalar.Price.t ->
  low_price:Scalar.Price.t ->
  close_price:Scalar.Price.t ->
  volume:Scalar.Quantity.t option ->
  (t, string) result

val compare_replay_order : t -> t -> int
val pp : Format.formatter -> t -> unit
