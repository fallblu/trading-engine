(** One instrument's completed OHLCV bar in a synchronized market slice. *)

type t = private {
  instrument_id : Id.Instrument.t;
  open_price : Scalar.Price.t;
  high_price : Scalar.Price.t;
  low_price : Scalar.Price.t;
  close_price : Scalar.Price.t;
  volume : Scalar.Quantity.t option;
}

val create :
  instrument_id:Id.Instrument.t ->
  open_price:Scalar.Price.t ->
  high_price:Scalar.Price.t ->
  low_price:Scalar.Price.t ->
  close_price:Scalar.Price.t ->
  volume:Scalar.Quantity.t option ->
  (t, string) result

val pp : Format.formatter -> t -> unit
