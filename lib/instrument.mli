(** Executable instrument metadata supplied by an approved catalog. *)

type t = private {
  id : Id.Instrument.t;
  symbol : string;
  quote_currency : string;
  tick_size : Scalar.Price.t;
  lot_size : Scalar.Quantity.t;
}

val create :
  id:Id.Instrument.t ->
  symbol:string ->
  quote_currency:string ->
  tick_size:Scalar.Price.t ->
  lot_size:Scalar.Quantity.t ->
  (t, string) result

val pp : Format.formatter -> t -> unit
