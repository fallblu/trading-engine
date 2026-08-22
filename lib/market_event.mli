(** Causally observable quote and trade events. *)

type aggressor_side = Buy | Sell | Unknown

type kind =
  | Quote of {
      bid_price : Scalar.Price.t;
      bid_quantity : Scalar.Quantity.t;
      ask_price : Scalar.Price.t;
      ask_quantity : Scalar.Quantity.t;
    }
  | Trade of {
      price : Scalar.Price.t;
      quantity : Scalar.Quantity.t;
      aggressor_side : aggressor_side;
    }

type t = private {
  instrument_id : Id.Instrument.t;
  event_at : Ptime.t;
  available_at : Ptime.t;
  received_at : Ptime.t;
  ingest_sequence : int64;
  kind : kind;
}

val quote :
  instrument_id:Id.Instrument.t ->
  event_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  ingest_sequence:int64 ->
  bid_price:Scalar.Price.t ->
  bid_quantity:Scalar.Quantity.t ->
  ask_price:Scalar.Price.t ->
  ask_quantity:Scalar.Quantity.t ->
  (t, string) result

val trade :
  instrument_id:Id.Instrument.t ->
  event_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  ingest_sequence:int64 ->
  price:Scalar.Price.t ->
  quantity:Scalar.Quantity.t ->
  aggressor_side:aggressor_side ->
  (t, string) result

val compare_replay_order : t -> t -> int
val aggressor_side_to_string : aggressor_side -> string
val aggressor_side_of_string : string -> (aggressor_side, string) result
