(** Causally ordered level-two order-book observations. *)

type side = Bid | Ask
type level = private { price : Scalar.Price.t; quantity : Scalar.Quantity.t }

type kind =
  | Snapshot of { bids : level list; asks : level list }
  | Set of { side : side; price : Scalar.Price.t; quantity : Scalar.Quantity.t }
  | Delete of { side : side; price : Scalar.Price.t }
  | Trade of {
      price : Scalar.Price.t;
      quantity : Scalar.Quantity.t;
      aggressor_side : Market_event.aggressor_side;
    }

type t = private {
  instrument_id : Id.Instrument.t;
  event_at : Ptime.t;
  available_at : Ptime.t;
  received_at : Ptime.t;
  ingest_sequence : int64;
  book_sequence : int64;
  kind : kind;
}

val level :
  price:Scalar.Price.t -> quantity:Scalar.Quantity.t -> (level, string) result

val snapshot :
  instrument_id:Id.Instrument.t ->
  event_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  ingest_sequence:int64 ->
  book_sequence:int64 ->
  bids:level list ->
  asks:level list ->
  (t, string) result

val set :
  instrument_id:Id.Instrument.t ->
  event_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  ingest_sequence:int64 ->
  book_sequence:int64 ->
  side:side ->
  price:Scalar.Price.t ->
  quantity:Scalar.Quantity.t ->
  (t, string) result

val delete :
  instrument_id:Id.Instrument.t ->
  event_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  ingest_sequence:int64 ->
  book_sequence:int64 ->
  side:side ->
  price:Scalar.Price.t ->
  (t, string) result

val trade :
  instrument_id:Id.Instrument.t ->
  event_at:Ptime.t ->
  available_at:Ptime.t ->
  received_at:Ptime.t ->
  ingest_sequence:int64 ->
  book_sequence:int64 ->
  price:Scalar.Price.t ->
  quantity:Scalar.Quantity.t ->
  aggressor_side:Market_event.aggressor_side ->
  (t, string) result

val compare_replay_order : t -> t -> int
val side_to_string : side -> string
val side_of_string : string -> (side, string) result
