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

type t = {
  instrument_id : Id.Instrument.t;
  event_at : Ptime.t;
  available_at : Ptime.t;
  received_at : Ptime.t;
  ingest_sequence : int64;
  kind : kind;
}

let validate_common ~event_at ~available_at ~received_at ~ingest_sequence =
  if Int64.compare ingest_sequence 0L <= 0 then
    Error "market event ingest sequence must be positive"
  else if Ptime.compare available_at event_at < 0 then
    Error "market event availability must not precede event time"
  else if Ptime.compare received_at available_at < 0 then
    Error "market event receipt must not precede availability"
  else Ok ()

let quote ~instrument_id ~event_at ~available_at ~received_at ~ingest_sequence
    ~bid_price ~bid_quantity ~ask_price ~ask_quantity =
  let ( let* ) result function_ = Result.bind result function_ in
  let* () =
    validate_common ~event_at ~available_at ~received_at ~ingest_sequence
  in
  if Scalar.Price.compare bid_price ask_price >= 0 then
    Error "quote bid price must be below ask price"
  else if
    Scalar.Quantity.is_zero bid_quantity || Scalar.Quantity.is_zero ask_quantity
  then Error "quote quantities must be positive"
  else
    Ok
      {
        instrument_id;
        event_at;
        available_at;
        received_at;
        ingest_sequence;
        kind = Quote { bid_price; bid_quantity; ask_price; ask_quantity };
      }

let trade ~instrument_id ~event_at ~available_at ~received_at ~ingest_sequence
    ~price ~quantity ~aggressor_side =
  let ( let* ) result function_ = Result.bind result function_ in
  let* () =
    validate_common ~event_at ~available_at ~received_at ~ingest_sequence
  in
  if Scalar.Quantity.is_zero quantity then
    Error "trade quantity must be positive"
  else
    Ok
      {
        instrument_id;
        event_at;
        available_at;
        received_at;
        ingest_sequence;
        kind = Trade { price; quantity; aggressor_side };
      }

let compare_replay_order left right =
  let availability = Ptime.compare left.available_at right.available_at in
  if availability <> 0 then availability
  else
    let receipt = Ptime.compare left.received_at right.received_at in
    if receipt <> 0 then receipt
    else Int64.compare left.ingest_sequence right.ingest_sequence

let aggressor_side_to_string = function
  | Buy -> "buy"
  | Sell -> "sell"
  | Unknown -> "unknown"

let aggressor_side_of_string = function
  | "buy" -> Ok Buy
  | "sell" -> Ok Sell
  | "unknown" -> Ok Unknown
  | _ -> Error "trade aggressor_side must be buy, sell, or unknown"
