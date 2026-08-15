type t = {
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

let create ~source_sequence ~instrument_id ~start_at ~end_at ~available_at
    ~received_at ~open_price ~high_price ~low_price ~close_price ~volume =
  if Int64.compare source_sequence 0L < 0 then
    Error "bar source sequence must be nonnegative"
  else if Ptime.compare start_at end_at >= 0 then
    Error "bar start must precede its end"
  else if Ptime.compare available_at end_at < 0 then
    Error "bar availability must not precede its end"
  else if Ptime.compare received_at available_at < 0 then
    Error "bar receipt must not precede its availability"
  else if Scalar.Price.compare low_price high_price > 0 then
    Error "bar low must not exceed its high"
  else if
    Scalar.Price.compare open_price low_price < 0
    || Scalar.Price.compare open_price high_price > 0
  then Error "bar open must lie inside its low-high range"
  else if
    Scalar.Price.compare close_price low_price < 0
    || Scalar.Price.compare close_price high_price > 0
  then Error "bar close must lie inside its low-high range"
  else
    Ok
      {
        source_sequence;
        instrument_id;
        start_at;
        end_at;
        available_at;
        received_at;
        open_price;
        high_price;
        low_price;
        close_price;
        volume;
      }

let compare_replay_order left right =
  let receipt = Ptime.compare left.received_at right.received_at in
  if receipt <> 0 then receipt
  else Int64.compare left.source_sequence right.source_sequence

let pp formatter bar =
  Format.fprintf formatter "bar[%Ld] %a close=%a" bar.source_sequence
    Id.Instrument.pp bar.instrument_id Scalar.Price.pp bar.close_price
