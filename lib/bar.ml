type t = {
  instrument_id : Id.Instrument.t;
  open_price : Scalar.Price.t;
  high_price : Scalar.Price.t;
  low_price : Scalar.Price.t;
  close_price : Scalar.Price.t;
  volume : Scalar.Quantity.t option;
}

let create ~instrument_id ~open_price ~high_price ~low_price ~close_price
    ~volume =
  if Scalar.Price.compare low_price high_price > 0 then
    Error "bar low must not exceed its high"
  else if
    Scalar.Price.compare open_price low_price < 0
    || Scalar.Price.compare open_price high_price > 0
  then Error "bar open must lie inside its low-high range"
  else if
    Scalar.Price.compare close_price low_price < 0
    || Scalar.Price.compare close_price high_price > 0
  then Error "bar close must lie inside its low-high range"
  else if Option.exists (fun value -> Scalar.Quantity.is_negative value) volume
  then Error "bar volume must be nonnegative"
  else
    Ok { instrument_id; open_price; high_price; low_price; close_price; volume }

let pp formatter bar =
  Format.fprintf formatter "bar[%a] close=%a" Id.Instrument.pp bar.instrument_id
    Scalar.Price.pp bar.close_price
