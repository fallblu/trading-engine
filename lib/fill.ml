type t = {
  id : Id.Fill.t;
  order_id : Id.Order.t;
  instrument_id : Id.Instrument.t;
  side : Order.side;
  quantity : Scalar.Quantity.t;
  price : Scalar.Price.t;
  notional : Scalar.Money.t;
  fee : Scalar.Money.t;
  executed_at : Ptime.t;
  slice_sequence : int64;
}

let create ~id ~order_id ~instrument_id ~side ~quantity ~price ~fee ~executed_at
    ~slice_sequence =
  if Scalar.Quantity.is_zero quantity then
    Error "fill quantity must be positive"
  else if Scalar.Money.compare fee Scalar.Money.zero < 0 then
    Error "fill fee must be nonnegative"
  else if Int64.compare slice_sequence 0L <= 0 then
    Error "fill slice sequence must be positive"
  else
    match Scalar.Money.notional price quantity with
    | Error _ as error -> error
    | Ok notional ->
        Ok
          {
            id;
            order_id;
            instrument_id;
            side;
            quantity;
            price;
            notional;
            fee;
            executed_at;
            slice_sequence;
          }

let equal left right =
  Id.Fill.equal left.id right.id
  && Id.Order.equal left.order_id right.order_id
  && Id.Instrument.equal left.instrument_id right.instrument_id
  && left.side = right.side
  && Scalar.Quantity.equal left.quantity right.quantity
  && Scalar.Price.equal left.price right.price
  && Scalar.Money.equal left.notional right.notional
  && Scalar.Money.equal left.fee right.fee
  && Ptime.equal left.executed_at right.executed_at
  && Int64.equal left.slice_sequence right.slice_sequence

let pp formatter fill =
  Format.fprintf formatter "%a %s %a @ %a" Id.Fill.pp fill.id
    (Order.side_to_string fill.side)
    Scalar.Quantity.pp fill.quantity Scalar.Price.pp fill.price
