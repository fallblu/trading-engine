type t = {
  id : Id.Fill.t;
  order_id : Id.Order.t;
  instrument_id : Id.Instrument.t;
  quote_currency : string;
  side : Order.side;
  quantity : Scalar.Quantity.t;
  price : Scalar.Price.t;
  notional : Scalar.Money.t;
  fee : Scalar.Money.t;
  fee_components : Fee_schedule.calculated_component list;
  executed_at : Ptime.t;
  slice_sequence : int64;
}

let create ~id ~order_id ~instrument_id ~quote_currency ~side ~quantity ~price
    ~fee ~fee_components ~executed_at ~slice_sequence =
  if not (Scalar.Quantity.is_positive quantity) then
    Error "fill quantity must be positive"
  else if String.length quote_currency = 0 then
    Error "fill quote currency must not be empty"
  else if
    not
      (String.for_all
         (fun character ->
           let code = Char.code character in
           code >= 0x21 && code <> 0x7f)
         quote_currency)
  then Error "fill quote currency must not contain whitespace"
  else if Int64.compare slice_sequence 0L <= 0 then
    Error "fill slice sequence must be positive"
  else
    match Scalar.Money.notional price quantity with
    | Error _ as error -> error
    | Ok notional when Scalar.Money.equal notional Scalar.Money.zero ->
        Error "fill notional must be at least one money micro-unit"
    | Ok notional ->
        let component_total =
          List.fold_left
            (fun result component ->
              Result.bind result (fun total ->
                  Scalar.Money.add total component.Fee_schedule.quote_amount))
            (Ok Scalar.Money.zero) fee_components
        in
        let component_total_valid =
          match component_total with
          | Ok total -> Scalar.Money.equal total fee
          | Error _ -> false
        in
        if fee_components <> [] && not component_total_valid then
          Error "fill fee components must sum to the fill fee"
        else
          Ok
            {
              id;
              order_id;
              instrument_id;
              quote_currency;
              side;
              quantity;
              price;
              notional;
              fee;
              fee_components;
              executed_at;
              slice_sequence;
            }

let equal left right =
  Id.Fill.equal left.id right.id
  && Id.Order.equal left.order_id right.order_id
  && Id.Instrument.equal left.instrument_id right.instrument_id
  && String.equal left.quote_currency right.quote_currency
  && left.side = right.side
  && Scalar.Quantity.equal left.quantity right.quantity
  && Scalar.Price.equal left.price right.price
  && Scalar.Money.equal left.notional right.notional
  && Scalar.Money.equal left.fee right.fee
  && left.fee_components = right.fee_components
  && Ptime.equal left.executed_at right.executed_at
  && Int64.equal left.slice_sequence right.slice_sequence

let pp formatter fill =
  Format.fprintf formatter "%a %s %a @ %a" Id.Fill.pp fill.id
    (Order.side_to_string fill.side)
    Scalar.Quantity.pp fill.quantity Scalar.Price.pp fill.price
