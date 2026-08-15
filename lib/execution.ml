type t = { participation_bps : int; fixed_fee : Scalar.Money.t; fee_bps : int }

type proposed_fill = {
  order_id : Id.Order.t;
  quantity : Scalar.Quantity.t;
  price : Scalar.Price.t;
  fee : Scalar.Money.t;
  executed_at : Ptime.t;
}

type match_result = {
  fills : proposed_fill list;
  market_ioc_orders : Id.Order.t list;
}

type capacity = Unlimited | Limited of Scalar.Quantity.t

let create ~participation_bps ~fixed_fee ~fee_bps =
  if participation_bps < 0 || participation_bps > 10_000 then
    Error "participation basis points must be between 0 and 10000"
  else if Scalar.Money.compare fixed_fee Scalar.Money.zero < 0 then
    Error "fixed fee must be nonnegative"
  else if fee_bps < 0 || fee_bps > 10_000 then
    Error "fee basis points must be between 0 and 10000"
  else Ok { participation_bps; fixed_fee; fee_bps }

let participation_bps state = state.participation_bps
let fixed_fee state = state.fixed_fee
let fee_bps state = state.fee_bps

let execution_price order bar =
  match order.Order.request.kind with
  | Order.Market -> Some (bar.Bar.open_price, bar.start_at)
  | Order.Limit limit -> (
      match order.request.side with
      | Order.Buy ->
          if Scalar.Price.compare bar.open_price limit <= 0 then
            Some (bar.open_price, bar.start_at)
          else if Scalar.Price.compare bar.low_price limit <= 0 then
            Some (limit, bar.end_at)
          else None
      | Order.Sell ->
          if Scalar.Price.compare bar.open_price limit >= 0 then
            Some (bar.open_price, bar.start_at)
          else if Scalar.Price.compare bar.high_price limit >= 0 then
            Some (limit, bar.end_at)
          else None)

let available_quantity capacity remaining =
  match capacity with
  | Unlimited -> remaining
  | Limited quantity -> Scalar.Quantity.minimum quantity remaining

let consume capacity quantity =
  match capacity with
  | Unlimited -> Ok Unlimited
  | Limited available -> (
      match Scalar.Quantity.subtract available quantity with
      | Error _ as error -> error
      | Ok remaining -> Ok (Limited remaining))

let initial_capacity state instrument bar =
  match bar.Bar.volume with
  | None -> Ok Unlimited
  | Some volume -> (
      match Scalar.Quantity.bps_floor volume ~bps:state.participation_bps with
      | Error _ as error -> error
      | Ok capacity -> (
          match
            Scalar.Quantity.round_down_to_multiple capacity
              ~multiple:instrument.Instrument.lot_size
          with
          | Error _ as error -> error
          | Ok capacity -> Ok (Limited capacity)))

let validate_bar_prices instrument bar =
  let tick = instrument.Instrument.tick_size in
  if not (Id.Instrument.equal instrument.id bar.Bar.instrument_id) then
    Error "execution instrument differs from the bar instrument"
  else if
    List.exists
      (fun price -> not (Scalar.Price.is_multiple price ~tick))
      [ bar.open_price; bar.high_price; bar.low_price; bar.close_price ]
  then Error "bar price is not aligned to the instrument tick size"
  else Ok ()

let match_bar state ~instrument ~oms bar =
  match validate_bar_prices instrument bar with
  | Error _ as error -> error
  | Ok () -> (
      match initial_capacity state instrument bar with
      | Error _ as error -> error
      | Ok initial_capacity -> (
          let eligible =
            Oms.active_for_instrument oms bar.Bar.instrument_id
            |> List.filter (fun order ->
                Int64.compare order.Order.eligible_after_bar_sequence
                  bar.source_sequence
                < 0
                && Ptime.compare order.created_at bar.start_at <= 0)
          in
          let step result order =
            match result with
            | Error _ as error -> error
            | Ok (capacity, fills, market_orders) -> (
                let market_orders =
                  if Order.is_market order then order.Order.id :: market_orders
                  else market_orders
                in
                match execution_price order bar with
                | None -> Ok (capacity, fills, market_orders)
                | Some (price, executed_at) -> (
                    let quantity =
                      available_quantity capacity
                        (Order.remaining_quantity order)
                    in
                    if Scalar.Quantity.is_zero quantity then
                      Ok (capacity, fills, market_orders)
                    else
                      match Scalar.Money.notional price quantity with
                      | Error _ as error -> error
                      | Ok notional -> (
                          match
                            Scalar.Money.fee ~fixed:state.fixed_fee
                              ~bps:state.fee_bps ~notional
                          with
                          | Error _ as error -> error
                          | Ok fee -> (
                              match consume capacity quantity with
                              | Error _ as error -> error
                              | Ok capacity ->
                                  let fill =
                                    {
                                      order_id = order.id;
                                      quantity;
                                      price;
                                      fee;
                                      executed_at;
                                    }
                                  in
                                  Ok (capacity, fill :: fills, market_orders))))
                )
          in
          match
            List.fold_left step (Ok (initial_capacity, [], [])) eligible
          with
          | Error _ as error -> error
          | Ok (_, fills, market_ioc_orders) ->
              Ok
                {
                  fills = List.rev fills;
                  market_ioc_orders = List.rev market_ioc_orders;
                }))
