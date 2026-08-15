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

let execution_price order market_slice bar =
  match order.Order.request.kind with
  | Order.Market -> Some (bar.Bar.open_price, market_slice.Market_slice.start_at)
  | Order.Limit limit -> (
      match order.request.side with
      | Order.Buy ->
          if Scalar.Price.compare bar.open_price limit <= 0 then
            Some (bar.open_price, market_slice.start_at)
          else if Scalar.Price.compare bar.low_price limit <= 0 then
            Some (limit, market_slice.end_at)
          else None
      | Order.Sell ->
          if Scalar.Price.compare bar.open_price limit >= 0 then
            Some (bar.open_price, market_slice.start_at)
          else if Scalar.Price.compare bar.high_price limit >= 0 then
            Some (limit, market_slice.end_at)
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
      | Ok capacity ->
          Scalar.Quantity.round_down_to_multiple capacity
            ~multiple:instrument.Instrument.lot_size
          |> Result.map (fun capacity -> Limited capacity))

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

let compare_execution_order left right =
  let side_rank = function Order.Sell -> 0 | Order.Buy -> 1 in
  let side =
    Int.compare
      (side_rank left.Order.request.side)
      (side_rank right.Order.request.side)
  in
  if side <> 0 then side
  else
    let sequence =
      Int64.compare left.Order.created_sequence right.Order.created_sequence
    in
    if sequence <> 0 then sequence else Id.Order.compare left.id right.id

let fold_slice state ~instruments ~oms (market_slice : Market_slice.t) ~init
    ~apply =
  let ( let* ) result function_ =
    match result with Ok value -> function_ value | Error _ as error -> error
  in
  let instrument_map =
    List.fold_left
      (fun result instrument ->
        Id.Instrument.Map.add instrument.Instrument.id instrument result)
      Id.Instrument.Map.empty instruments
  in
  let prepare result bar =
    let* capacities = result in
    match Id.Instrument.Map.find_opt bar.Bar.instrument_id instrument_map with
    | None -> Error "market slice bar refers to an unknown instrument"
    | Some instrument ->
        let* () = validate_bar_prices instrument bar in
        let* capacity = initial_capacity state instrument bar in
        Ok (Id.Instrument.Map.add bar.instrument_id capacity capacities)
  in
  let* capacities =
    List.fold_left prepare (Ok Id.Instrument.Map.empty) market_slice.bars
  in
  let eligible =
    Oms.active_orders oms
    |> List.filter (fun order ->
        Int64.compare order.Order.eligible_after_slice_sequence
          market_slice.slice_sequence
        < 0
        && Ptime.compare order.created_at market_slice.start_at <= 0)
    |> List.sort compare_execution_order
  in
  let step result order =
    let* capacities, accumulator, market_orders = result in
    let market_orders =
      if Order.is_market order then order.Order.id :: market_orders
      else market_orders
    in
    let instrument_id = order.Order.request.instrument_id in
    match
      ( Market_slice.bar market_slice instrument_id,
        Id.Instrument.Map.find_opt instrument_id capacities,
        Id.Instrument.Map.find_opt instrument_id instrument_map )
    with
    | None, _, _ | _, None, _ | _, _, None ->
        Error "eligible order has no configured bar in the market slice"
    | Some bar, Some capacity, Some instrument -> (
        match execution_price order market_slice bar with
        | None -> Ok (capacities, accumulator, market_orders)
        | Some (price, executed_at) ->
            let quantity =
              available_quantity capacity (Order.remaining_quantity order)
            in
            if Scalar.Quantity.is_zero quantity then
              Ok (capacities, accumulator, market_orders)
            else
              let* notional = Scalar.Money.notional price quantity in
              let* fee =
                Scalar.Money.fee ~fixed:state.fixed_fee ~bps:state.fee_bps
                  ~notional
              in
              let proposed =
                { order_id = order.id; quantity; price; fee; executed_at }
              in
              let* accumulator, applied_quantity = apply accumulator proposed in
              if Scalar.Quantity.compare applied_quantity quantity > 0 then
                Error "applied fill quantity exceeds the execution proposal"
              else if
                not
                  (Scalar.Quantity.is_multiple applied_quantity
                     ~lot:instrument.Instrument.lot_size)
              then
                Error
                  "applied fill quantity is not aligned to the instrument lot \
                   size"
              else
                let* capacity = consume capacity applied_quantity in
                let capacities =
                  Id.Instrument.Map.add instrument_id capacity capacities
                in
                Ok (capacities, accumulator, market_orders))
  in
  let* _, accumulator, market_ioc_orders =
    List.fold_left step (Ok (capacities, init, [])) eligible
  in
  Ok (accumulator, List.rev market_ioc_orders)

let match_slice state ~instruments ~oms market_slice =
  let apply fills proposed = Ok (proposed :: fills, proposed.quantity) in
  match fold_slice state ~instruments ~oms market_slice ~init:[] ~apply with
  | Error _ as error -> error
  | Ok (fills, market_ioc_orders) ->
      Ok { fills = List.rev fills; market_ioc_orders }
