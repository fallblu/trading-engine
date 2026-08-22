type fee_configuration =
  | Legacy of { fixed_fee : Scalar.Money.t; fee_bps : int }
  | Schedules of Fee_schedule.t Id.Instrument.Map.t

type t = { participation_bps : int; fee_configuration : fee_configuration }

type proposed_fill = {
  order_id : Id.Order.t;
  quantity : Scalar.Quantity.t;
  price : Scalar.Price.t;
  fee : Scalar.Money.t;
  fee_components : Fee_schedule.calculated_component list;
  liquidity : Fee_schedule.liquidity;
  executed_at : Ptime.t;
}

type match_result = {
  fills : proposed_fill list;
  triggers : (Id.Order.t * Ptime.t * int64) list;
  market_ioc_orders : Id.Order.t list;
}

type capacity = Unlimited | Limited of Scalar.Quantity.t

type cursor = Cursor of (Oms.t -> (step, string) result)

and step =
  | Finished of Id.Order.t list
  | Triggered of Id.Order.t * Ptime.t * int64 * cursor
  | Proposed of proposed_fill * (Scalar.Quantity.t -> (cursor, string) result)

let cursor next = Cursor (fun oms -> next ~oms)

let create ~participation_bps ~fixed_fee ~fee_bps =
  if participation_bps < 0 || participation_bps > 10_000 then
    Error "participation basis points must be between 0 and 10000"
  else if Scalar.Money.compare fixed_fee Scalar.Money.zero < 0 then
    Error "fixed fee must be nonnegative"
  else if fee_bps < 0 || fee_bps > 10_000 then
    Error "fee basis points must be between 0 and 10000"
  else
    Ok { participation_bps; fee_configuration = Legacy { fixed_fee; fee_bps } }

let create_v2 ~participation_bps ~fee_schedules =
  if participation_bps < 0 || participation_bps > 10_000 then
    Error "participation basis points must be between 0 and 10000"
  else
    let add result schedule =
      let ( let* ) result function_ =
        match result with
        | Ok value -> function_ value
        | Error _ as error -> error
      in
      let* schedules = result in
      let instrument_id = Fee_schedule.instrument_id schedule in
      if Id.Instrument.Map.mem instrument_id schedules then
        Error "fee schedules must have unique instrument IDs"
      else Ok (Id.Instrument.Map.add instrument_id schedule schedules)
    in
    Result.map
      (fun schedules ->
        { participation_bps; fee_configuration = Schedules schedules })
      (List.fold_left add (Ok Id.Instrument.Map.empty) fee_schedules)

let participation_bps state = state.participation_bps

let fixed_fee state =
  match state.fee_configuration with
  | Legacy { fixed_fee; _ } -> fixed_fee
  | Schedules _ -> Scalar.Money.zero

let fee_bps state =
  match state.fee_configuration with
  | Legacy { fee_bps; _ } -> fee_bps
  | Schedules _ -> 0

let fee_schedules state =
  match state.fee_configuration with
  | Legacy _ -> []
  | Schedules schedules -> Id.Instrument.Map.bindings schedules |> List.map snd

let calculate_fee state ~instrument ~notional ~quantity ~liquidity ~fx_rates =
  match state.fee_configuration with
  | Legacy { fixed_fee; fee_bps } ->
      let ( let* ) result function_ =
        match result with
        | Ok value -> function_ value
        | Error _ as error -> error
      in
      let* fee = Scalar.Money.fee ~fixed:fixed_fee ~bps:fee_bps ~notional in
      Ok ([], fee)
  | Schedules schedules -> (
      match Id.Instrument.Map.find_opt instrument.Instrument.id schedules with
      | None -> Error "execution instrument has no configured fee schedule"
      | Some schedule ->
          Fee_schedule.calculate schedule
            ~quote_currency:instrument.quote_currency ~notional ~quantity
            ~liquidity ~fx_rates)

let execution_price order market_slice bar =
  match Order.effective_kind order with
  | None -> None
  | Some Order.Market ->
      Some
        ( bar.Bar.open_price,
          market_slice.Market_slice.start_at,
          Fee_schedule.Taker )
  | Some (Order.Limit limit) -> (
      match order.request.side with
      | Order.Buy ->
          if Scalar.Price.compare bar.open_price limit <= 0 then
            Some (bar.open_price, market_slice.start_at, Fee_schedule.Taker)
          else if Scalar.Price.compare bar.low_price limit <= 0 then
            Some (limit, market_slice.end_at, Fee_schedule.Maker)
          else None
      | Order.Sell ->
          if Scalar.Price.compare bar.open_price limit >= 0 then
            Some (bar.open_price, market_slice.start_at, Fee_schedule.Taker)
          else if Scalar.Price.compare bar.high_price limit >= 0 then
            Some (limit, market_slice.end_at, Fee_schedule.Maker)
          else None)
  | Some (Order.Stop _ | Order.Stop_limit _) -> None

let stop_trigger order market_slice bar =
  match (order.Order.request.kind, order.request.side) with
  | Order.Stop trigger_price, Order.Buy
  | Order.Stop_limit { trigger_price; _ }, Order.Buy ->
      if Scalar.Price.compare bar.Bar.open_price trigger_price >= 0 then
        Some market_slice.Market_slice.start_at
      else if Scalar.Price.compare bar.high_price trigger_price >= 0 then
        Some market_slice.end_at
      else None
  | Order.Stop trigger_price, Order.Sell
  | Order.Stop_limit { trigger_price; _ }, Order.Sell ->
      if Scalar.Price.compare bar.Bar.open_price trigger_price <= 0 then
        Some market_slice.Market_slice.start_at
      else if Scalar.Price.compare bar.low_price trigger_price <= 0 then
        Some market_slice.end_at
      else None
  | (Order.Market | Order.Limit _), _ -> None

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
          Scalar.Quantity.round_toward_zero_to_multiple capacity
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
  let origin_rank = function Order.Margin_liquidation -> 0 | _ -> 1 in
  let origin =
    Int.compare
      (origin_rank left.Order.request.origin)
      (origin_rank right.Order.request.origin)
  in
  let side_rank = function Order.Sell -> 0 | Order.Buy -> 1 in
  let side =
    Int.compare
      (side_rank left.Order.request.side)
      (side_rank right.Order.request.side)
  in
  if origin <> 0 then origin
  else if side <> 0 then side
  else
    let sequence =
      Int64.compare left.Order.created_sequence right.Order.created_sequence
    in
    if sequence <> 0 then sequence else Id.Order.compare left.id right.id

let start_slice state ~instruments ~oms (market_slice : Market_slice.t) =
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
        && Ptime.compare order.created_at market_slice.start_at <= 0
        &&
        match order.trigger_state with
        | Some (Order.Triggered { triggered_slice_sequence; _ }) ->
            Int64.compare triggered_slice_sequence market_slice.slice_sequence
            < 0
        | Some Order.Dormant | None -> true)
    |> List.sort compare_execution_order
  in
  let market_ioc_orders =
    List.filter_map
      (fun order ->
        if Order.is_ioc order && not (Order.is_dormant_stop order) then
          Some order.Order.id
        else None)
      eligible
  in
  let eligible_order_ids = List.map (fun order -> order.Order.id) eligible in
  let rec make_cursor capacities remaining =
    Cursor
      (fun current_oms ->
        match remaining with
        | [] -> Ok (Finished market_ioc_orders)
        | order_id :: remaining -> (
            match Oms.find current_oms order_id with
            | None -> Error "eligible order disappeared during matching"
            | Some order when not (Order.is_active order) ->
                let (Cursor next) = make_cursor capacities remaining in
                next current_oms
            | Some order -> propose capacities remaining current_oms order))
  and propose capacities remaining current_oms order =
    let instrument_id = order.Order.request.instrument_id in
    match
      ( Market_slice.bar market_slice instrument_id,
        Id.Instrument.Map.find_opt instrument_id capacities,
        Id.Instrument.Map.find_opt instrument_id instrument_map )
    with
    | None, _, _ | _, None, _ | _, _, None ->
        Error "eligible order has no configured bar in the market slice"
    | Some bar, Some capacity, Some instrument -> (
        if Order.is_dormant_stop order then
          match stop_trigger order market_slice bar with
          | None ->
              let (Cursor next) = make_cursor capacities remaining in
              next current_oms
          | Some triggered_at ->
              Ok
                (Triggered
                   ( order.id,
                     triggered_at,
                     market_slice.slice_sequence,
                     make_cursor capacities remaining ))
        else
          match execution_price order market_slice bar with
          | None ->
              let (Cursor next) = make_cursor capacities remaining in
              next current_oms
          | Some (price, executed_at, liquidity) ->
              let quantity =
                available_quantity capacity (Order.remaining_quantity order)
              in
              if
                Scalar.Quantity.is_zero quantity
                || Order.is_fok order
                   && Scalar.Quantity.compare quantity
                        (Order.remaining_quantity order)
                      < 0
              then
                let (Cursor next) = make_cursor capacities remaining in
                next current_oms
              else
                let* notional = Scalar.Money.notional price quantity in
                let* fee_components, fee =
                  calculate_fee state ~instrument ~notional ~quantity ~liquidity
                    ~fx_rates:
                      (List.map
                         (fun mark -> (mark.Market_slice.currency, mark.rate))
                         market_slice.fx_rates)
                in
                let proposed =
                  {
                    order_id = order.id;
                    quantity;
                    price;
                    fee;
                    fee_components;
                    liquidity;
                    executed_at;
                  }
                in
                let continue applied_quantity =
                  if
                    Scalar.Quantity.compare applied_quantity
                      Scalar.Quantity.zero
                    < 0
                  then Error "applied fill quantity must be nonnegative"
                  else if Scalar.Quantity.compare applied_quantity quantity > 0
                  then
                    Error "applied fill quantity exceeds the execution proposal"
                  else if
                    not
                      (Scalar.Quantity.is_multiple applied_quantity
                         ~lot:instrument.Instrument.lot_size)
                  then
                    Error
                      "applied fill quantity is not aligned to the instrument \
                       lot size"
                  else
                    let* capacity = consume capacity applied_quantity in
                    let capacities =
                      Id.Instrument.Map.add instrument_id capacity capacities
                    in
                    Ok (make_cursor capacities remaining)
                in
                Ok (Proposed (proposed, continue)))
  in
  Ok (make_cursor capacities eligible_order_ids)

let finished market_ioc_orders =
  cursor (fun ~oms:_ -> Ok (Finished market_ioc_orders))

let next (Cursor next) ~oms = next oms

let fold_slice state ~instruments ~oms market_slice ~init ~apply =
  let ( let* ) result function_ =
    match result with Ok value -> function_ value | Error _ as error -> error
  in
  let* cursor = start_slice state ~instruments ~oms market_slice in
  let rec fold accumulator cursor =
    match next cursor ~oms with
    | Error _ as error -> error
    | Ok (Finished market_ioc_orders) -> Ok (accumulator, market_ioc_orders)
    | Ok (Triggered _) ->
        Error "fold_slice cannot persist a triggered conditional order"
    | Ok (Proposed (proposed, continue)) ->
        let* accumulator, applied_quantity = apply accumulator proposed in
        let* cursor = continue applied_quantity in
        fold accumulator cursor
  in
  fold init cursor

let match_slice state ~instruments ~oms market_slice =
  let ( let* ) result function_ =
    match result with Ok value -> function_ value | Error _ as error -> error
  in
  let* cursor = start_slice state ~instruments ~oms market_slice in
  let rec collect fills triggers cursor =
    match next cursor ~oms with
    | Error _ as error -> error
    | Ok (Finished market_ioc_orders) ->
        Ok
          {
            fills = List.rev fills;
            triggers = List.rev triggers;
            market_ioc_orders;
          }
    | Ok (Triggered (order_id, triggered_at, slice_sequence, cursor)) ->
        collect fills
          ((order_id, triggered_at, slice_sequence) :: triggers)
          cursor
    | Ok (Proposed (proposed, continue)) ->
        let* cursor = continue proposed.quantity in
        collect (proposed :: fills) triggers cursor
  in
  collect [] [] cursor
