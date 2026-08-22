type fee_configuration =
  | Legacy of { fixed_fee : Scalar.Money.t; fee_bps : int }
  | Schedules of Fee_schedule.t Id.Instrument.Map.t

type missing_volume_policy = Reject_missing_volume | Zero_impact

type cost_model = {
  half_spread_bps : int;
  impact_coefficient_bps : int;
  missing_volume_policy : missing_volume_policy;
}

type t = {
  participation_bps : int;
  fee_configuration : fee_configuration;
  cost_model : cost_model option;
  book_depth_limit : int option;
}

type price_attribution = {
  reference_price : Scalar.Price.t;
  spread_adjustment : Scalar.Money.t;
  impact_adjustment : Scalar.Money.t;
  final_price : Scalar.Price.t;
}

type proposed_fill = {
  order_id : Id.Order.t;
  quantity : Scalar.Quantity.t;
  price : Scalar.Price.t;
  fee : Scalar.Money.t;
  fee_components : Fee_schedule.calculated_component list;
  liquidity : Fee_schedule.liquidity;
  executed_at : Ptime.t;
  price_attribution : price_attribution option;
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

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let cursor next = Cursor (fun oms -> next ~oms)

let create ~participation_bps ~fixed_fee ~fee_bps =
  if participation_bps < 0 || participation_bps > 10_000 then
    Error "participation basis points must be between 0 and 10000"
  else if Scalar.Money.compare fixed_fee Scalar.Money.zero < 0 then
    Error "fixed fee must be nonnegative"
  else if fee_bps < 0 || fee_bps > 10_000 then
    Error "fee basis points must be between 0 and 10000"
  else
    Ok
      {
        participation_bps;
        fee_configuration = Legacy { fixed_fee; fee_bps };
        cost_model = None;
        book_depth_limit = None;
      }

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
        {
          participation_bps;
          fee_configuration = Schedules schedules;
          cost_model = None;
          book_depth_limit = None;
        })
      (List.fold_left add (Ok Id.Instrument.Map.empty) fee_schedules)

let create_conservative ~participation_bps ~fee_schedules ~half_spread_bps
    ~impact_coefficient_bps ~missing_volume_policy =
  if half_spread_bps < 0 || half_spread_bps > 10_000 then
    Error "half-spread basis points must be between 0 and 10000"
  else if impact_coefficient_bps < 0 || impact_coefficient_bps > 10_000 then
    Error "impact coefficient basis points must be between 0 and 10000"
  else
    Result.map
      (fun state ->
        {
          state with
          cost_model =
            Some
              { half_spread_bps; impact_coefficient_bps; missing_volume_policy };
        })
      (create_v2 ~participation_bps ~fee_schedules)

let create_order_book ~participation_bps ~fee_schedules ~max_depth_levels =
  if max_depth_levels <= 0 || max_depth_levels > 1024 then
    Error "order-book depth limit must be between 1 and 1024"
  else
    Result.map
      (fun state -> { state with book_depth_limit = Some max_depth_levels })
      (create_v2 ~participation_bps ~fee_schedules)

let participation_bps state = state.participation_bps
let book_depth_limit state = state.book_depth_limit

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

let cost_model state = state.cost_model

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

type limit_fill_policy = Optimistic_touch | Next_open_only | Adverse_touch

let checked_price_micros value =
  if Z.fits_int64 value then Scalar.Price.of_micros (Z.to_int64 value)
  else Error "execution price overflow"

let adverse_reference side limit tick =
  let limit = Z.of_int64 (Scalar.Price.to_micros limit) in
  let tick = Z.of_int64 (Scalar.Price.to_micros tick) in
  checked_price_micros
    (match side with Order.Buy -> Z.sub limit tick | Sell -> Z.add limit tick)

let execution_reference policy instrument order market_slice bar =
  match Order.effective_kind order with
  | None -> None
  | Some Order.Market ->
      Some
        ( bar.Bar.open_price,
          market_slice.Market_slice.start_at,
          Fee_schedule.Taker )
  | Some (Order.Limit limit) -> (
      match order.request.side with
      | Order.Buy -> (
          if Scalar.Price.compare bar.open_price limit <= 0 then
            Some (bar.open_price, market_slice.start_at, Fee_schedule.Taker)
          else
            match policy with
            | Optimistic_touch ->
                if Scalar.Price.compare bar.low_price limit <= 0 then
                  Some (limit, market_slice.end_at, Fee_schedule.Maker)
                else None
            | Next_open_only -> None
            | Adverse_touch -> (
                match
                  adverse_reference Order.Buy limit
                    instrument.Instrument.tick_size
                with
                | Error _ -> None
                | Ok reference ->
                    if Scalar.Price.compare bar.low_price reference <= 0 then
                      Some (reference, market_slice.end_at, Fee_schedule.Maker)
                    else None))
      | Order.Sell -> (
          if Scalar.Price.compare bar.open_price limit >= 0 then
            Some (bar.open_price, market_slice.start_at, Fee_schedule.Taker)
          else
            match policy with
            | Optimistic_touch ->
                if Scalar.Price.compare bar.high_price limit >= 0 then
                  Some (limit, market_slice.end_at, Fee_schedule.Maker)
                else None
            | Next_open_only -> None
            | Adverse_touch -> (
                match
                  adverse_reference Order.Sell limit instrument.tick_size
                with
                | Error _ -> None
                | Ok reference ->
                    if Scalar.Price.compare bar.high_price reference >= 0 then
                      Some (reference, market_slice.end_at, Fee_schedule.Maker)
                    else None)))
  | Some (Order.Stop _ | Order.Stop_limit _) -> None

let ceil_div numerator denominator =
  if Z.equal numerator Z.zero then Z.zero
  else Z.div (Z.add numerator (Z.pred denominator)) denominator

let round_up_to_tick value tick = Z.mul (ceil_div value tick) tick

let price_adjustment reference bps =
  ceil_div
    (Z.mul (Z.of_int64 (Scalar.Price.to_micros reference)) (Z.of_int bps))
    (Z.of_int 10_000)

let impact_adjustment reference coefficient quantity volume =
  ceil_div
    (Z.mul
       (Z.mul
          (Z.of_int64 (Scalar.Price.to_micros reference))
          (Z.of_int coefficient))
       (Z.of_int64 (Scalar.Quantity.to_micros quantity)))
    (Z.mul (Z.of_int 10_000) (Z.of_int64 (Scalar.Quantity.to_micros volume)))

let apply_cost_model state instrument order bar quantity reference =
  match state.cost_model with
  | None -> Ok (Some (reference, None))
  | Some model ->
      let tick =
        Z.of_int64 (Scalar.Price.to_micros instrument.Instrument.tick_size)
      in
      let spread =
        price_adjustment reference model.half_spread_bps |> fun value ->
        round_up_to_tick value tick
      in
      let* impact =
        if model.impact_coefficient_bps = 0 then Ok Z.zero
        else
          match bar.Bar.volume with
          | Some volume when not (Scalar.Quantity.is_zero volume) ->
              Ok
                ( impact_adjustment reference model.impact_coefficient_bps
                    quantity volume
                |> fun value -> round_up_to_tick value tick )
          | Some _ | None -> (
              match model.missing_volume_policy with
              | Reject_missing_volume ->
                  Error "impact model requires completed-bar volume"
              | Zero_impact -> Ok Z.zero)
      in
      let adjustment = Z.add spread impact in
      let reference_micros = Z.of_int64 (Scalar.Price.to_micros reference) in
      let final_micros =
        match order.Order.request.side with
        | Buy -> Z.add reference_micros adjustment
        | Sell -> Z.sub reference_micros adjustment
      in
      let* final_price = checked_price_micros final_micros in
      let respects_limit =
        match Order.effective_kind order with
        | Some (Order.Limit limit) -> (
            match order.request.side with
            | Buy -> Scalar.Price.compare final_price limit <= 0
            | Sell -> Scalar.Price.compare final_price limit >= 0)
        | Some (Market | Stop _ | Stop_limit _) | None -> true
      in
      if not respects_limit then Ok None
      else if not (Z.fits_int64 spread && Z.fits_int64 impact) then
        Error "execution price adjustment overflow"
      else
        Ok
          (Some
             ( final_price,
               Some
                 {
                   reference_price = reference;
                   spread_adjustment =
                     Scalar.Money.of_micros (Z.to_int64 spread);
                   impact_adjustment =
                     Scalar.Money.of_micros (Z.to_int64 impact);
                   final_price;
                 } ))

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
  let origin_rank = function
    | Order.Margin_liquidation -> 0
    | Order.Borrow_recall -> 1
    | Order.Direct | Order.Target_rebalance -> 2
  in
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

let start_slice_with_policy policy state ~instruments ~oms
    (market_slice : Market_slice.t) =
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
          match
            execution_reference policy instrument order market_slice bar
          with
          | None ->
              let (Cursor next) = make_cursor capacities remaining in
              next current_oms
          | Some (reference_price, executed_at, liquidity) -> (
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
                let* priced =
                  apply_cost_model state instrument order bar quantity
                    reference_price
                in
                match priced with
                | None ->
                    let (Cursor next) = make_cursor capacities remaining in
                    next current_oms
                | Some (price, price_attribution) ->
                    let* notional = Scalar.Money.notional price quantity in
                    let* fee_components, fee =
                      calculate_fee state ~instrument ~notional ~quantity
                        ~liquidity
                        ~fx_rates:
                          (List.map
                             (fun mark ->
                               (mark.Market_slice.currency, mark.rate))
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
                        price_attribution;
                      }
                    in
                    let continue applied_quantity =
                      if
                        Scalar.Quantity.compare applied_quantity
                          Scalar.Quantity.zero
                        < 0
                      then Error "applied fill quantity must be nonnegative"
                      else if
                        Scalar.Quantity.compare applied_quantity quantity > 0
                      then
                        Error
                          "applied fill quantity exceeds the execution proposal"
                      else if
                        not
                          (Scalar.Quantity.is_multiple applied_quantity
                             ~lot:instrument.Instrument.lot_size)
                      then
                        Error
                          "applied fill quantity is not aligned to the \
                           instrument lot size"
                      else
                        let* capacity = consume capacity applied_quantity in
                        let capacities =
                          Id.Instrument.Map.add instrument_id capacity
                            capacities
                        in
                        Ok (make_cursor capacities remaining)
                    in
                    Ok (Proposed (proposed, continue))))
  in
  Ok (make_cursor capacities eligible_order_ids)

let start_slice state = start_slice_with_policy Optimistic_touch state
let start_slice_next_open state = start_slice_with_policy Next_open_only state

let start_slice_adverse_touch state =
  start_slice_with_policy Adverse_touch state

type observable_liquidity =
  | Quote_liquidity of { bid : Scalar.Quantity.t; ask : Scalar.Quantity.t }
  | Trade_liquidity of Scalar.Quantity.t

let event_capacity state instrument quantity =
  let* capacity =
    Scalar.Quantity.bps_floor quantity ~bps:state.participation_bps
  in
  Scalar.Quantity.round_toward_zero_to_multiple capacity
    ~multiple:instrument.Instrument.lot_size

let validate_market_event instrument (event : Market_event.t) =
  let tick = instrument.Instrument.tick_size in
  let aligned = function
    | Market_event.Quote { bid_price; ask_price; _ } ->
        Scalar.Price.is_multiple bid_price ~tick
        && Scalar.Price.is_multiple ask_price ~tick
    | Trade { price; _ } -> Scalar.Price.is_multiple price ~tick
  in
  if not (Id.Instrument.equal instrument.id event.instrument_id) then
    Error "execution instrument differs from the market event instrument"
  else if not (aligned event.kind) then
    Error "market event price is not aligned to the instrument tick size"
  else Ok ()

let event_trigger order (event : Market_event.t) =
  let observed_price =
    match (event.kind, order.Order.request.side) with
    | Market_event.Quote { ask_price; _ }, Order.Buy -> ask_price
    | Quote { bid_price; _ }, Sell -> bid_price
    | Trade { price; _ }, _ -> price
  in
  match (order.request.kind, order.request.side) with
  | Order.Stop trigger, Buy | Stop_limit { trigger_price = trigger; _ }, Buy ->
      Scalar.Price.compare observed_price trigger >= 0
  | Order.Stop trigger, Sell | Stop_limit { trigger_price = trigger; _ }, Sell
    ->
      Scalar.Price.compare observed_price trigger <= 0
  | (Market | Limit _), _ -> false

let event_opportunity order (event : Market_event.t) liquidity =
  match
    (event.kind, liquidity, Order.effective_kind order, order.request.side)
  with
  | Quote { ask_price; _ }, Quote_liquidity { ask; _ }, Some Market, Buy ->
      Some (ask_price, ask, Fee_schedule.Taker)
  | Quote { bid_price; _ }, Quote_liquidity { bid; _ }, Some Market, Sell ->
      Some (bid_price, bid, Fee_schedule.Taker)
  | Quote { ask_price; _ }, Quote_liquidity { ask; _ }, Some (Limit limit), Buy
    when Scalar.Price.compare ask_price limit <= 0 ->
      Some (ask_price, ask, Fee_schedule.Taker)
  | Quote { bid_price; _ }, Quote_liquidity { bid; _ }, Some (Limit limit), Sell
    when Scalar.Price.compare bid_price limit >= 0 ->
      Some (bid_price, bid, Fee_schedule.Taker)
  | ( Trade { price; aggressor_side = Market_event.Sell; _ },
      Trade_liquidity quantity,
      Some (Limit limit),
      Buy )
    when Scalar.Price.compare price limit <= 0 ->
      Some (price, quantity, Fee_schedule.Maker)
  | ( Trade { price; aggressor_side = Market_event.Buy; _ },
      Trade_liquidity quantity,
      Some (Limit limit),
      Sell )
    when Scalar.Price.compare price limit >= 0 ->
      Some (price, quantity, Fee_schedule.Maker)
  | _ -> None

let consume_observable side liquidity quantity =
  match liquidity with
  | Quote_liquidity { bid; ask } ->
      if side = Order.Buy then
        Result.map
          (fun ask -> Quote_liquidity { bid; ask })
          (Scalar.Quantity.subtract ask quantity)
      else
        Result.map
          (fun bid -> Quote_liquidity { bid; ask })
          (Scalar.Quantity.subtract bid quantity)
  | Trade_liquidity available ->
      Result.map
        (fun value -> Trade_liquidity value)
        (Scalar.Quantity.subtract available quantity)

let start_slice_quote_trade state ~instruments ~oms
    (market_slice : Market_slice.t) =
  let instrument_map =
    List.fold_left
      (fun map instrument ->
        Id.Instrument.Map.add instrument.Instrument.id instrument map)
      Id.Instrument.Map.empty instruments
  in
  let prepare_event event =
    match
      Id.Instrument.Map.find_opt event.Market_event.instrument_id instrument_map
    with
    | None -> Error "market event refers to an unknown instrument"
    | Some instrument ->
        let* () = validate_market_event instrument event in
        if
          Ptime.compare event.event_at market_slice.start_at < 0
          || Ptime.compare event.event_at market_slice.end_at > 0
          || Ptime.compare event.received_at market_slice.received_at > 0
        then Error "market event falls outside its observable slice boundary"
        else
          let* liquidity =
            match event.kind with
            | Market_event.Quote { bid_quantity; ask_quantity; _ } ->
                let* bid = event_capacity state instrument bid_quantity in
                let* ask = event_capacity state instrument ask_quantity in
                Ok (Quote_liquidity { bid; ask })
            | Trade { quantity; _ } ->
                Result.map
                  (fun value -> Trade_liquidity value)
                  (event_capacity state instrument quantity)
          in
          Ok (event, instrument, liquidity)
  in
  let* events =
    List.fold_right
      (fun event result ->
        let* prepared = prepare_event event in
        let* remaining = result in
        Ok (prepared :: remaining))
      market_slice.market_events (Ok [])
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
  let order_ids = List.map (fun order -> order.Order.id) eligible in
  let market_ioc_orders =
    eligible
    |> List.filter_map (fun order ->
        if Order.is_ioc order && not (Order.is_dormant_stop order) then
          Some order.Order.id
        else None)
  in
  let rec make_events = function
    | [] -> cursor (fun ~oms:_ -> Ok (Finished market_ioc_orders))
    | (event, instrument, liquidity) :: remaining_events ->
        make_orders event instrument liquidity order_ids remaining_events
  and make_orders event instrument liquidity remaining remaining_events =
    Cursor
      (fun current_oms ->
        match remaining with
        | [] ->
            let (Cursor next) = make_events remaining_events in
            next current_oms
        | order_id :: remaining_orders -> (
            match Oms.find current_oms order_id with
            | None -> Error "eligible order disappeared during market replay"
            | Some order when not (Order.is_active order) ->
                let (Cursor next) =
                  make_orders event instrument liquidity remaining_orders
                    remaining_events
                in
                next current_oms
            | Some order
              when not
                     (Id.Instrument.equal order.request.instrument_id
                        event.Market_event.instrument_id) ->
                let (Cursor next) =
                  make_orders event instrument liquidity remaining_orders
                    remaining_events
                in
                next current_oms
            | Some order when Order.is_dormant_stop order ->
                let continuation =
                  make_orders event instrument liquidity remaining_orders
                    remaining_events
                in
                if event_trigger order event then
                  Ok
                    (Triggered
                       ( order.id,
                         event.event_at,
                         market_slice.slice_sequence,
                         continuation ))
                else
                  let (Cursor next) = continuation in
                  next current_oms
            | Some order -> (
                match event_opportunity order event liquidity with
                | None ->
                    let (Cursor next) =
                      make_orders event instrument liquidity remaining_orders
                        remaining_events
                    in
                    next current_oms
                | Some (price, available, fee_liquidity) ->
                    let quantity =
                      Scalar.Quantity.minimum available
                        (Order.remaining_quantity order)
                    in
                    if
                      Scalar.Quantity.is_zero quantity
                      || Order.is_fok order
                         && Scalar.Quantity.compare quantity
                              (Order.remaining_quantity order)
                            < 0
                    then
                      let (Cursor next) =
                        make_orders event instrument liquidity remaining_orders
                          remaining_events
                      in
                      next current_oms
                    else
                      let* notional = Scalar.Money.notional price quantity in
                      let* fee_components, fee =
                        calculate_fee state ~instrument ~notional ~quantity
                          ~liquidity:fee_liquidity
                          ~fx_rates:
                            (List.map
                               (fun mark ->
                                 (mark.Market_slice.currency, mark.rate))
                               market_slice.fx_rates)
                      in
                      let proposed =
                        {
                          order_id = order.id;
                          quantity;
                          price;
                          fee;
                          fee_components;
                          liquidity = fee_liquidity;
                          executed_at = event.event_at;
                          price_attribution = None;
                        }
                      in
                      let continue applied_quantity =
                        if Scalar.Quantity.compare applied_quantity quantity > 0
                        then
                          Error
                            "applied fill quantity exceeds observable liquidity"
                        else if
                          Scalar.Quantity.compare applied_quantity
                            Scalar.Quantity.zero
                          < 0
                        then Error "applied fill quantity must be nonnegative"
                        else if
                          not
                            (Scalar.Quantity.is_multiple applied_quantity
                               ~lot:instrument.Instrument.lot_size)
                        then
                          Error
                            "applied fill quantity is not aligned to the \
                             instrument lot size"
                        else
                          let* liquidity =
                            consume_observable order.request.side liquidity
                              applied_quantity
                          in
                          Ok
                            (make_orders event instrument liquidity
                               remaining_orders remaining_events)
                      in
                      Ok (Proposed (proposed, continue)))))
  in
  Ok (make_events events)

type book_state = {
  book_sequence : int64;
  bids : Order_book_event.level list;
  asks : Order_book_event.level list;
}

type book_view =
  | Book_snapshot of book_state
  | Book_added of Order_book_event.side * Scalar.Price.t * Scalar.Quantity.t
  | Book_reduced of Order_book_event.side * Scalar.Price.t * Scalar.Quantity.t
  | Book_trade of
      Scalar.Price.t * Scalar.Quantity.t * Market_event.aggressor_side

let book_level_quantity price levels =
  List.find_opt
    (fun (level : Order_book_event.level) ->
      Scalar.Price.compare level.price price = 0)
    levels
  |> Option.map (fun level -> level.Order_book_event.quantity)
  |> Option.value ~default:Scalar.Quantity.zero

let sort_book_levels side levels =
  List.sort
    (fun (left : Order_book_event.level) right ->
      let comparison = Scalar.Price.compare left.price right.price in
      match side with
      | Order_book_event.Bid -> -comparison
      | Order_book_event.Ask -> comparison)
    levels

let replace_book_level side price quantity levels =
  let level = Order_book_event.level ~price ~quantity |> Result.get_ok in
  level
  :: List.filter
       (fun (existing : Order_book_event.level) ->
         Scalar.Price.compare existing.price price <> 0)
       levels
  |> sort_book_levels side

let remove_book_level price levels =
  List.filter
    (fun (level : Order_book_event.level) ->
      Scalar.Price.compare level.price price <> 0)
    levels

let consume_book_levels price quantity levels =
  let rec consume reversed = function
    | [] -> Error "order-book execution level disappeared"
    | (level : Order_book_event.level) :: remaining ->
        if Scalar.Price.compare level.price price <> 0 then
          consume (level :: reversed) remaining
        else if Scalar.Quantity.compare quantity level.quantity > 0 then
          Error "applied fill quantity exceeds order-book liquidity"
        else if Scalar.Quantity.compare quantity level.quantity = 0 then
          Ok (List.rev_append reversed remaining)
        else
          let* remaining_quantity =
            Scalar.Quantity.subtract level.quantity quantity
          in
          let* level =
            Order_book_event.level ~price:level.price
              ~quantity:remaining_quantity
          in
          Ok (List.rev_append reversed (level :: remaining))
  in
  consume [] levels

let consume_book_view order price quantity = function
  | Book_snapshot book -> (
      match order.Order.request.side with
      | Buy ->
          Result.map
            (fun asks -> Book_snapshot { book with asks })
            (consume_book_levels price quantity book.asks)
      | Sell ->
          Result.map
            (fun bids -> Book_snapshot { book with bids })
            (consume_book_levels price quantity book.bids))
  | Book_added (side, added_price, available)
    when Scalar.Price.compare price added_price = 0 ->
      if Scalar.Quantity.compare quantity available > 0 then
        Error "applied fill quantity exceeds order-book liquidity"
      else if Scalar.Quantity.compare quantity available = 0 then
        Ok (Book_added (side, added_price, Scalar.Quantity.zero))
      else
        let* remaining = Scalar.Quantity.subtract available quantity in
        Ok (Book_added (side, added_price, remaining))
  | view -> Ok view

let valid_book depth_limit book =
  List.length book.bids <= depth_limit
  && List.length book.asks <= depth_limit
  &&
  match (book.bids, book.asks) with
  | bid :: _, ask :: _ -> Scalar.Price.compare bid.price ask.price <= 0
  | _ -> true

let consume_feed_trade aggressor price quantity book =
  let eligible level =
    match aggressor with
    | Market_event.Buy ->
        Scalar.Price.compare level.Order_book_event.price price <= 0
    | Sell -> Scalar.Price.compare level.price price >= 0
    | Unknown -> false
  in
  let rec consume remaining consumed = function
    | levels when Scalar.Quantity.is_zero remaining ->
        Ok (List.rev_append consumed levels)
    | level :: levels when eligible level ->
        if Scalar.Quantity.compare level.quantity remaining <= 0 then
          let* remaining = Scalar.Quantity.subtract remaining level.quantity in
          consume remaining consumed levels
        else
          let* quantity = Scalar.Quantity.subtract level.quantity remaining in
          let* level = Order_book_event.level ~price:level.price ~quantity in
          Ok (List.rev_append consumed (level :: levels))
    | _ -> Error "order-book trade exceeds observable depth"
  in
  match aggressor with
  | Market_event.Buy ->
      Result.map
        (fun asks -> { book with asks })
        (consume quantity [] book.asks)
  | Sell ->
      Result.map
        (fun bids -> { book with bids })
        (consume quantity [] book.bids)
  | Unknown -> Ok book

let start_slice_order_book state ~instruments ~oms
    (market_slice : Market_slice.t) =
  let depth_limit = Option.value state.book_depth_limit ~default:0 in
  if depth_limit = 0 then Error "order-book execution configuration is required"
  else
    let instrument_map =
      List.fold_left
        (fun map instrument ->
          Id.Instrument.Map.add instrument.Instrument.id instrument map)
        Id.Instrument.Map.empty instruments
    in
    let validate_event instrument (event : Order_book_event.t) =
      let prices, quantities =
        match event.kind with
        | Snapshot { bids; asks } ->
            ( List.map (fun level -> level.Order_book_event.price) (bids @ asks),
              List.map
                (fun level -> level.Order_book_event.quantity)
                (bids @ asks) )
        | Set { price; quantity; _ } -> ([ price ], [ quantity ])
        | Delete { price; _ } -> ([ price ], [])
        | Trade { price; quantity; _ } -> ([ price ], [ quantity ])
      in
      if
        not
          (List.for_all
             (fun price ->
               Scalar.Price.is_multiple price
                 ~tick:instrument.Instrument.tick_size)
             prices)
      then Error "order-book price is not aligned to the instrument tick size"
      else if
        not
          (List.for_all
             (fun quantity ->
               Scalar.Quantity.is_multiple quantity ~lot:instrument.lot_size)
             quantities)
      then Error "order-book quantity is not aligned to the instrument lot size"
      else if
        Ptime.compare event.event_at market_slice.start_at < 0
        || Ptime.compare event.event_at market_slice.end_at > 0
        || Ptime.compare event.available_at market_slice.available_at > 0
        || Ptime.compare event.received_at market_slice.received_at > 0
      then Error "order-book event falls outside its observable slice boundary"
      else Ok ()
    in
    let prepare (books, prepared) (event : Order_book_event.t) =
      let* instrument =
        match Id.Instrument.Map.find_opt event.instrument_id instrument_map with
        | Some instrument -> Ok instrument
        | None -> Error "order-book event refers to an unknown instrument"
      in
      let* () = validate_event instrument event in
      let prior = Id.Instrument.Map.find_opt event.instrument_id books in
      let* book, view =
        match (prior, event.kind) with
        | None, Snapshot { bids; asks } ->
            let book = { book_sequence = event.book_sequence; bids; asks } in
            if valid_book depth_limit book then Ok (book, Book_snapshot book)
            else Error "order-book snapshot exceeds depth or crosses"
        | Some _, Snapshot _ ->
            Error "order-book bundle contains more than one snapshot"
        | None, _ -> Error "order-book bundle must begin with a snapshot"
        | Some prior, kind -> (
            if Int64.succ prior.book_sequence <> event.book_sequence then
              Error "order-book sequences must be contiguous"
            else
              let next_sequence book =
                { book with book_sequence = event.book_sequence }
              in
              match kind with
              | Set { side; price; quantity } ->
                  let levels =
                    match side with
                    | Bid -> prior.bids
                    | Order_book_event.Ask -> prior.asks
                  in
                  let old_quantity = book_level_quantity price levels in
                  let levels = replace_book_level side price quantity levels in
                  let book =
                    match side with
                    | Order_book_event.Bid ->
                        next_sequence { prior with bids = levels }
                    | Order_book_event.Ask ->
                        next_sequence { prior with asks = levels }
                  in
                  if not (valid_book depth_limit book) then
                    Error "order-book update exceeds depth or crosses"
                  else if Scalar.Quantity.compare quantity old_quantity > 0 then
                    let* added =
                      Scalar.Quantity.subtract quantity old_quantity
                    in
                    Ok (book, Book_added (side, price, added))
                  else
                    let* removed =
                      Scalar.Quantity.subtract old_quantity quantity
                    in
                    Ok (book, Book_reduced (side, price, removed))
              | Delete { side; price } ->
                  let levels =
                    match side with
                    | Bid -> prior.bids
                    | Order_book_event.Ask -> prior.asks
                  in
                  let old_quantity = book_level_quantity price levels in
                  if Scalar.Quantity.is_zero old_quantity then
                    Error "order-book delete refers to a missing level"
                  else
                    let levels = remove_book_level price levels in
                    let book =
                      match side with
                      | Order_book_event.Bid ->
                          next_sequence { prior with bids = levels }
                      | Order_book_event.Ask ->
                          next_sequence { prior with asks = levels }
                    in
                    Ok (book, Book_reduced (side, price, old_quantity))
              | Trade { price; quantity; aggressor_side } ->
                  let* book =
                    consume_feed_trade aggressor_side price quantity prior
                  in
                  Ok
                    ( next_sequence book,
                      Book_trade (price, quantity, aggressor_side) )
              | Snapshot _ -> assert false)
      in
      Ok
        ( Id.Instrument.Map.add event.instrument_id book books,
          (event, instrument, view) :: prepared )
    in
    let* books, reversed =
      List.fold_left
        (fun result event ->
          Result.bind result (fun state -> prepare state event))
        (Ok (Id.Instrument.Map.empty, []))
        market_slice.order_book_events
    in
    let expected =
      List.map (fun instrument -> instrument.Instrument.id) instruments
      |> Id.Instrument.Set.of_list
    in
    let observed =
      Id.Instrument.Map.fold
        (fun instrument_id _ ids -> Id.Instrument.Set.add instrument_id ids)
        books Id.Instrument.Set.empty
    in
    if not (Id.Instrument.Set.equal expected observed) then
      Error "order-book snapshots must cover every configured instrument"
    else
      let events = List.rev reversed in
      let eligible =
        Oms.active_orders oms
        |> List.filter (fun order ->
            Int64.compare order.Order.eligible_after_slice_sequence
              market_slice.slice_sequence
            < 0
            && Ptime.compare order.created_at market_slice.start_at <= 0)
        |> List.sort compare_execution_order
      in
      let order_ids = List.map (fun order -> order.Order.id) eligible in
      let market_ioc_orders =
        List.filter_map
          (fun order ->
            if Order.is_ioc order && not (Order.is_dormant_stop order) then
              Some order.Order.id
            else None)
          eligible
      in
      let queue_for_snapshot queues instrument book =
        let rec build prior queues = function
          | [] -> Ok queues
          | order :: remaining -> (
              match Order.effective_kind order with
              | Some (Order.Limit limit)
                when Id.Instrument.equal order.request.instrument_id
                       instrument.Instrument.id ->
                  let opposite =
                    match order.request.side with
                    | Buy -> book.asks
                    | Sell -> book.bids
                  in
                  let marketable =
                    match opposite with
                    | [] -> false
                    | best :: _ -> (
                        match order.request.side with
                        | Buy -> Scalar.Price.compare best.price limit <= 0
                        | Sell -> Scalar.Price.compare best.price limit >= 0)
                  in
                  if marketable then build (order :: prior) queues remaining
                  else
                    let same_side =
                      match order.request.side with
                      | Buy -> book.bids
                      | Sell -> book.asks
                    in
                    let external_quantity =
                      book_level_quantity limit same_side
                    in
                    let* ahead =
                      List.fold_left
                        (fun result earlier ->
                          let* ahead = result in
                          match Order.effective_kind earlier with
                          | Some (Order.Limit earlier_limit)
                            when earlier.request.side = order.request.side
                                 && Id.Instrument.equal
                                      earlier.request.instrument_id
                                      order.request.instrument_id
                                 && Scalar.Price.compare earlier_limit limit = 0
                            ->
                              Scalar.Quantity.add ahead
                                (Order.remaining_quantity earlier)
                          | _ -> Ok ahead)
                        (Ok external_quantity) prior
                    in
                    build (order :: prior)
                      (Id.Order.Map.add order.id ahead queues)
                      remaining
              | _ -> build (order :: prior) queues remaining)
        in
        build [] queues eligible
      in
      let order_matches_level order side price =
        match Order.effective_kind order with
        | Some Order.Market ->
            (order.request.side = Buy && side = Order_book_event.Ask)
            || (order.request.side = Sell && side = Order_book_event.Bid)
        | Some (Limit limit) ->
            order.request.side = Buy
            && side = Order_book_event.Ask
            && Scalar.Price.compare price limit <= 0
            || order.request.side = Sell
               && side = Order_book_event.Bid
               && Scalar.Price.compare price limit >= 0
        | _ -> false
      in
      let levels_for_order order view =
        match view with
        | Book_snapshot book ->
            let levels =
              match order.Order.request.side with
              | Buy -> book.asks
              | Sell -> book.bids
            in
            List.filter
              (fun level ->
                order_matches_level order
                  (match order.request.side with
                  | Buy -> Order_book_event.Ask
                  | Sell -> Order_book_event.Bid)
                  level.Order_book_event.price)
              levels
        | Book_added (side, price, quantity)
          when order_matches_level order side price
               && not (Scalar.Quantity.is_zero quantity) ->
            [ Order_book_event.level ~price ~quantity |> Result.get_ok ]
        | _ -> []
      in
      let reduce_queue queues instrument_id side price removed =
        Id.Order.Map.mapi
          (fun order_id ahead ->
            match Oms.find oms order_id with
            | Some order
              when Id.Instrument.equal order.request.instrument_id instrument_id
                   && (match order.request.side with
                     | Buy -> side = Order_book_event.Bid
                     | Sell -> side = Order_book_event.Ask)
                   &&
                   match Order.effective_kind order with
                   | Some (Limit limit) -> Scalar.Price.compare limit price = 0
                   | _ -> false ->
                if Scalar.Quantity.compare removed ahead >= 0 then
                  Scalar.Quantity.zero
                else Scalar.Quantity.subtract ahead removed |> Result.get_ok
            | _ -> ahead)
          queues
      in
      let trade_allowances queues instrument_id price quantity aggressor =
        Id.Order.Map.fold
          (fun order_id ahead (queues, allowances) ->
            match Oms.find oms order_id with
            | Some order
              when Id.Instrument.equal order.request.instrument_id instrument_id
                   && (match (order.request.side, aggressor) with
                     | Buy, Market_event.Sell | Sell, Buy -> true
                     | _ -> false)
                   &&
                   match Order.effective_kind order with
                   | Some (Limit limit) -> (
                       match order.request.side with
                       | Buy -> Scalar.Price.compare price limit <= 0
                       | Sell -> Scalar.Price.compare price limit >= 0)
                   | _ -> false ->
                let next_ahead =
                  if Scalar.Quantity.compare quantity ahead >= 0 then
                    Scalar.Quantity.zero
                  else Scalar.Quantity.subtract ahead quantity |> Result.get_ok
                in
                let through =
                  if Scalar.Quantity.compare quantity ahead <= 0 then
                    Scalar.Quantity.zero
                  else Scalar.Quantity.subtract quantity ahead |> Result.get_ok
                in
                ( Id.Order.Map.add order_id next_ahead queues,
                  Id.Order.Map.add order_id through allowances )
            | _ -> (queues, allowances))
          queues
          (queues, Id.Order.Map.empty)
      in
      let rec make_events queues = function
        | [] -> Ok (cursor (fun ~oms:_ -> Ok (Finished market_ioc_orders)))
        | ((event : Order_book_event.t), instrument, view) :: remaining_events
          ->
            let* queues, allowances =
              match view with
              | Book_snapshot book ->
                  Result.map
                    (fun queues -> (queues, Id.Order.Map.empty))
                    (queue_for_snapshot queues instrument book)
              | Book_reduced (side, price, removed) ->
                  Ok
                    ( reduce_queue queues event.instrument_id side price removed,
                      Id.Order.Map.empty )
              | Book_trade (price, quantity, aggressor) ->
                  Ok
                    (trade_allowances queues event.instrument_id price quantity
                       aggressor)
              | Book_added _ -> Ok (queues, Id.Order.Map.empty)
            in
            Ok
              (make_orders queues allowances event instrument view order_ids
                 remaining_events)
      and make_orders queues allowances event instrument view remaining
          remaining_events =
        Cursor
          (fun current_oms ->
            match remaining with
            | [] ->
                let* cursor = make_events queues remaining_events in
                let (Cursor next) = cursor in
                next current_oms
            | order_id :: remaining_orders -> (
                match Oms.find current_oms order_id with
                | None ->
                    Error "eligible order disappeared during order-book replay"
                | Some order when not (Order.is_active order) ->
                    let (Cursor next) =
                      make_orders queues allowances event instrument view
                        remaining_orders remaining_events
                    in
                    next current_oms
                | Some order
                  when not
                         (Id.Instrument.equal order.request.instrument_id
                            event.Order_book_event.instrument_id) ->
                    let (Cursor next) =
                      make_orders queues allowances event instrument view
                        remaining_orders remaining_events
                    in
                    next current_oms
                | Some order when Order.is_dormant_stop order ->
                    let observed =
                      match view with
                      | Book_snapshot book -> (
                          match order.request.side with
                          | Buy -> (
                              match book.asks with
                              | level :: _ -> Some level.Order_book_event.price
                              | [] -> None)
                          | Sell -> (
                              match book.bids with
                              | level :: _ -> Some level.Order_book_event.price
                              | [] -> None))
                      | Book_added (_, price, _)
                      | Book_reduced (_, price, _)
                      | Book_trade (price, _, _) ->
                          Some price
                    in
                    let triggered =
                      match
                        (order.request.kind, order.request.side, observed)
                      with
                      | ( ( Stop trigger
                          | Stop_limit { trigger_price = trigger; _ } ),
                          Buy,
                          Some price ) ->
                          Scalar.Price.compare price trigger >= 0
                      | ( ( Stop trigger
                          | Stop_limit { trigger_price = trigger; _ } ),
                          Sell,
                          Some price ) ->
                          Scalar.Price.compare price trigger <= 0
                      | _ -> false
                    in
                    let continuation =
                      make_orders queues allowances event instrument view
                        remaining_orders remaining_events
                    in
                    if triggered then
                      Ok
                        (Triggered
                           ( order.id,
                             event.event_at,
                             market_slice.slice_sequence,
                             continuation ))
                    else
                      let (Cursor next) = continuation in
                      next current_oms
                | Some order -> (
                    let levels = levels_for_order order view in
                    let passive = Id.Order.Map.find_opt order.id allowances in
                    let* fok_capacity =
                      List.fold_left
                        (fun result (level : Order_book_event.level) ->
                          let* total = result in
                          let* capacity =
                            event_capacity state instrument level.quantity
                          in
                          Scalar.Quantity.add total capacity)
                        (Ok Scalar.Quantity.zero) levels
                    in
                    let opportunity =
                      match (levels, passive, view) with
                      | level :: _, _, _ ->
                          Some
                            ( level.price,
                              level.quantity,
                              Fee_schedule.Taker,
                              true )
                      | [], Some quantity, Book_trade (price, _, _) ->
                          Some (price, quantity, Fee_schedule.Maker, false)
                      | _ -> None
                    in
                    match opportunity with
                    | None ->
                        let (Cursor next) =
                          make_orders queues allowances event instrument view
                            remaining_orders remaining_events
                        in
                        next current_oms
                    | Some (price, available, fee_liquidity, repeat_order) ->
                        let* capacity =
                          event_capacity state instrument available
                        in
                        let quantity =
                          Scalar.Quantity.minimum capacity
                            (Order.remaining_quantity order)
                        in
                        if
                          Scalar.Quantity.is_zero quantity
                          || Order.is_fok order
                             && Scalar.Quantity.compare
                                  (if levels = [] then capacity
                                   else fok_capacity)
                                  (Order.remaining_quantity order)
                                < 0
                        then
                          let (Cursor next) =
                            make_orders queues allowances event instrument view
                              remaining_orders remaining_events
                          in
                          next current_oms
                        else
                          let* notional =
                            Scalar.Money.notional price quantity
                          in
                          let* fee_components, fee =
                            calculate_fee state ~instrument ~notional ~quantity
                              ~liquidity:fee_liquidity
                              ~fx_rates:
                                (List.map
                                   (fun mark ->
                                     (mark.Market_slice.currency, mark.rate))
                                   market_slice.fx_rates)
                          in
                          let proposed =
                            {
                              order_id = order.id;
                              quantity;
                              price;
                              fee;
                              fee_components;
                              liquidity = fee_liquidity;
                              executed_at = event.event_at;
                              price_attribution = None;
                            }
                          in
                          let continue applied_quantity =
                            if
                              Scalar.Quantity.compare applied_quantity quantity
                              > 0
                            then
                              Error
                                "applied fill quantity exceeds order-book \
                                 liquidity"
                            else if
                              Scalar.Quantity.compare applied_quantity
                                Scalar.Quantity.zero
                              < 0
                            then
                              Error "applied fill quantity must be nonnegative"
                            else if
                              not
                                (Scalar.Quantity.is_multiple applied_quantity
                                   ~lot:instrument.Instrument.lot_size)
                            then
                              Error
                                "applied fill quantity is not aligned to the \
                                 instrument lot size"
                            else
                              let* next_view =
                                if repeat_order then
                                  consume_book_view order price applied_quantity
                                    view
                                else Ok view
                              in
                              let next_orders =
                                if
                                  repeat_order
                                  && not
                                       (Scalar.Quantity.is_zero applied_quantity)
                                then order_id :: remaining_orders
                                else remaining_orders
                              in
                              Ok
                                (make_orders queues allowances event instrument
                                   next_view next_orders remaining_events)
                          in
                          Ok (Proposed (proposed, continue)))))
      in
      make_events Id.Order.Map.empty events

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
