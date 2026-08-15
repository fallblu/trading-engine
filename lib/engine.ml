type config = {
  risk : Risk.t;
  execution : Execution.t;
  max_internal_events : int;
}

let config ~risk ~execution ~max_internal_events =
  if max_internal_events <= 0 then
    Error "maximum internal events must be positive"
  else Ok { risk; execution; max_internal_events }

let valid_sha256 value =
  String.length value = 64
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       value

module Make (Strategy_impl : Strategy.S) = struct
  type t = {
    run_id : Id.Run.t;
    scenario_sha256 : string;
    config : config;
    engine_sequence : int64;
    next_order_number : int64;
    next_fill_number : int64;
    last_slice_sequence : int64 option;
    last_slice_end : Ptime.t option;
    last_received_at : Ptime.t option;
    latest_bars : Bar.t Id.Instrument.Map.t;
    desired_targets : Scalar.Quantity.t Id.Instrument.Map.t option;
    account : Account.t;
    oms : Oms.t;
    strategy_state : Strategy_impl.state;
    started : bool;
    completed : bool;
  }

  type pending =
    | Notify of Strategy.context * Strategy.event
    | Act of Strategy.intent

  type reduction = {
    state : t;
    now : Ptime.t;
    current_slice_sequence : int64;
    audits_rev : Audit.t list;
    pending : pending list;
    processed : int;
  }

  let create ~run_id ~scenario_sha256 ~config ~initial_cash ~strategy_state =
    if Scalar.Money.compare initial_cash Scalar.Money.zero < 0 then
      Error "initial cash must be nonnegative"
    else if not (valid_sha256 scenario_sha256) then
      Error "scenario SHA-256 must contain 64 lowercase hexadecimal characters"
    else
      Ok
        {
          run_id;
          scenario_sha256;
          config;
          engine_sequence = 0L;
          next_order_number = 1L;
          next_fill_number = 1L;
          last_slice_sequence = None;
          last_slice_end = None;
          last_received_at = None;
          latest_bars = Id.Instrument.Map.empty;
          desired_targets = None;
          account = Account.create ~initial_cash;
          oms = Oms.empty;
          strategy_state;
          started = false;
          completed = false;
        }

  let account state = state.account
  let oms state = state.oms

  let latest_bar state instrument_id =
    Id.Instrument.Map.find_opt instrument_id state.latest_bars

  let strategy_state state = state.strategy_state

  let next_sequence value =
    if Int64.equal value Int64.max_int then Error "engine sequence is exhausted"
    else Ok (Int64.succ value)

  let emit reduction event =
    match next_sequence reduction.state.engine_sequence with
    | Error _ as error -> error
    | Ok engine_sequence ->
        let audit =
          Audit.create ~engine_sequence ~run_id:reduction.state.run_id
            ~recorded_at:reduction.now event
        in
        let state = { reduction.state with engine_sequence } in
        Ok { reduction with state; audits_rev = audit :: reduction.audits_rev }

  let ensure_started reduction =
    if reduction.state.started then Ok reduction
    else
      let state = { reduction.state with started = true } in
      emit { reduction with state }
        (Audit.Run_started { scenario_sha256 = reduction.state.scenario_sha256 })

  let enqueue reduction items =
    { reduction with pending = reduction.pending @ items }

  let strategy_context state now =
    let latest_bars =
      Id.Instrument.Map.bindings state.latest_bars |> List.map snd
    in
    Strategy.context ~now ~account:state.account
      ~working_orders:(Oms.active_orders state.oms)
      ~latest_bars

  let notification reduction event =
    Notify (strategy_context reduction.state reduction.now, event)

  let order_id state =
    let value =
      Printf.sprintf "%s-order-%012Ld"
        (Id.Run.to_string state.run_id)
        state.next_order_number
    in
    Id.Order.of_string_exn value

  let fill_id state =
    let value =
      Printf.sprintf "%s-fill-%012Ld"
        (Id.Run.to_string state.run_id)
        state.next_fill_number
    in
    Id.Fill.of_string_exn value

  let increment_order_number state =
    match next_sequence state.next_order_number with
    | Error _ as error -> error
    | Ok next_order_number -> Ok { state with next_order_number }

  let increment_fill_number state =
    match next_sequence state.next_fill_number with
    | Error _ as error -> error
    | Ok next_fill_number -> Ok { state with next_fill_number }

  let reject_intent reduction reason =
    match emit reduction (Audit.Intent_rejected reason) with
    | Error _ as error -> error
    | Ok reduction ->
        Ok
          (enqueue reduction
             [ notification reduction (Strategy.Intent_rejected reason) ])

  let submit_order reduction request =
    let id = order_id reduction.state in
    match increment_order_number reduction.state with
    | Error _ as error -> error
    | Ok numbered_state -> (
        let reduction = { reduction with state = numbered_state } in
        let event_sequence_result =
          next_sequence reduction.state.engine_sequence
        in
        match event_sequence_result with
        | Error _ as error -> error
        | Ok order_sequence -> (
            match
              Risk.check reduction.state.config.risk
                ~account:reduction.state.account ~oms:reduction.state.oms
                request
            with
            | Ok () -> (
                match
                  Oms.accept reduction.state.oms ~id
                    ~accepted_sequence:order_sequence ~created_at:reduction.now
                    ~eligible_after_slice_sequence:
                      reduction.current_slice_sequence request
                with
                | Error _ as error -> error
                | Ok (oms, order) -> (
                    let reduction =
                      { reduction with state = { reduction.state with oms } }
                    in
                    match emit reduction (Audit.Order_accepted order) with
                    | Error _ as error -> error
                    | Ok reduction ->
                        Ok
                          (enqueue reduction
                             [
                               notification reduction
                                 (Strategy.Order_updated order);
                             ])))
            | Error reason -> (
                match
                  Oms.reject reduction.state.oms ~id
                    ~rejected_sequence:order_sequence ~created_at:reduction.now
                    ~eligible_after_slice_sequence:
                      reduction.current_slice_sequence request ~reason
                with
                | Error _ as error -> error
                | Ok (oms, order) -> (
                    let reduction =
                      { reduction with state = { reduction.state with oms } }
                    in
                    match emit reduction (Audit.Order_rejected order) with
                    | Error _ as error -> error
                    | Ok reduction ->
                        Ok
                          (enqueue reduction
                             [
                               notification reduction
                                 (Strategy.Order_updated order);
                               notification reduction
                                 (Strategy.Intent_rejected reason);
                             ])))))

  let cancel_order reduction ~reason order_id =
    match Oms.cancel reduction.state.oms order_id with
    | Error message -> reject_intent reduction message
    | Ok (oms, order) -> (
        let reduction =
          { reduction with state = { reduction.state with oms } }
        in
        match emit reduction (Audit.Order_cancelled { order; reason }) with
        | Error _ as error -> error
        | Ok reduction ->
            Ok
              (enqueue reduction
                 [ notification reduction (Strategy.Order_updated order) ]))

  let rec cancel_orders reduction ~reason = function
    | [] -> Ok reduction
    | order_id :: remaining -> (
        match cancel_order reduction ~reason order_id with
        | Error _ as error -> error
        | Ok reduction -> cancel_orders reduction ~reason remaining)

  let configured_instruments state =
    Risk.instruments state.config.risk
    |> List.sort (fun left right ->
        Id.Instrument.compare left.Instrument.id right.Instrument.id)

  let validate_target_ids state ids =
    let expected =
      configured_instruments state
      |> List.map (fun instrument -> instrument.Instrument.id)
    in
    let actual = List.sort_uniq Id.Instrument.compare ids in
    if List.length actual <> List.length ids then
      Error "target portfolio must contain each instrument exactly once"
    else if expected <> actual then
      Error "target portfolio must cover every configured instrument"
    else Ok ()

  let target_quantities state (targets : Strategy.quantity_target list) =
    let ids =
      List.map
        (fun (target : Strategy.quantity_target) -> target.instrument_id)
        targets
    in
    let ( let* ) result function_ =
      match result with
      | Ok value -> function_ value
      | Error _ as error -> error
    in
    let* () = validate_target_ids state ids in
    let add result (target : Strategy.quantity_target) =
      let* desired, requested = result in
      match Risk.instrument state.config.risk target.Strategy.instrument_id with
      | None -> Error "target quantity refers to an unknown instrument"
      | Some instrument ->
          if
            not
              (Scalar.Quantity.is_multiple target.quantity
                 ~lot:instrument.Instrument.lot_size)
          then Error "target quantity is not aligned to its instrument lot"
          else if
            Scalar.Quantity.compare target.quantity
              (Risk.max_position state.config.risk)
            > 0
          then Error "target quantity exceeds the maximum position"
          else
            Ok
              ( Id.Instrument.Map.add target.instrument_id target.quantity
                  desired,
                Audit.
                  {
                    instrument_id = target.instrument_id;
                    weight = None;
                    quantity = target.quantity;
                    reference_price = None;
                  }
                :: requested )
    in
    let* desired, requested =
      List.fold_left add (Ok (Id.Instrument.Map.empty, [])) targets
    in
    Ok (desired, List.rev requested)

  let target_weights state (targets : Strategy.weight_target list) =
    let ids =
      List.map
        (fun (target : Strategy.weight_target) -> target.instrument_id)
        targets
    in
    let ( let* ) result function_ =
      match result with
      | Ok value -> function_ value
      | Error _ as error -> error
    in
    let* () = validate_target_ids state ids in
    let* total_weight =
      List.fold_left
        (fun result (target : Strategy.weight_target) ->
          let* total = result in
          Scalar.Weight.add total target.Strategy.weight)
        (Ok Scalar.Weight.zero) targets
    in
    if Scalar.Weight.compare total_weight Scalar.Weight.one > 0 then
      Error "target weights must sum to at most one"
    else
      let* valuation =
        let marks =
          Id.Instrument.Map.bindings state.latest_bars
          |> List.map (fun (instrument_id, bar) ->
              (instrument_id, bar.Bar.close_price))
        in
        Account.value state.account ~marks
      in
      let add result (target : Strategy.weight_target) =
        let* desired, requested = result in
        match
          ( Risk.instrument state.config.risk target.Strategy.instrument_id,
            Id.Instrument.Map.find_opt target.instrument_id state.latest_bars )
        with
        | None, _ -> Error "target weight refers to an unknown instrument"
        | _, None -> Error "target weight has no current market price"
        | Some instrument, Some bar ->
            let* quantity =
              Planner.quantity_from_weight ~equity:valuation.equity
                ~weight:target.weight ~price:bar.close_price
                ~lot_size:instrument.lot_size
            in
            if
              Scalar.Quantity.compare quantity
                (Risk.max_position state.config.risk)
              > 0
            then Error "weight-derived target exceeds the maximum position"
            else
              Ok
                ( Id.Instrument.Map.add target.instrument_id quantity desired,
                  Audit.
                    {
                      instrument_id = target.instrument_id;
                      weight = Some target.weight;
                      quantity;
                      reference_price = Some bar.close_price;
                    }
                  :: requested )
      in
      let* desired, requested =
        List.fold_left add (Ok (Id.Instrument.Map.empty, [])) targets
      in
      Ok (desired, List.rev requested)

  let replace_targets reduction basis desired requested =
    match
      emit reduction
        (Audit.Target_portfolio_requested { basis; targets = requested })
    with
    | Error _ as error -> error
    | Ok reduction -> (
        let target_orders =
          Oms.active_orders reduction.state.oms
          |> List.filter (fun order ->
              order.Order.request.origin = Order.Target_rebalance)
          |> List.map (fun order -> order.Order.id)
        in
        match
          cancel_orders reduction ~reason:Audit.Target_replaced target_orders
        with
        | Error _ as error -> error
        | Ok reduction ->
            Ok
              {
                reduction with
                state = { reduction.state with desired_targets = Some desired };
              })

  let set_quantity_targets reduction targets =
    match target_quantities reduction.state targets with
    | Error reason -> reject_intent reduction reason
    | Ok (desired, requested) ->
        replace_targets reduction Audit.Quantities desired requested

  let set_weight_targets reduction targets =
    match target_weights reduction.state targets with
    | Error reason -> reject_intent reduction reason
    | Ok (desired, requested) ->
        replace_targets reduction Audit.Weights desired requested

  let metric reduction name value =
    if String.length name = 0 || String.trim name <> name then
      reject_intent reduction "metric name must be a nonempty trimmed string"
    else emit reduction (Audit.Metric_emitted { name; value })

  let handle_intent reduction = function
    | Strategy.Target_weights targets -> set_weight_targets reduction targets
    | Strategy.Target_quantities targets ->
        set_quantity_targets reduction targets
    | Strategy.Submit_order request ->
        if request.Order.origin <> Order.Direct then
          reject_intent reduction
            "direct strategy submissions must use direct order origin"
        else submit_order reduction request
    | Strategy.Cancel_order order_id ->
        cancel_order reduction ~reason:Audit.Strategy_requested order_id
    | Strategy.Emit_metric { name; value } -> metric reduction name value

  let handle_notification reduction context event =
    let strategy_state, intents =
      Strategy_impl.on_event reduction.state.strategy_state context event
    in
    let state = { reduction.state with strategy_state } in
    let actions = List.map (fun intent -> Act intent) intents in
    Ok (enqueue { reduction with state } actions)

  let rec drain reduction =
    match reduction.pending with
    | [] -> Ok reduction
    | _ when reduction.processed >= reduction.state.config.max_internal_events
      ->
        Error "maximum internal event count exceeded"
    | item :: pending -> (
        let reduction =
          { reduction with pending; processed = reduction.processed + 1 }
        in
        let result =
          match item with
          | Notify (context, event) ->
              handle_notification reduction context event
          | Act intent -> handle_intent reduction intent
        in
        match result with
        | Error _ as error -> error
        | Ok reduction -> drain reduction)

  let validate_slice state market_slice =
    let expected =
      configured_instruments state
      |> List.map (fun instrument -> instrument.Instrument.id)
    in
    let ids =
      List.map (fun bar -> bar.Bar.instrument_id) market_slice.Market_slice.bars
    in
    let actual = List.sort_uniq Id.Instrument.compare ids in
    if List.length ids <> List.length actual || actual <> expected then
      Error "market slice must contain each configured instrument exactly once"
    else
      match state.last_slice_sequence with
      | Some sequence
        when Int64.compare market_slice.slice_sequence sequence <= 0 ->
          Error "market slice sequence must increase"
      | _ -> (
          match state.last_slice_end with
          | Some end_at when Ptime.compare market_slice.end_at end_at <= 0 ->
              Error "market slice end must increase"
          | _ -> (
              match state.last_received_at with
              | Some received_at
                when Ptime.compare market_slice.received_at received_at < 0 ->
                  Error "market slice receipt time must not move backward"
              | _ -> Ok ()))

  let fill_cost execution price quantity =
    match Scalar.Money.notional price quantity with
    | Error _ as error -> error
    | Ok notional -> (
        match
          Scalar.Money.fee
            ~fixed:(Execution.fixed_fee execution)
            ~bps:(Execution.fee_bps execution)
            ~notional
        with
        | Error _ as error -> error
        | Ok fee -> (
            match Scalar.Money.add notional fee with
            | Error _ as error -> error
            | Ok cost -> Ok (fee, cost)))

  let affordable_quantity state instrument price requested =
    let cash = Account.cash state.account in
    let lot = instrument.Instrument.lot_size in
    let lot_value = Scalar.Quantity.to_int64 lot in
    let requested_lots =
      Int64.div (Scalar.Quantity.to_int64 requested) lot_value
    in
    let affordable lots =
      let quantity_value = Int64.mul lots lot_value in
      let quantity = Scalar.Quantity.of_int64 quantity_value |> Result.get_ok in
      match fill_cost state.config.execution price quantity with
      | Ok (_, cost) -> Scalar.Money.compare cost cash <= 0
      | Error _ -> false
    in
    let rec search low high =
      if Int64.compare low high >= 0 then low
      else
        let difference = Int64.sub high low in
        let upper_half =
          Int64.add (Int64.div difference 2L) (Int64.rem difference 2L)
        in
        let middle = Int64.add low upper_half in
        if affordable middle then search middle high
        else search low (Int64.pred middle)
    in
    let lots = search 0L requested_lots in
    Scalar.Quantity.of_int64 (Int64.mul lots lot_value)

  let apply_fill reduction market_slice proposed quantity fee =
    match Oms.find reduction.state.oms proposed.Execution.order_id with
    | None -> Error "execution proposal refers to an unknown order"
    | Some order -> (
        let id = fill_id reduction.state in
        match increment_fill_number reduction.state with
        | Error _ as error -> error
        | Ok state -> (
            let reduction = { reduction with state } in
            match
              Fill.create ~id ~order_id:order.id
                ~instrument_id:order.request.instrument_id
                ~side:order.request.side ~quantity ~price:proposed.price ~fee
                ~executed_at:proposed.executed_at
                ~slice_sequence:market_slice.Market_slice.slice_sequence
            with
            | Error _ as error -> error
            | Ok fill -> (
                match Oms.apply_fill reduction.state.oms fill with
                | Error _ as error -> error
                | Ok (_, Oms.Duplicate) ->
                    Error "newly allocated fill ID was duplicated"
                | Ok (oms, Oms.Applied order) -> (
                    match Account.apply_fill reduction.state.account fill with
                    | Error _ as error -> error
                    | Ok account -> (
                        let state = { reduction.state with oms; account } in
                        let reduction = { reduction with state } in
                        match emit reduction (Audit.Fill_applied fill) with
                        | Error _ as error -> error
                        | Ok reduction ->
                            Ok
                              (enqueue reduction
                                 [
                                   notification reduction
                                     (Strategy.Fill_received fill);
                                   notification reduction
                                     (Strategy.Order_updated order);
                                 ]))))))

  let apply_proposed_fill reduction market_slice proposed =
    match Oms.find reduction.state.oms proposed.Execution.order_id with
    | None -> Error "execution proposal refers to an unknown order"
    | Some order -> (
        match order.Order.request.side with
        | Order.Sell ->
            apply_fill reduction market_slice proposed proposed.quantity
              proposed.fee
        | Order.Buy -> (
            match
              Risk.instrument reduction.state.config.risk
                order.request.instrument_id
            with
            | None -> Error "execution order refers to an unknown instrument"
            | Some instrument -> (
                match
                  affordable_quantity reduction.state instrument proposed.price
                    proposed.quantity
                with
                | Error _ as error -> error
                | Ok affordable_quantity -> (
                    let clipped =
                      Scalar.Quantity.compare affordable_quantity
                        proposed.quantity
                      < 0
                    in
                    let limited =
                      if clipped then
                        emit reduction
                          (Audit.Cash_limited
                             {
                               order_id = order.id;
                               instrument_id = order.request.instrument_id;
                               requested_quantity = proposed.quantity;
                               affordable_quantity;
                               price = proposed.price;
                             })
                      else Ok reduction
                    in
                    match limited with
                    | Error _ as error -> error
                    | Ok reduction -> (
                        if Scalar.Quantity.is_zero affordable_quantity then
                          Ok reduction
                        else
                          match
                            fill_cost reduction.state.config.execution
                              proposed.price affordable_quantity
                          with
                          | Error _ as error -> error
                          | Ok (fee, _) ->
                              apply_fill reduction market_slice proposed
                                affordable_quantity fee)))))

  let rec apply_fills reduction market_slice = function
    | [] -> Ok reduction
    | proposed :: remaining -> (
        match apply_proposed_fill reduction market_slice proposed with
        | Error _ as error -> error
        | Ok reduction -> apply_fills reduction market_slice remaining)

  let rec cancel_market_remainders reduction = function
    | [] -> Ok reduction
    | order_id :: remaining -> (
        match Oms.find reduction.state.oms order_id with
        | None -> Error "market IOC order disappeared during matching"
        | Some order -> (
            let result =
              if Order.is_active order then
                cancel_order reduction ~reason:Audit.Market_ioc order_id
              else Ok reduction
            in
            match result with
            | Error _ as error -> error
            | Ok reduction -> cancel_market_remainders reduction remaining))

  let value state =
    let marks =
      Id.Instrument.Map.bindings state.latest_bars
      |> List.map (fun (instrument_id, bar) ->
          (instrument_id, bar.Bar.close_price))
    in
    Account.value state.account ~marks

  let valuation reduction =
    match value reduction.state with
    | Error _ as error -> error
    | Ok valuation -> emit reduction (Audit.Valuation valuation)

  let bounded_target_request state instrument_id target =
    let active = Oms.active_for_instrument state.oms instrument_id in
    let direct =
      List.filter
        (fun order -> order.Order.request.origin = Order.Direct)
        active
    in
    let target_orders =
      List.filter
        (fun order -> order.Order.request.origin = Order.Target_rebalance)
        active
    in
    if direct <> [] || target_orders <> [] then Ok None
    else
      let current = Account.position_quantity state.account instrument_id in
      if Scalar.Quantity.equal current target then Ok None
      else
        let side, delta =
          if Scalar.Quantity.compare target current > 0 then
            (Order.Buy, Scalar.Quantity.subtract target current)
          else (Order.Sell, Scalar.Quantity.subtract current target)
        in
        match delta with
        | Error _ as error -> error
        | Ok delta -> (
            match Risk.instrument state.config.risk instrument_id with
            | None -> Error "target refers to an unknown instrument"
            | Some instrument -> (
                let bounded =
                  Scalar.Quantity.minimum delta
                    (Risk.max_order_quantity state.config.risk)
                in
                match
                  Scalar.Quantity.round_down_to_multiple bounded
                    ~multiple:instrument.lot_size
                with
                | Error _ as error -> error
                | Ok quantity when Scalar.Quantity.is_zero quantity ->
                    Error "target order limit cannot cover one instrument lot"
                | Ok quantity ->
                    Order.request ~instrument_id ~side ~quantity
                      ~kind:Order.Market ~origin:Order.Target_rebalance
                    |> Result.map Option.some))

  let reconcile_targets reduction =
    match reduction.state.desired_targets with
    | None -> Ok reduction
    | Some desired ->
        Id.Instrument.Map.bindings desired
        |> List.fold_left
             (fun result (instrument_id, target) ->
               match result with
               | Error _ as error -> error
               | Ok reduction -> (
                   match
                     bounded_target_request reduction.state instrument_id target
                   with
                   | Error _ as error -> error
                   | Ok None -> Ok reduction
                   | Ok (Some request) -> submit_order reduction request))
             (Ok reduction)

  let process_slice state market_slice =
    if state.completed then
      Error "completed engine cannot process another market slice"
    else
      match validate_slice state market_slice with
      | Error _ as error -> error
      | Ok () -> (
          let state =
            {
              state with
              last_slice_sequence = Some market_slice.slice_sequence;
              last_slice_end = Some market_slice.end_at;
              last_received_at = Some market_slice.received_at;
            }
          in
          let reduction =
            {
              state;
              now = market_slice.received_at;
              current_slice_sequence = market_slice.slice_sequence;
              audits_rev = [];
              pending = [];
              processed = 0;
            }
          in
          match ensure_started reduction with
          | Error _ as error -> error
          | Ok reduction -> (
              match
                emit reduction (Audit.Market_slice_received market_slice)
              with
              | Error _ as error -> error
              | Ok reduction -> (
                  match
                    Execution.match_slice state.config.execution
                      ~instruments:(configured_instruments state)
                      ~oms:state.oms market_slice
                  with
                  | Error _ as error -> error
                  | Ok matched -> (
                      match
                        apply_fills reduction market_slice matched.fills
                      with
                      | Error _ as error -> error
                      | Ok reduction -> (
                          match
                            cancel_market_remainders reduction
                              matched.market_ioc_orders
                          with
                          | Error _ as error -> error
                          | Ok reduction -> (
                              let latest_bars =
                                List.fold_left
                                  (fun bars bar ->
                                    Id.Instrument.Map.add bar.Bar.instrument_id
                                      bar bars)
                                  reduction.state.latest_bars market_slice.bars
                              in
                              let state =
                                { reduction.state with latest_bars }
                              in
                              let reduction =
                                enqueue { reduction with state }
                                  [
                                    notification { reduction with state }
                                      (Strategy.Market_slice_closed market_slice);
                                  ]
                              in
                              match drain reduction with
                              | Error _ as error -> error
                              | Ok reduction -> (
                                  match reconcile_targets reduction with
                                  | Error _ as error -> error
                                  | Ok reduction -> (
                                      match drain reduction with
                                      | Error _ as error -> error
                                      | Ok reduction -> (
                                          match valuation reduction with
                                          | Error _ as error -> error
                                          | Ok reduction ->
                                              Ok
                                                ( reduction.state,
                                                  List.rev reduction.audits_rev
                                                ))))))))))

  let order_counts orders =
    List.fold_left
      (fun (counts : Audit.order_counts) order ->
        let counts = { counts with total = counts.total + 1 } in
        match order.Order.status with
        | Working | Partially_filled ->
            { counts with active = counts.active + 1 }
        | Filled -> { counts with filled = counts.filled + 1 }
        | Rejected _ -> { counts with rejected = counts.rejected + 1 }
        | Cancelled -> { counts with cancelled = counts.cancelled + 1 })
      Audit.{ total = 0; active = 0; filled = 0; rejected = 0; cancelled = 0 }
      orders

  let complete state =
    if state.completed then Error "engine run is already complete"
    else
      match value state with
      | Error _ as error -> error
      | Ok valuation -> (
          let order_counts = Oms.orders state.oms |> order_counts in
          let state = { state with completed = true } in
          let reduction =
            {
              state;
              now = Option.value state.last_received_at ~default:Ptime.epoch;
              current_slice_sequence =
                Option.value state.last_slice_sequence ~default:0L;
              audits_rev = [];
              pending = [];
              processed = 0;
            }
          in
          match ensure_started reduction with
          | Error _ as error -> error
          | Ok reduction -> (
              match
                emit reduction
                  (Audit.Run_completed
                     {
                       scenario_sha256 = state.scenario_sha256;
                       valuation;
                       order_counts;
                     })
              with
              | Error _ as error -> error
              | Ok reduction ->
                  Ok (reduction.state, valuation, List.rev reduction.audits_rev)
              ))
end
