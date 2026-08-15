type config = {
  risk : Risk.t;
  execution : Execution.t;
  max_internal_events : int;
}

let config ~risk ~execution ~max_internal_events =
  if max_internal_events <= 0 then
    Error "maximum internal events must be positive"
  else Ok { risk; execution; max_internal_events }

module Make (Strategy_impl : Strategy.S) = struct
  type t = {
    run_id : Id.Run.t;
    config : config;
    engine_sequence : int64;
    next_order_number : int64;
    next_fill_number : int64;
    last_source_sequence : int64 option;
    last_received_at : Ptime.t option;
    latest_bars : Bar.t Id.Instrument.Map.t;
    account : Account.t;
    oms : Oms.t;
    strategy_state : Strategy_impl.state;
    completed : bool;
  }

  type pending =
    | Notify of Strategy.context * Strategy.event
    | Act of Strategy.intent

  type reduction = {
    state : t;
    now : Ptime.t;
    current_bar_sequence : int64;
    audits_rev : Audit.t list;
    pending : pending list;
    processed : int;
  }

  let create ~run_id ~config ~initial_cash ~strategy_state =
    {
      run_id;
      config;
      engine_sequence = 0L;
      next_order_number = 1L;
      next_fill_number = 1L;
      last_source_sequence = None;
      last_received_at = None;
      latest_bars = Id.Instrument.Map.empty;
      account = Account.create ~initial_cash;
      oms = Oms.empty;
      strategy_state;
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
                    ~eligible_after_bar_sequence:reduction.current_bar_sequence
                    request
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
                    ~eligible_after_bar_sequence:reduction.current_bar_sequence
                    request ~reason
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

  let target_position reduction instrument_id quantity =
    match
      emit reduction (Audit.Target_requested { instrument_id; quantity })
    with
    | Error _ as error -> error
    | Ok reduction
      when Risk.instrument reduction.state.config.risk instrument_id = None ->
        reject_intent reduction "target refers to an unknown instrument"
    | Ok reduction -> (
        match
          Planner.target_position ~account:reduction.state.account
            ~oms:reduction.state.oms ~instrument_id ~target:quantity
        with
        | Error reason -> reject_intent reduction reason
        | Ok plan -> (
            match
              cancel_orders reduction ~reason:Audit.Target_replaced
                plan.cancel_orders
            with
            | Error _ as error -> error
            | Ok reduction -> (
                match plan.submit_order with
                | None -> Ok reduction
                | Some request -> submit_order reduction request)))

  let metric reduction name value =
    if String.length name = 0 || String.trim name <> name then
      reject_intent reduction "metric name must be a nonempty trimmed string"
    else emit reduction (Audit.Metric_emitted { name; value })

  let handle_intent reduction = function
    | Strategy.Target_position { instrument_id; quantity } ->
        target_position reduction instrument_id quantity
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

  let validate_bar state bar =
    if Risk.instrument state.config.risk bar.Bar.instrument_id = None then
      Error "bar refers to an unknown instrument"
    else
      match state.last_source_sequence with
      | Some sequence when Int64.compare bar.source_sequence sequence <= 0 ->
          Error "bar source sequence must increase globally"
      | _ -> (
          match state.last_received_at with
          | Some received when Ptime.compare bar.received_at received < 0 ->
              Error "bar receipt time must not move backward"
          | _ -> (
              match
                Id.Instrument.Map.find_opt bar.instrument_id state.latest_bars
              with
              | Some previous
                when Ptime.compare bar.end_at previous.Bar.end_at <= 0 ->
                  Error "bar end must increase for each instrument"
              | _ -> Ok ()))

  let apply_proposed_fill reduction bar proposed =
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
                ~side:order.request.side ~quantity:proposed.quantity
                ~price:proposed.price ~fee:proposed.fee
                ~executed_at:proposed.executed_at
                ~bar_sequence:bar.Bar.source_sequence
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

  let rec apply_fills reduction bar = function
    | [] -> Ok reduction
    | proposed :: remaining -> (
        match apply_proposed_fill reduction bar proposed with
        | Error _ as error -> error
        | Ok reduction -> apply_fills reduction bar remaining)

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

  let process_bar state bar =
    if state.completed then Error "completed engine cannot process another bar"
    else
      match validate_bar state bar with
      | Error _ as error -> error
      | Ok () -> (
          let state =
            {
              state with
              last_source_sequence = Some bar.Bar.source_sequence;
              last_received_at = Some bar.received_at;
            }
          in
          let reduction =
            {
              state;
              now = bar.received_at;
              current_bar_sequence = bar.source_sequence;
              audits_rev = [];
              pending = [];
              processed = 0;
            }
          in
          match emit reduction (Audit.Bar_received bar) with
          | Error _ as error -> error
          | Ok reduction -> (
              match Risk.instrument state.config.risk bar.instrument_id with
              | None ->
                  Error
                    "validated bar instrument disappeared from risk \
                     configuration"
              | Some instrument -> (
                  match
                    Execution.match_bar state.config.execution ~instrument
                      ~oms:state.oms bar
                  with
                  | Error _ as error -> error
                  | Ok matched -> (
                      match apply_fills reduction bar matched.fills with
                      | Error _ as error -> error
                      | Ok reduction -> (
                          match
                            cancel_market_remainders reduction
                              matched.market_ioc_orders
                          with
                          | Error _ as error -> error
                          | Ok reduction -> (
                              let latest_bars =
                                Id.Instrument.Map.add bar.instrument_id bar
                                  reduction.state.latest_bars
                              in
                              let state =
                                { reduction.state with latest_bars }
                              in
                              let reduction =
                                enqueue { reduction with state }
                                  [
                                    notification { reduction with state }
                                      (Strategy.Bar_closed bar);
                                  ]
                              in
                              match drain reduction with
                              | Error _ as error -> error
                              | Ok reduction -> (
                                  match valuation reduction with
                                  | Error _ as error -> error
                                  | Ok reduction ->
                                      Ok
                                        ( reduction.state,
                                          List.rev reduction.audits_rev ))))))))

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
              current_bar_sequence =
                Option.value state.last_source_sequence ~default:0L;
              audits_rev = [];
              pending = [];
              processed = 0;
            }
          in
          match
            emit reduction (Audit.Run_completed { valuation; order_counts })
          with
          | Error _ as error -> error
          | Ok reduction ->
              Ok (reduction.state, valuation, List.rev reduction.audits_rev))
end
