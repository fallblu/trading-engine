type config = {
  contract_version : string;
  risk : Risk.t;
  execution_model : Execution_model.t;
  execution : Execution.t;
  max_internal_events : int;
}

let config ~contract_version ~risk ~execution_model ~execution
    ~max_internal_events =
  if not (Contract.is_supported contract_version) then
    Error "engine contract version is unsupported"
  else if max_internal_events <= 0 then
    Error "maximum internal events must be positive"
  else
    Ok
      {
        contract_version;
        risk;
        execution_model;
        execution;
        max_internal_events;
      }

let valid_sha256 value =
  String.length value = 64
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       value

module Interactive = struct
  let ( let* ) result function_ =
    match result with Ok value -> function_ value | Error _ as error -> error

  type desired_targets = {
    quantities : Scalar.Quantity.t Id.Instrument.Map.t;
    cause_ids : Id.Event.t list;
  }

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
    latest_fx_rates : (string * Scalar.Price.t) list;
    applied_action_ids : Id.Corporate_action.Set.t;
    desired_targets : desired_targets option;
    liquidation_pending : bool;
    account : Account.t;
    oms : Oms.t;
    started : bool;
    completed : bool;
  }

  type pending =
    | Notify of Id.Event.t list * Strategy.event
    | Act of Id.Event.t list * Strategy.intent

  type reduction = {
    state : t;
    now : Ptime.t;
    current_slice_sequence : int64;
    slice_event_id : Id.Event.t option;
    causation_ids : Id.Event.t list;
    audits_rev : Audit.t list;
    pending : pending list;
    processed : int;
  }

  let create ~run_id ~scenario_sha256 ~config ~initial_cash =
    if not (valid_sha256 scenario_sha256) then
      Error "scenario SHA-256 must contain 64 lowercase hexadecimal characters"
    else
      let expected_currencies =
        Risk.base_currency config.risk
        :: List.map
             (fun instrument -> instrument.Instrument.quote_currency)
             (Risk.instruments config.risk)
        |> List.sort_uniq String.compare
      in
      let supplied_currencies =
        List.map fst initial_cash |> List.sort_uniq String.compare
      in
      if supplied_currencies <> expected_currencies then
        Error "initial cash must contain every configured currency exactly once"
      else
        match
          Account.create
            ~base_currency:(Risk.base_currency config.risk)
            ~initial_cash
        with
        | Error _ as error -> error
        | Ok account ->
            let base_rate =
              Scalar.Price.of_decimal_string "1" |> Result.get_ok
            in
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
                latest_fx_rates =
                  [ (Risk.base_currency config.risk, base_rate) ];
                applied_action_ids = Id.Corporate_action.Set.empty;
                desired_targets = None;
                liquidation_pending = false;
                account;
                oms = Oms.empty;
                started = false;
                completed = false;
              }

  let account state = state.account
  let oms state = state.oms

  let latest_bar state instrument_id =
    Id.Instrument.Map.find_opt instrument_id state.latest_bars

  let next_sequence value =
    if Int64.equal value Int64.max_int then Error "engine sequence is exhausted"
    else Ok (Int64.succ value)

  let normalize_causes causes = List.sort_uniq Id.Event.compare causes

  let with_causes reduction causes =
    { reduction with causation_ids = normalize_causes causes }

  let emit_with_id reduction event =
    match next_sequence reduction.state.engine_sequence with
    | Error _ as error -> error
    | Ok engine_sequence ->
        let event_id =
          Audit.event_id ~run_id:reduction.state.run_id ~engine_sequence
        in
        let audit =
          Audit.create ~contract_version:reduction.state.config.contract_version
            ~engine_sequence
            ~causation_ids:(normalize_causes reduction.causation_ids)
            ~run_id:reduction.state.run_id ~recorded_at:reduction.now event
        in
        let state = { reduction.state with engine_sequence } in
        Ok
          ( { reduction with state; audits_rev = audit :: reduction.audits_rev },
            event_id )

  let emit reduction event = emit_with_id reduction event |> Result.map fst

  let ensure_started reduction =
    if reduction.state.started then Ok reduction
    else
      let state = { reduction.state with started = true } in
      emit_with_id
        (with_causes { reduction with state } [])
        (Audit.Run_started
           {
             scenario_sha256 = reduction.state.scenario_sha256;
             execution_model =
               Execution_model.name reduction.state.config.execution_model;
           })
      |> Result.map (fun (reduction, event_id) ->
          with_causes reduction [ event_id ])

  let enqueue reduction items =
    { reduction with pending = reduction.pending @ items }

  let prepend reduction items =
    { reduction with pending = items @ reduction.pending }

  let value state =
    let marks =
      Id.Instrument.Map.bindings state.latest_bars
      |> List.map (fun (instrument_id, bar) ->
          (instrument_id, bar.Bar.close_price))
    in
    Account.value state.account
      ~instruments:(Risk.instruments state.config.risk)
      ~marks ~fx_rates:state.latest_fx_rates

  let strategy_context state now =
    let latest_bars =
      Id.Instrument.Map.bindings state.latest_bars |> List.map snd
    in
    let* valuation = value state in
    Strategy.context ~now ~valuation
      ~working_orders:(Oms.active_orders state.oms)
      ~latest_bars

  let notification _reduction ~causation_ids event =
    Ok (Notify (causation_ids, event))

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
    match emit_with_id reduction (Audit.Intent_rejected reason) with
    | Error _ as error -> error
    | Ok (reduction, event_id) ->
        let* pending =
          notification reduction ~causation_ids:[ event_id ]
            (Strategy.Intent_rejected reason)
        in
        Ok (enqueue reduction [ pending ])

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
            let created_event_id =
              Audit.event_id ~run_id:reduction.state.run_id
                ~engine_sequence:order_sequence
            in
            match
              let marks =
                Id.Instrument.Map.bindings reduction.state.latest_bars
                |> List.map (fun (instrument_id, bar) ->
                    (instrument_id, bar.Bar.close_price))
              in
              Risk.check reduction.state.config.risk
                ~account:reduction.state.account ~oms:reduction.state.oms ~marks
                ~fx_rates:reduction.state.latest_fx_rates request
            with
            | Ok () -> (
                match
                  Oms.accept reduction.state.oms ~id ~created_event_id
                    ~accepted_sequence:order_sequence ~created_at:reduction.now
                    ~eligible_after_slice_sequence:
                      reduction.current_slice_sequence request
                with
                | Error _ as error -> error
                | Ok (oms, order) -> (
                    let reduction =
                      { reduction with state = { reduction.state with oms } }
                    in
                    match
                      emit_with_id reduction (Audit.Order_accepted order)
                    with
                    | Error _ as error -> error
                    | Ok (reduction, event_id) ->
                        let* pending =
                          notification reduction ~causation_ids:[ event_id ]
                            (Strategy.Order_updated order)
                        in
                        Ok (enqueue reduction [ pending ])))
            | Error reason -> (
                match
                  Oms.reject reduction.state.oms ~id ~created_event_id
                    ~rejected_sequence:order_sequence ~created_at:reduction.now
                    ~eligible_after_slice_sequence:
                      reduction.current_slice_sequence request ~reason
                with
                | Error _ as error -> error
                | Ok (oms, order) -> (
                    let reduction =
                      { reduction with state = { reduction.state with oms } }
                    in
                    match
                      emit_with_id reduction (Audit.Order_rejected order)
                    with
                    | Error _ as error -> error
                    | Ok (reduction, event_id) ->
                        let* order_pending =
                          notification reduction ~causation_ids:[ event_id ]
                            (Strategy.Order_updated order)
                        in
                        let* rejection_pending =
                          notification reduction ~causation_ids:[ event_id ]
                            (Strategy.Intent_rejected reason)
                        in
                        Ok
                          (enqueue reduction
                             [ order_pending; rejection_pending ])))))

  let cancel_order reduction ~reason order_id =
    match Oms.cancel reduction.state.oms order_id with
    | Error message -> reject_intent reduction message
    | Ok (oms, order) -> (
        let reduction =
          { reduction with state = { reduction.state with oms } }
          |> fun reduction ->
          with_causes reduction
            (order.Order.created_event_id :: reduction.causation_ids)
        in
        match
          emit_with_id reduction (Audit.Order_cancelled { order; reason })
        with
        | Error _ as error -> error
        | Ok (reduction, event_id) ->
            let* pending =
              notification reduction ~causation_ids:[ event_id ]
                (Strategy.Order_updated order)
            in
            Ok (enqueue reduction [ pending ]))

  let cancel_orders reduction ~reason order_ids =
    let causation_ids = reduction.causation_ids in
    let rec cancel reduction = function
      | [] -> Ok (with_causes reduction causation_ids)
      | order_id :: remaining -> (
          match
            cancel_order (with_causes reduction causation_ids) ~reason order_id
          with
          | Error _ as error -> error
          | Ok reduction -> cancel reduction remaining)
    in
    cancel reduction order_ids

  let configured_instruments state =
    Risk.instruments state.config.risk
    |> List.sort (fun left right ->
        Id.Instrument.compare left.Instrument.id right.Instrument.id)

  let event_ids_after state count =
    let rec collect sequence remaining values =
      if remaining = 0 then Ok (List.rev values)
      else
        let* sequence = next_sequence sequence in
        let id =
          Audit.event_id ~run_id:state.run_id ~engine_sequence:sequence
        in
        collect sequence (remaining - 1) (id :: values)
    in
    collect state.engine_sequence count []

  let split_desired_targets desired action event_id =
    match
      Id.Instrument.Map.find_opt action.Corporate_action.instrument_id
        desired.quantities
    with
    | None -> Error "split target refers to an unknown instrument"
    | Some quantity -> (
        match action.kind with
        | Corporate_action.Cash_dividend _ -> Ok desired
        | Corporate_action.Split { numerator; denominator } ->
            let* quantity =
              Scalar.Quantity.scale_ratio_exact quantity ~numerator ~denominator
            in
            Ok
              {
                quantities =
                  Id.Instrument.Map.add action.instrument_id quantity
                    desired.quantities;
                cause_ids = event_id :: desired.cause_ids;
              })

  let apply_split_action reduction action numerator denominator =
    let instrument_id = action.Corporate_action.instrument_id in
    let previous_quantity =
      Account.position_quantity reduction.state.account instrument_id
    in
    let* account =
      Account.apply_split reduction.state.account ~instrument_id ~numerator
        ~denominator
    in
    let adjusted_quantity = Account.position_quantity account instrument_id in
    let reduction =
      { reduction with state = { reduction.state with account } }
    in
    let* reduction, split_event_id =
      emit_with_id reduction
        (Audit.Split_applied { action; previous_quantity; adjusted_quantity })
    in
    let* desired_targets =
      match reduction.state.desired_targets with
      | None -> Ok None
      | Some desired ->
          split_desired_targets desired action split_event_id
          |> Result.map Option.some
    in
    let active = Oms.active_for_instrument reduction.state.oms instrument_id in
    let* updated_event_ids =
      event_ids_after reduction.state (List.length active)
    in
    let* oms, adjusted =
      Oms.adjust_for_split reduction.state.oms ~instrument_id ~updated_event_ids
        ~numerator ~denominator
    in
    let* instrument =
      match Risk.instrument reduction.state.config.risk instrument_id with
      | Some value -> Ok value
      | None -> Error "split refers to an unknown instrument"
    in
    let* () =
      List.fold_left
        (fun result order ->
          let* () = result in
          if
            not
              (Scalar.Quantity.is_multiple order.Order.request.quantity
                 ~lot:instrument.lot_size)
          then Error "split-adjusted order is not aligned to the instrument lot"
          else
            match order.request.kind with
            | Order.Market -> Ok ()
            | Order.Limit price ->
                if Scalar.Price.is_multiple price ~tick:instrument.tick_size
                then Ok ()
                else
                  Error
                    "split-adjusted limit price is not aligned to the \
                     instrument tick")
        (Ok ()) adjusted
    in
    let state = { reduction.state with oms; desired_targets } in
    let reduction = { reduction with state } in
    List.fold_left
      (fun result order ->
        let* reduction = result in
        let causes = [ order.Order.created_event_id; split_event_id ] in
        let* reduction, emitted_id =
          emit_with_id
            (with_causes reduction causes)
            (Audit.Order_adjusted { order; action_id = action.id })
        in
        if not (Id.Event.equal emitted_id order.updated_event_id) then
          Error "split order adjustment event ID prediction diverged"
        else Ok reduction)
      (Ok reduction) adjusted

  let apply_dividend_action reduction action amount_per_unit =
    let instrument_id = action.Corporate_action.instrument_id in
    let quantity =
      Account.position_quantity reduction.state.account instrument_id
    in
    let* instrument =
      match Risk.instrument reduction.state.config.risk instrument_id with
      | Some value -> Ok value
      | None -> Error "cash dividend refers to an unknown instrument"
    in
    let* cash_amount = Scalar.Money.for_quantity amount_per_unit quantity in
    let* account =
      Account.apply_cash_dividend reduction.state.account ~instrument_id
        ~quote_currency:instrument.quote_currency ~amount_per_unit
    in
    let reduction =
      { reduction with state = { reduction.state with account } }
    in
    emit reduction
      (Audit.Cash_dividend_applied { action; quantity; cash_amount })

  let apply_corporate_actions reduction actions =
    List.fold_left
      (fun result action ->
        let* reduction = result in
        let causes = Option.to_list reduction.slice_event_id in
        let reduction = with_causes reduction causes in
        match action.Corporate_action.kind with
        | Split { numerator; denominator } ->
            apply_split_action reduction action numerator denominator
        | Cash_dividend { amount_per_unit } ->
            apply_dividend_action reduction action amount_per_unit)
      (Ok reduction) actions

  let borrow_fee ~notional ~bps span =
    if bps = 0 || Scalar.Money.equal notional Scalar.Money.zero then
      Ok Scalar.Money.zero
    else
      let days, picoseconds = Ptime.Span.to_d_ps span in
      let picoseconds_per_second = Z.of_string "1000000000000" in
      let duration =
        Z.add
          (Z.mul (Z.of_int days)
             (Z.mul (Z.of_int 86_400) picoseconds_per_second))
          (Z.of_int64 picoseconds)
      in
      let numerator =
        Z.mul
          (Z.mul (Z.of_int64 (Scalar.Money.to_micros notional)) (Z.of_int bps))
          duration
      in
      let denominator =
        Z.mul
          (Z.mul (Z.of_int 10_000) (Z.of_int (365 * 86_400)))
          picoseconds_per_second
      in
      let fee =
        if Z.equal numerator Z.zero then Z.zero
        else Z.div (Z.add numerator (Z.pred denominator)) denominator
      in
      if Z.fits_int64 fee then Ok (Scalar.Money.of_micros (Z.to_int64 fee))
      else Error "short borrow fee overflow"

  let apply_borrow_fees reduction market_slice =
    let span =
      Ptime.diff market_slice.Market_slice.end_at market_slice.start_at
    in
    List.fold_left
      (fun result instrument ->
        let* reduction = result in
        let quantity =
          Account.position_quantity reduction.state.account
            instrument.Instrument.id
        in
        if
          (not (Scalar.Quantity.is_negative quantity))
          || Risk.short_borrow_bps reduction.state.config.risk = 0
        then Ok reduction
        else
          let* short_quantity = Scalar.Quantity.absolute quantity in
          let* bar =
            match Market_slice.bar market_slice instrument.id with
            | Some value -> Ok value
            | None -> Error "short position has no market slice bar"
          in
          let* notional = Scalar.Money.notional bar.open_price short_quantity in
          let borrow_bps = Risk.short_borrow_bps reduction.state.config.risk in
          let* fee = borrow_fee ~notional ~bps:borrow_bps span in
          if Scalar.Money.equal fee Scalar.Money.zero then Ok reduction
          else
            let* account =
              Account.apply_borrow_fee reduction.state.account
                ~instrument_id:instrument.id
                ~quote_currency:instrument.quote_currency ~fee
            in
            let reduction =
              { reduction with state = { reduction.state with account } }
            in
            let causes = Option.to_list reduction.slice_event_id in
            emit
              (with_causes reduction causes)
              (Audit.Borrow_fee_applied
                 {
                   instrument_id = instrument.id;
                   quote_currency = instrument.quote_currency;
                   short_quantity;
                   reference_price = bar.open_price;
                   borrow_bps;
                   period_start = market_slice.start_at;
                   period_end = market_slice.end_at;
                   fee;
                 }))
      (Ok reduction)
      (configured_instruments reduction.state)

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
          else
            let* () = Risk.check_position state.config.risk target.quantity in
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
    let* gross_weight =
      List.fold_left
        (fun result (target : Strategy.weight_target) ->
          let* total = result in
          let* absolute = Scalar.Weight.absolute target.Strategy.weight in
          Scalar.Weight.add total absolute)
        (Ok Scalar.Weight.zero) targets
    in
    if
      Int64.compare
        (Scalar.Weight.to_micros gross_weight)
        (Scalar.Ratio.to_micros (Risk.max_leverage state.config.risk))
      > 0
    then Error "target gross weight exceeds maximum leverage"
    else
      let* valuation =
        let marks =
          Id.Instrument.Map.bindings state.latest_bars
          |> List.map (fun (instrument_id, bar) ->
              (instrument_id, bar.Bar.close_price))
        in
        Account.value state.account
          ~instruments:(Risk.instruments state.config.risk)
          ~marks ~fx_rates:state.latest_fx_rates
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
            let* () = Risk.check_position state.config.risk quantity in
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
      emit_with_id reduction
        (Audit.Target_portfolio_requested { basis; targets = requested })
    with
    | Error _ as error -> error
    | Ok (reduction, target_event_id) -> (
        let reduction = with_causes reduction [ target_event_id ] in
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
                state =
                  {
                    reduction.state with
                    desired_targets =
                      Some
                        {
                          quantities = desired;
                          cause_ids = [ target_event_id ];
                        };
                  };
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
    | intent when reduction.state.liquidation_pending -> (
        match intent with
        | Strategy.Emit_metric { name; value } -> metric reduction name value
        | _ -> reject_intent reduction "margin liquidation is in progress")
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

  type drain_result =
    | Drained of reduction
    | Strategy_requested of {
        reduction : reduction;
        causation_ids : Id.Event.t list;
        context : Strategy.context;
        event : Strategy.event;
      }

  let rec drain reduction =
    match reduction.pending with
    | [] -> Ok (Drained reduction)
    | _ when reduction.processed >= reduction.state.config.max_internal_events
      ->
        Error "maximum internal event count exceeded"
    | item :: pending -> (
        let reduction =
          { reduction with pending; processed = reduction.processed + 1 }
        in
        match item with
        | Notify (causation_ids, event) ->
            let reduction = with_causes reduction causation_ids in
            let* context = strategy_context reduction.state reduction.now in
            Ok (Strategy_requested { reduction; causation_ids; context; event })
        | Act (causation_ids, intent) -> (
            match
              handle_intent (with_causes reduction causation_ids) intent
            with
            | Error _ as error -> error
            | Ok reduction -> drain reduction))

  let validate_slice state market_slice =
    let expected =
      configured_instruments state
      |> List.map (fun instrument -> instrument.Instrument.id)
    in
    let ids =
      List.map (fun bar -> bar.Bar.instrument_id) market_slice.Market_slice.bars
    in
    let actual = List.sort_uniq Id.Instrument.compare ids in
    let expected_currencies =
      Risk.base_currency state.config.risk
      :: List.map
           (fun instrument -> instrument.Instrument.quote_currency)
           (configured_instruments state)
      |> List.sort_uniq String.compare
    in
    let actual_currencies =
      List.map
        (fun mark -> mark.Market_slice.currency)
        market_slice.Market_slice.fx_rates
      |> List.sort_uniq String.compare
    in
    let one = Scalar.Price.of_decimal_string "1" |> Result.get_ok in
    let actions_valid =
      List.for_all
        (fun action ->
          Option.is_some
            (Risk.instrument state.config.risk
               action.Corporate_action.instrument_id)
          && not
               (Id.Corporate_action.Set.mem action.id state.applied_action_ids))
        market_slice.corporate_actions
    in
    if List.length ids <> List.length actual || actual <> expected then
      Error "market slice must contain each configured instrument exactly once"
    else if actual_currencies <> expected_currencies then
      Error "market slice must contain each configured currency FX rate"
    else if
      not
        (Option.exists
           (fun rate -> Scalar.Price.equal rate one)
           (Market_slice.fx_rate market_slice
              (Risk.base_currency state.config.risk)))
    then Error "market slice base-currency FX rate must equal one"
    else if not actions_valid then
      Error "corporate action is unknown or was already applied"
    else
      match state.last_slice_sequence with
      | Some sequence
        when Int64.compare market_slice.slice_sequence sequence <= 0 ->
          Error "market slice sequence must increase"
      | _ -> (
          match state.last_slice_end with
          | Some end_at when Ptime.compare market_slice.start_at end_at < 0 ->
              Error "market slice start must not precede previous end"
          | _ -> (
              match state.last_received_at with
              | Some received_at
                when Ptime.compare market_slice.received_at received_at < 0 ->
                  Error "market slice receipt time must not move backward"
              | _ -> Ok ()))

  let fill_fee execution price quantity =
    let* notional = Scalar.Money.notional price quantity in
    Scalar.Money.fee
      ~fixed:(Execution.fixed_fee execution)
      ~bps:(Execution.fee_bps execution)
      ~notional

  let slice_open_marks market_slice =
    List.map
      (fun bar -> (bar.Bar.instrument_id, bar.Bar.open_price))
      market_slice.Market_slice.bars

  let permitted_fill state market_slice order proposed instrument =
    let marks = slice_open_marks market_slice in
    let instruments = configured_instruments state in
    let* before =
      Account.value state.account ~instruments ~marks
        ~fx_rates:state.latest_fx_rates
    in
    let before_position =
      Account.position_quantity state.account instrument.Instrument.id
    in
    let candidate quantity =
      let prepared =
        let* fee =
          fill_fee state.config.execution proposed.Execution.price quantity
        in
        let* fill =
          Fill.create ~id:(fill_id state) ~order_id:order.Order.id
            ~instrument_id:instrument.id
            ~quote_currency:instrument.quote_currency ~side:order.request.side
            ~quantity ~price:proposed.price ~fee
            ~executed_at:proposed.executed_at
            ~slice_sequence:market_slice.Market_slice.slice_sequence
        in
        let* account = Account.apply_fill state.account fill in
        let after_position = Account.position_quantity account instrument.id in
        let* after =
          Account.value account ~instruments ~marks
            ~fx_rates:state.latest_fx_rates
        in
        Ok (fee, after_position, after)
      in
      match prepared with
      | Error message -> Error (`Invalid message)
      | Ok (fee, after_position, after) -> (
          match
            Risk.check_post_fill state.config.risk ~before_position
              ~after_position ~before ~after
          with
          | Ok () -> Ok fee
          | Error (Risk.Limit limit) -> Error (`Limit limit)
          | Error (Risk.Invalid message) -> Error (`Invalid message))
    in
    let lot_value = Scalar.Quantity.to_micros instrument.lot_size in
    let quantity_limit =
      Scalar.Quantity.minimum proposed.quantity
        (Risk.max_order_quantity state.config.risk)
    in
    let requested_lots =
      Int64.div (Scalar.Quantity.to_micros quantity_limit) lot_value
    in
    let rec search low high =
      if Int64.compare low high >= 0 then Ok low
      else
        let difference = Int64.sub high low in
        let upper_half =
          Int64.add (Int64.div difference 2L) (Int64.rem difference 2L)
        in
        let middle = Int64.add low upper_half in
        let quantity = Scalar.Quantity.of_micros (Int64.mul middle lot_value) in
        match candidate quantity with
        | Ok _ -> search middle high
        | Error (`Limit _) -> search low (Int64.pred middle)
        | Error (`Invalid message) -> Error message
    in
    let* lots = search 0L requested_lots in
    let quantity = Scalar.Quantity.of_micros (Int64.mul lots lot_value) in
    let clipped = Scalar.Quantity.compare quantity proposed.quantity < 0 in
    let* limit =
      if not clipped then Ok None
      else if Int64.equal lots requested_lots then
        Ok
          (Some
             (Risk.Maximum_order_quantity
                (Risk.max_order_quantity state.config.risk)))
      else
        let next_lots = Int64.succ lots in
        let next_quantity =
          Scalar.Quantity.of_micros (Int64.mul next_lots lot_value)
        in
        match candidate next_quantity with
        | Error (`Limit limit) -> Ok (Some limit)
        | Error (`Invalid message) -> Error message
        | Ok _ -> Error "fill clipping search produced a nonmaximal quantity"
    in
    if Scalar.Quantity.is_zero quantity then
      Ok (quantity, Scalar.Money.zero, limit)
    else
      match candidate quantity with
      | Ok fee -> Ok (quantity, fee, limit)
      | Error (`Invalid message) -> Error message
      | Error (`Limit _) -> Error "permitted fill violates its limiting policy"

  let apply_fill reduction market_slice proposed quantity fee =
    match Oms.find reduction.state.oms proposed.Execution.order_id with
    | None -> Error "execution proposal refers to an unknown order"
    | Some order -> (
        match
          Risk.instrument reduction.state.config.risk
            order.request.instrument_id
        with
        | None -> Error "execution order refers to an unknown instrument"
        | Some instrument -> (
            let id = fill_id reduction.state in
            match increment_fill_number reduction.state with
            | Error _ as error -> error
            | Ok state -> (
                let reduction = { reduction with state } in
                match
                  Fill.create ~id ~order_id:order.id
                    ~instrument_id:order.request.instrument_id
                    ~quote_currency:instrument.Instrument.quote_currency
                    ~side:order.request.side ~quantity ~price:proposed.price
                    ~fee ~executed_at:proposed.executed_at
                    ~slice_sequence:market_slice.Market_slice.slice_sequence
                with
                | Error _ as error -> error
                | Ok fill -> (
                    match Oms.apply_fill reduction.state.oms fill with
                    | Error _ as error -> error
                    | Ok (_, Oms.Duplicate) ->
                        Error "newly allocated fill ID was duplicated"
                    | Ok (oms, Oms.Applied order) -> (
                        match
                          Account.apply_fill reduction.state.account fill
                        with
                        | Error _ as error -> error
                        | Ok account -> (
                            let state = { reduction.state with oms; account } in
                            let reduction = { reduction with state } in
                            match
                              emit_with_id reduction (Audit.Fill_applied fill)
                            with
                            | Error _ as error -> error
                            | Ok (reduction, event_id) ->
                                let* fill_pending =
                                  notification reduction
                                    ~causation_ids:[ event_id ]
                                    (Strategy.Fill_received fill)
                                in
                                let* order_pending =
                                  notification reduction
                                    ~causation_ids:[ event_id ]
                                    (Strategy.Order_updated order)
                                in
                                Ok
                                  (enqueue reduction
                                     [ fill_pending; order_pending ])))))))

  let apply_proposed_fill reduction market_slice proposed =
    match Oms.find reduction.state.oms proposed.Execution.order_id with
    | None -> Error "execution proposal refers to an unknown order"
    | Some order ->
        let causes =
          match reduction.slice_event_id with
          | None ->
              [ order.Order.created_event_id; order.Order.updated_event_id ]
          | Some slice_event_id ->
              [
                order.Order.created_event_id;
                order.Order.updated_event_id;
                slice_event_id;
              ]
        in
        let reduction = with_causes reduction causes in
        let* instrument =
          match
            Risk.instrument reduction.state.config.risk
              order.request.instrument_id
          with
          | Some value -> Ok value
          | None -> Error "execution order refers to an unknown instrument"
        in
        let* permitted_quantity, fee, limit =
          permitted_fill reduction.state market_slice order proposed instrument
        in
        let* reduction =
          match limit with
          | None -> Ok reduction
          | Some limit ->
              if
                String.equal reduction.state.config.contract_version
                  Contract.previous_version
              then
                emit reduction
                  (Audit.Margin_limited
                     {
                       order_id = order.id;
                       instrument_id = order.request.instrument_id;
                       requested_quantity = proposed.quantity;
                       permitted_quantity;
                       price = proposed.price;
                     })
              else
                emit reduction
                  (Audit.Fill_clipped
                     {
                       order_id = order.id;
                       instrument_id = order.request.instrument_id;
                       proposed_quantity = proposed.quantity;
                       permitted_quantity;
                       price = proposed.price;
                       limit;
                     })
        in
        if Scalar.Quantity.is_zero permitted_quantity then
          Ok (reduction, permitted_quantity)
        else
          apply_fill reduction market_slice proposed permitted_quantity fee
          |> Result.map (fun reduction -> (reduction, permitted_quantity))

  let cancel_market_remainders reduction order_ids =
    let causation_ids = reduction.causation_ids in
    let rec cancel reduction = function
      | [] -> Ok (with_causes reduction causation_ids)
      | order_id :: remaining -> (
          match Oms.find reduction.state.oms order_id with
          | None -> Error "market IOC order disappeared during matching"
          | Some order -> (
              let result =
                if Order.is_active order then
                  cancel_order
                    (with_causes reduction causation_ids)
                    ~reason:Audit.Market_ioc order_id
                else Ok reduction
              in
              match result with
              | Error _ as error -> error
              | Ok reduction -> cancel reduction remaining))
    in
    cancel reduction order_ids

  let audit_valuation state =
    let* account = value state in
    let* margin = Risk.margin_snapshot state.config.risk account in
    Ok Audit.{ account; margin }

  let valuation reduction =
    match audit_valuation reduction.state with
    | Error _ as error -> error
    | Ok valuation ->
        let causes = Option.to_list reduction.slice_event_id in
        emit (with_causes reduction causes) (Audit.Valuation valuation)

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
        let side =
          if Scalar.Quantity.compare target current > 0 then Order.Buy
          else Order.Sell
        in
        let crosses_zero =
          Scalar.Quantity.is_positive current
          && Scalar.Quantity.is_negative target
          || Scalar.Quantity.is_negative current
             && Scalar.Quantity.is_positive target
        in
        let delta =
          if crosses_zero then Scalar.Quantity.absolute current
          else
            Scalar.Quantity.subtract target current |> fun result ->
            Result.bind result Scalar.Quantity.absolute
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
                  Scalar.Quantity.round_toward_zero_to_multiple bounded
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
        Id.Instrument.Map.bindings desired.quantities
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
                   | Ok (Some request) ->
                       let causes =
                         match reduction.slice_event_id with
                         | None -> desired.cause_ids
                         | Some slice_event_id ->
                             slice_event_id :: desired.cause_ids
                       in
                       submit_order (with_causes reduction causes) request))
             (Ok reduction)

  let positions_flat state =
    configured_instruments state
    |> List.for_all (fun instrument ->
        Account.position_quantity state.account instrument.Instrument.id
        |> Scalar.Quantity.is_zero)

  let ensure_liquidation_orders reduction causes =
    List.fold_left
      (fun result instrument ->
        let* reduction = result in
        let quantity =
          Account.position_quantity reduction.state.account
            instrument.Instrument.id
        in
        let already_working =
          Oms.active_for_instrument reduction.state.oms instrument.id
          |> List.exists (fun order ->
              order.Order.request.origin = Order.Margin_liquidation)
        in
        if Scalar.Quantity.is_zero quantity || already_working then Ok reduction
        else
          let* absolute = Scalar.Quantity.absolute quantity in
          let bounded =
            Scalar.Quantity.minimum absolute
              (Risk.max_order_quantity reduction.state.config.risk)
          in
          let* quantity =
            Scalar.Quantity.round_toward_zero_to_multiple bounded
              ~multiple:instrument.lot_size
          in
          if Scalar.Quantity.is_zero quantity then
            Error "margin liquidation cannot cover one instrument lot"
          else
            let side =
              if
                Scalar.Quantity.is_positive
                  (Account.position_quantity reduction.state.account
                     instrument.id)
              then Order.Sell
              else Order.Buy
            in
            let* request =
              Order.request ~instrument_id:instrument.id ~side ~quantity
                ~kind:Order.Market ~origin:Order.Margin_liquidation
            in
            submit_order (with_causes reduction causes) request)
      (Ok reduction)
      (configured_instruments reduction.state)

  let assess_margin reduction =
    let* valuation = audit_valuation reduction.state in
    if (not reduction.state.liquidation_pending) && valuation.margin.margin_call
    then
      let causes = Option.to_list reduction.slice_event_id in
      let* reduction, margin_event_id =
        emit_with_id
          (with_causes reduction causes)
          (Audit.Margin_call_triggered valuation)
      in
      let active_ids =
        Oms.active_orders reduction.state.oms
        |> List.map (fun order -> order.Order.id)
      in
      let* reduction =
        cancel_orders
          (with_causes reduction [ margin_event_id ])
          ~reason:Audit.Margin_call active_ids
      in
      let state =
        {
          reduction.state with
          desired_targets = None;
          liquidation_pending = true;
        }
      in
      ensure_liquidation_orders { reduction with state } [ margin_event_id ]
    else if reduction.state.liquidation_pending then
      if positions_flat reduction.state && not valuation.margin.margin_call then
        let causes = Option.to_list reduction.slice_event_id in
        let* reduction =
          emit (with_causes reduction causes) (Audit.Margin_restored valuation)
        in
        Ok
          {
            reduction with
            state = { reduction.state with liquidation_pending = false };
          }
      else
        ensure_liquidation_orders reduction
          (Option.to_list reduction.slice_event_id)
    else Ok reduction

  type phase =
    | Match_slice of Market_slice.t * Execution.cursor
    | Reconcile_targets
    | Finish_slice

  type progress =
    | Awaiting_strategy of {
        reduction : reduction;
        phase : phase;
        causation_ids : Id.Event.t list;
        context : Strategy.context;
        event : Strategy.event;
      }
    | Slice_completed of t * Audit.t list

  let rec continue phase reduction =
    let* drained = drain reduction in
    match drained with
    | Strategy_requested { reduction; causation_ids; context; event } ->
        Ok
          (Awaiting_strategy { reduction; phase; causation_ids; context; event })
    | Drained reduction -> (
        match phase with
        | Match_slice (market_slice, cursor) -> (
            match Execution.next cursor ~oms:reduction.state.oms with
            | Error _ as error -> error
            | Ok (Execution.Proposed (proposed, advance)) ->
                let* reduction, applied_quantity =
                  apply_proposed_fill reduction market_slice proposed
                in
                let* cursor = advance applied_quantity in
                continue (Match_slice (market_slice, cursor)) reduction
            | Ok (Execution.Finished market_ioc_orders) ->
                let* slice_event_id =
                  match reduction.slice_event_id with
                  | Some value -> Ok value
                  | None -> Error "matching slice has no audit event"
                in
                let reduction = with_causes reduction [ slice_event_id ] in
                let* reduction =
                  cancel_market_remainders reduction market_ioc_orders
                in
                let* pending =
                  notification reduction ~causation_ids:[ slice_event_id ]
                    (Strategy.Market_slice_closed market_slice)
                in
                continue Reconcile_targets (enqueue reduction [ pending ]))
        | Reconcile_targets ->
            let* reduction = reconcile_targets reduction in
            continue Finish_slice reduction
        | Finish_slice ->
            let* reduction = assess_margin reduction in
            if reduction.pending = [] then
              let* reduction = valuation reduction in
              Ok
                (Slice_completed (reduction.state, List.rev reduction.audits_rev))
            else continue Finish_slice reduction)

  let strategy_request = function
    | Awaiting_strategy { context; event; _ } -> Some (context, event)
    | Slice_completed _ -> None

  let slice_result = function
    | Awaiting_strategy _ -> None
    | Slice_completed (state, audits) -> Some (state, audits)

  let resume progress intents =
    match progress with
    | Slice_completed _ ->
        Error "completed slice cannot accept strategy intents"
    | Awaiting_strategy { reduction; phase; causation_ids; _ } ->
        let actions =
          List.map (fun intent -> Act (causation_ids, intent)) intents
        in
        continue phase (prepend reduction actions)

  let process_slice state market_slice =
    if state.completed then
      Error "completed engine cannot process another market slice"
    else
      let* () = validate_slice state market_slice in
      let state =
        let applied_action_ids =
          List.fold_left
            (fun ids action ->
              Id.Corporate_action.Set.add action.Corporate_action.id ids)
            state.applied_action_ids market_slice.corporate_actions
        in
        let latest_bars =
          List.fold_left
            (fun bars bar ->
              Id.Instrument.Map.add bar.Bar.instrument_id bar bars)
            state.latest_bars market_slice.bars
        in
        {
          state with
          last_slice_sequence = Some market_slice.slice_sequence;
          last_slice_end = Some market_slice.end_at;
          last_received_at = Some market_slice.received_at;
          latest_fx_rates =
            List.map
              (fun mark -> (mark.Market_slice.currency, mark.Market_slice.rate))
              market_slice.fx_rates;
          latest_bars;
          applied_action_ids;
        }
      in
      let reduction =
        {
          state;
          now = market_slice.received_at;
          current_slice_sequence = market_slice.slice_sequence;
          slice_event_id = None;
          causation_ids = [];
          audits_rev = [];
          pending = [];
          processed = 0;
        }
      in
      let* reduction = ensure_started reduction in
      let* reduction, slice_event_id =
        emit_with_id (with_causes reduction [])
          (Audit.Market_slice_received market_slice)
      in
      let reduction =
        {
          reduction with
          slice_event_id = Some slice_event_id;
          causation_ids = [ slice_event_id ];
        }
      in
      let* reduction =
        apply_corporate_actions reduction market_slice.corporate_actions
      in
      let* reduction = apply_borrow_fees reduction market_slice in
      let* cursor =
        Execution_model.start_slice reduction.state.config.execution_model
          reduction.state.config.execution
          ~instruments:(configured_instruments reduction.state)
          ~oms:reduction.state.oms market_slice
      in
      continue (Match_slice (market_slice, cursor)) reduction

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
          let* margin = Risk.margin_snapshot state.config.risk valuation in
          let audit_valuation = Audit.{ account = valuation; margin } in
          let state = { state with completed = true } in
          let causation_ids =
            if Int64.equal state.engine_sequence 0L then []
            else
              [
                Audit.event_id ~run_id:state.run_id
                  ~engine_sequence:state.engine_sequence;
              ]
          in
          let reduction =
            {
              state;
              now = Option.value state.last_received_at ~default:Ptime.epoch;
              current_slice_sequence =
                Option.value state.last_slice_sequence ~default:0L;
              slice_event_id = None;
              causation_ids;
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
                       execution_model =
                         Execution_model.name state.config.execution_model;
                       valuation = audit_valuation;
                       order_counts;
                     })
              with
              | Error _ as error -> error
              | Ok reduction ->
                  Ok (reduction.state, valuation, List.rev reduction.audits_rev)
              ))
end

module Make (Strategy_impl : Strategy.S) = struct
  let ( let* ) result function_ =
    match result with Ok value -> function_ value | Error _ as error -> error

  type t = { engine : Interactive.t; strategy_state : Strategy_impl.state }

  let create ~run_id ~scenario_sha256 ~config ~initial_cash ~strategy_state =
    Interactive.create ~run_id ~scenario_sha256 ~config ~initial_cash
    |> Result.map (fun engine -> { engine; strategy_state })

  let account state = Interactive.account state.engine
  let oms state = Interactive.oms state.engine

  let latest_bar state instrument_id =
    Interactive.latest_bar state.engine instrument_id

  let strategy_state state = state.strategy_state
  let with_strategy_state state strategy_state = { state with strategy_state }

  let rec drive strategy_state progress =
    match Interactive.strategy_request progress with
    | Some (context, event) ->
        let strategy_state, intents =
          Strategy_impl.on_event strategy_state context event
        in
        let* progress = Interactive.resume progress intents in
        drive strategy_state progress
    | None -> (
        match Interactive.slice_result progress with
        | Some (engine, audits) -> Ok ({ engine; strategy_state }, audits)
        | None -> Error "interactive engine reached an invalid progress state")

  let process_slice state market_slice =
    let* progress = Interactive.process_slice state.engine market_slice in
    drive state.strategy_state progress

  let complete state =
    let* engine, valuation, audits = Interactive.complete state.engine in
    Ok ({ state with engine }, valuation, audits)
end
