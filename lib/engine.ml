module Currency_map = Map.Make (String)

type config = {
  contract_version : string;
  risk : Risk.t;
  venue_calendars : Venue_calendar.t list;
  execution_model : Execution_model.t;
  execution : Execution.t;
  financing : Financing.policy option;
  settlement : Settlement.policy option;
  max_internal_events : int;
}

let make_config ~venue_calendars ~contract_version ~risk ~execution_model
    ~execution ~financing ~settlement ~max_internal_events =
  if not (Contract.is_supported contract_version) then
    Error "engine contract version is unsupported"
  else if max_internal_events <= 0 then
    Error "maximum internal events must be positive"
  else if max_internal_events > Resource_limits.internal_events then
    Error
      (Printf.sprintf "maximum internal events is %d; limit is %d"
         max_internal_events Resource_limits.internal_events)
  else
    Ok
      {
        contract_version;
        risk;
        venue_calendars;
        execution_model;
        execution;
        financing;
        settlement;
        max_internal_events;
      }

let config ~contract_version ~risk ~execution_model ~execution
    ~max_internal_events =
  make_config ~venue_calendars:[] ~contract_version ~risk ~execution_model
    ~execution ~financing:None ~max_internal_events ~settlement:None

let config_v8 ~contract_version ~risk ~venue_calendars ~execution_model
    ~execution ~max_internal_events =
  make_config ~venue_calendars ~contract_version ~risk ~execution_model
    ~execution ~financing:None ~max_internal_events ~settlement:None

let config_v10 ~contract_version ~risk ~venue_calendars ~execution_model
    ~execution ~financing ~max_internal_events =
  make_config ~venue_calendars ~contract_version ~risk ~execution_model
    ~execution ~financing:(Some financing) ~max_internal_events ~settlement:None

let config_v11 ~contract_version ~risk ~venue_calendars ~execution_model
    ~execution ~financing ~settlement ~max_internal_events =
  make_config ~venue_calendars ~contract_version ~risk ~execution_model
    ~execution ~financing:(Some financing) ~settlement:(Some settlement)
    ~max_internal_events

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
    latest_marks : Scalar.Price.t Id.Instrument.Map.t;
    latest_fx_rates : (string * Scalar.Price.t) list;
    latest_borrow : Financing.borrow_observation Id.Instrument.Map.t;
    latest_cash_rates : Financing.cash_rate_observation Currency_map.t;
    settlement_instructions : Settlement.instruction list;
    initial_portfolio : Initial_portfolio.t option;
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

  module Pending_queue = struct
    type t = { front : pending list; back : pending list }

    let empty = { front = []; back = [] }
    let is_empty queue = queue.front = [] && queue.back = []

    let enqueue queue items =
      { queue with back = List.rev_append items queue.back }

    let prepend queue items = { queue with front = items @ queue.front }

    let pop queue =
      match queue.front with
      | item :: front -> Some (item, { queue with front })
      | [] -> (
          match List.rev queue.back with
          | [] -> None
          | item :: front -> Some (item, { front; back = [] }))
  end

  type reduction = {
    state : t;
    now : Ptime.t;
    current_slice_sequence : int64;
    slice_event_id : Id.Event.t option;
    causation_ids : Id.Event.t list;
    audits_rev : Audit.t list;
    pending : Pending_queue.t;
    processed : int;
  }

  let create_state ~run_id ~scenario_sha256 ~config ~account ~latest_marks
      ~latest_fx_rates ~initial_portfolio =
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
        latest_marks;
        latest_fx_rates;
        latest_borrow = Id.Instrument.Map.empty;
        latest_cash_rates = Currency_map.empty;
        settlement_instructions = [];
        initial_portfolio;
        applied_action_ids = Id.Corporate_action.Set.empty;
        desired_targets = None;
        liquidation_pending = false;
        account;
        oms = Oms.empty;
        started = false;
        completed = false;
      }

  let expected_currencies config =
    Risk.base_currency config.risk
    :: List.map
         (fun instrument -> instrument.Instrument.quote_currency)
         (Risk.instruments config.risk)
    |> List.sort_uniq String.compare

  let create ~run_id ~scenario_sha256 ~config ~initial_cash =
    if not (valid_sha256 scenario_sha256) then
      Error "scenario SHA-256 must contain 64 lowercase hexadecimal characters"
    else
      let expected_currencies = expected_currencies config in
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
            create_state ~run_id ~scenario_sha256 ~config ~account
              ~latest_marks:Id.Instrument.Map.empty
              ~latest_fx_rates:[ (Risk.base_currency config.risk, base_rate) ]
              ~initial_portfolio:None

  let create_with_portfolio ~run_id ~scenario_sha256 ~config ~initial_portfolio
      =
    if not (valid_sha256 scenario_sha256) then
      Error "scenario SHA-256 must contain 64 lowercase hexadecimal characters"
    else if
      not
        (String.equal initial_portfolio.Initial_portfolio.base_currency
           (Risk.base_currency config.risk))
    then Error "initial portfolio base currency differs from risk configuration"
    else
      let supplied =
        List.map fst initial_portfolio.cash |> List.sort_uniq String.compare
      in
      if supplied <> expected_currencies config then
        Error "initial cash must contain every configured currency exactly once"
      else
        let* account = Account.of_initial_portfolio initial_portfolio in
        let latest_marks =
          List.fold_left
            (fun marks (instrument_id, price) ->
              Id.Instrument.Map.add instrument_id price marks)
            Id.Instrument.Map.empty initial_portfolio.marks
        in
        let* valuation =
          Account.value account
            ~instruments:(Risk.instruments config.risk)
            ~marks:initial_portfolio.marks ~fx_rates:initial_portfolio.fx_rates
        in
        let* () = Risk.check_initial config.risk valuation in
        create_state ~run_id ~scenario_sha256 ~config ~account ~latest_marks
          ~latest_fx_rates:initial_portfolio.fx_rates
          ~initial_portfolio:(Some initial_portfolio)

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

  let value state =
    Account.value state.account
      ~instruments:(Risk.instruments state.config.risk)
      ~marks:(Id.Instrument.Map.bindings state.latest_marks)
      ~fx_rates:state.latest_fx_rates

  let ensure_started reduction =
    if reduction.state.started then Ok reduction
    else
      let state = { reduction.state with started = true } in
      let* reduction, event_id =
        emit_with_id
          (with_causes { reduction with state } [])
          (Audit.Run_started
             {
               scenario_sha256 = reduction.state.scenario_sha256;
               execution_model =
                 Execution_model.name reduction.state.config.execution_model;
             })
      in
      let reduction = with_causes reduction [ event_id ] in
      match reduction.state.initial_portfolio with
      | None -> Ok reduction
      | Some portfolio ->
          let* account = value reduction.state in
          let* margin =
            Risk.margin_snapshot reduction.state.config.risk account
          in
          let valuation = Audit.{ account; margin } in
          let* reduction, initial_event_id =
            emit_with_id reduction
              (Audit.Initial_state { portfolio; valuation })
          in
          emit
            (with_causes reduction [ initial_event_id ])
            (Audit.Valuation valuation)

  let enqueue reduction items =
    { reduction with pending = Pending_queue.enqueue reduction.pending items }

  let prepend reduction items =
    { reduction with pending = Pending_queue.prepend reduction.pending items }

  let strategy_context state now =
    let latest_bars =
      Id.Instrument.Map.bindings state.latest_bars |> List.map snd
    in
    let* valuation = value state in
    let* group_exposures = Risk.group_exposures state.config.risk valuation in
    Strategy.context ~now ~valuation ~group_exposures
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
              let* () =
                match request.Order.time_in_force with
                | Order.Day { venue_id; calendar_id } -> (
                    match
                      List.find_opt
                        (fun calendar ->
                          Id.Venue_calendar.equal calendar.Venue_calendar.id
                            calendar_id)
                        reduction.state.config.venue_calendars
                    with
                    | None -> Error "DAY order refers to an unknown calendar"
                    | Some calendar ->
                        if not (Id.Venue.equal calendar.venue_id venue_id) then
                          Error "DAY order venue differs from its calendar"
                        else if
                          not
                            (Id.Instrument.Set.mem request.instrument_id
                               calendar.instrument_ids)
                        then
                          Error
                            "DAY order calendar does not cover its instrument"
                        else Ok ())
                | Order.Gtc | Order.Ioc | Order.Fok | Order.Gtd _ -> Ok ()
              in
              let marks =
                Id.Instrument.Map.bindings reduction.state.latest_marks
              in
              let* () =
                match
                  ( reduction.state.config.financing,
                    request.Order.side,
                    Account.position_quantity reduction.state.account
                      request.instrument_id )
                with
                | Some policy, Order.Sell, position
                  when policy.Financing.locate_policy = Financing.Reject_order
                       && not (Scalar.Quantity.is_positive position) ->
                    let available =
                      match
                        Id.Instrument.Map.find_opt request.instrument_id
                          reduction.state.latest_borrow
                      with
                      | Some observation when not observation.Financing.recalled
                        ->
                          observation.available_quantity
                      | None | Some _ -> Scalar.Quantity.zero
                    in
                    let* located = Scalar.Quantity.absolute position in
                    let* reserved =
                      Oms.active_for_instrument reduction.state.oms
                        request.instrument_id
                      |> List.fold_left
                           (fun result order ->
                             let* total = result in
                             if order.Order.request.side = Order.Sell then
                               Scalar.Quantity.add total
                                 (Order.remaining_quantity order)
                             else Ok total)
                           (Ok Scalar.Quantity.zero)
                    in
                    let* requested = Scalar.Quantity.add located reserved in
                    let* requested =
                      Scalar.Quantity.add requested request.quantity
                    in
                    if Scalar.Quantity.compare requested available > 0 then
                      Error "order exceeds effective borrow availability"
                    else Ok ()
                | _ -> Ok ()
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

  let submit_recall_order reduction market_slice instrument quantity =
    let* request =
      Order.request_v8 ~instrument_id:instrument.Instrument.id ~side:Order.Buy
        ~quantity ~kind:Order.Market ~time_in_force:Order.Ioc
        ~origin:Order.Borrow_recall
    in
    let id = order_id reduction.state in
    let* state = increment_order_number reduction.state in
    let reduction = { reduction with state } in
    let* order_sequence = next_sequence reduction.state.engine_sequence in
    let created_event_id =
      Audit.event_id ~run_id:reduction.state.run_id
        ~engine_sequence:order_sequence
    in
    let eligible_after_slice_sequence =
      Int64.pred market_slice.Market_slice.slice_sequence
    in
    let* oms, order =
      Oms.accept reduction.state.oms ~id ~created_event_id
        ~accepted_sequence:order_sequence ~created_at:market_slice.start_at
        ~eligible_after_slice_sequence request
    in
    let reduction = { reduction with state = { reduction.state with oms } } in
    let* reduction, event_id =
      emit_with_id reduction (Audit.Order_accepted order)
    in
    let* pending =
      notification reduction ~causation_ids:[ event_id ]
        (Strategy.Order_updated order)
    in
    Ok (enqueue reduction [ pending ])

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

  let trigger_order reduction order_id ~triggered_at ~triggered_slice_sequence =
    let* sequence = next_sequence reduction.state.engine_sequence in
    let updated_event_id =
      Audit.event_id ~run_id:reduction.state.run_id ~engine_sequence:sequence
    in
    let* oms, order =
      Oms.trigger reduction.state.oms order_id ~updated_event_id ~triggered_at
        ~triggered_slice_sequence
    in
    let reduction =
      { reduction with state = { reduction.state with oms } }
      |> fun reduction ->
      with_causes reduction
        (order.Order.created_event_id :: reduction.causation_ids)
    in
    let* reduction, emitted_id =
      emit_with_id reduction (Audit.Order_triggered order)
    in
    if not (Id.Event.equal emitted_id updated_event_id) then
      Error "order trigger event ID prediction diverged"
    else
      let* pending =
        notification reduction ~causation_ids:[ emitted_id ]
          (Strategy.Order_updated order)
      in
      Ok (enqueue reduction [ pending ])

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
            | Order.Limit price | Order.Stop price ->
                if Scalar.Price.is_multiple price ~tick:instrument.tick_size
                then Ok ()
                else
                  Error
                    "split-adjusted order price is not aligned to the \
                     instrument tick"
            | Order.Stop_limit { trigger_price; limit_price } ->
                if
                  Scalar.Price.is_multiple trigger_price
                    ~tick:instrument.tick_size
                  && Scalar.Price.is_multiple limit_price
                       ~tick:instrument.tick_size
                then Ok ()
                else
                  Error
                    "split-adjusted order price is not aligned to the \
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

  let apply_legacy_borrow_fees reduction market_slice =
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

  let apply_observed_borrow_fees reduction market_slice policy =
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
        if not (Scalar.Quantity.is_negative quantity) then Ok reduction
        else
          match
            Id.Instrument.Map.find_opt instrument.id
              reduction.state.latest_borrow
          with
          | None -> (
              match policy.Financing.borrow_missing_data with
              | Financing.Zero -> Ok reduction
              | Financing.Reject ->
                  Error
                    (Format.asprintf
                       "open short has no effective borrow observation for %a"
                       Id.Instrument.pp instrument.id))
          | Some observation ->
              let* short_quantity = Scalar.Quantity.absolute quantity in
              let* bar =
                match Market_slice.bar market_slice instrument.id with
                | Some value -> Ok value
                | None -> Error "short position has no market slice bar"
              in
              let* notional =
                Scalar.Money.notional bar.open_price short_quantity
              in
              let* amount =
                Financing.accrue policy ~principal:notional
                  ~annual_rate_bps:observation.annual_rate_bps span
              in
              if Scalar.Money.equal amount Scalar.Money.zero then Ok reduction
              else
                let* account =
                  Account.apply_borrow_fee reduction.state.account
                    ~instrument_id:instrument.id
                    ~quote_currency:instrument.quote_currency ~fee:amount
                in
                let reduction =
                  { reduction with state = { reduction.state with account } }
                in
                emit
                  (with_causes reduction
                     (Option.to_list reduction.slice_event_id))
                  (Audit.Borrow_charge_applied
                     {
                       observation;
                       quote_currency = instrument.quote_currency;
                       short_quantity;
                       reference_price = bar.open_price;
                       day_count = policy.day_count;
                       compounding = policy.compounding;
                       period_start = market_slice.start_at;
                       period_end = market_slice.end_at;
                       amount;
                     }))
      (Ok reduction)
      (configured_instruments reduction.state)

  let apply_cash_interest reduction market_slice policy =
    let span =
      Ptime.diff market_slice.Market_slice.end_at market_slice.start_at
    in
    List.fold_left
      (fun result (currency, opening_balance) ->
        let* reduction = result in
        if Scalar.Money.equal opening_balance Scalar.Money.zero then
          Ok reduction
        else
          match
            Currency_map.find_opt currency reduction.state.latest_cash_rates
          with
          | None -> (
              match policy.Financing.cash_missing_data with
              | Financing.Zero -> Ok reduction
              | Financing.Reject ->
                  Error
                    ("nonzero cash balance has no effective rate for currency "
                   ^ currency))
          | Some observation ->
              let debit =
                Scalar.Money.compare opening_balance Scalar.Money.zero < 0
              in
              let applied_rate_bps =
                if debit then observation.debit_rate_bps
                else observation.credit_rate_bps
              in
              let* principal =
                if debit then Scalar.Money.negate opening_balance
                else Ok opening_balance
              in
              let* accrued =
                Financing.accrue policy ~principal
                  ~annual_rate_bps:applied_rate_bps span
              in
              let* amount =
                if debit then Scalar.Money.negate accrued else Ok accrued
              in
              if Scalar.Money.equal amount Scalar.Money.zero then Ok reduction
              else
                let* account =
                  Account.apply_cash_interest reduction.state.account ~currency
                    ~interest:amount
                in
                let* closing_balance =
                  match Account.cash account currency with
                  | Some value -> Ok value
                  | None -> Error "cash interest removed its currency ledger"
                in
                let reduction =
                  { reduction with state = { reduction.state with account } }
                in
                emit
                  (with_causes reduction
                     (Option.to_list reduction.slice_event_id))
                  (Audit.Cash_interest_applied
                     {
                       observation;
                       opening_balance;
                       applied_rate_bps;
                       day_count = policy.day_count;
                       compounding = policy.compounding;
                       period_start = market_slice.start_at;
                       period_end = market_slice.end_at;
                       amount;
                       closing_balance;
                     }))
      (Ok reduction)
      (Account.cash_balances reduction.state.account)

  let process_borrow_recalls reduction market_slice policy =
    List.fold_left
      (fun result instrument ->
        let* reduction = result in
        match
          Id.Instrument.Map.find_opt instrument.Instrument.id
            reduction.state.latest_borrow
        with
        | None | Some { Financing.recalled = false; _ } -> Ok reduction
        | Some observation ->
            let quantity =
              Account.position_quantity reduction.state.account instrument.id
            in
            if not (Scalar.Quantity.is_negative quantity) then Ok reduction
            else
              let* short_quantity = Scalar.Quantity.absolute quantity in
              let close_out_quantity =
                match policy.Financing.recall_policy with
                | Financing.Reject_new_shorts -> Scalar.Quantity.zero
                | Financing.Close_out -> short_quantity
              in
              let* reduction, recall_event_id =
                emit_with_id
                  (with_causes reduction
                     (Option.to_list reduction.slice_event_id))
                  (Audit.Borrow_recall_received
                     { observation; short_quantity; close_out_quantity })
              in
              let active_sells =
                Oms.active_for_instrument reduction.state.oms instrument.id
                |> List.filter_map (fun order ->
                    if order.Order.request.side = Order.Sell then Some order.id
                    else None)
              in
              let* reduction =
                cancel_orders
                  (with_causes reduction [ recall_event_id ])
                  ~reason:Audit.Borrow_recall active_sells
              in
              if Scalar.Quantity.is_zero close_out_quantity then Ok reduction
              else
                submit_recall_order
                  (with_causes reduction [ recall_event_id ])
                  market_slice instrument close_out_quantity)
      (Ok reduction)
      (configured_instruments reduction.state)

  let apply_financing reduction market_slice =
    match reduction.state.config.financing with
    | None -> apply_legacy_borrow_fees reduction market_slice
    | Some policy ->
        let* reduction = process_borrow_recalls reduction market_slice policy in
        let* reduction =
          apply_observed_borrow_fees reduction market_slice policy
        in
        apply_cash_interest reduction market_slice policy

  let process_settlements reduction (market_slice : Market_slice.t) =
    match reduction.state.config.settlement with
    | None -> Ok reduction
    | Some _ ->
        List.fold_left
          (fun result (instruction : Settlement.instruction) ->
            let* reduction = result in
            match instruction.status with
            | Settlement.Settled _ | Settlement.Failed _ -> Ok reduction
            | Settlement.Pending ->
                if not (Settlement.is_due instruction market_slice.start_at)
                then Ok reduction
                else
                  let failure =
                    List.find_opt
                      (fun (failure : Settlement.failure) ->
                        String.equal failure.instruction_id
                          instruction.instruction_id)
                      market_slice.Market_slice.settlement_failures
                  in
                  let* instruction, account, event =
                    match failure with
                    | Some failure ->
                        let* instruction =
                          Settlement.fail instruction
                            ~failed_at:market_slice.start_at
                            ~reason:failure.reason
                        in
                        Ok
                          ( instruction,
                            reduction.state.account,
                            Audit.Settlement_failed instruction )
                    | None ->
                        let* account =
                          Account.apply_settlement reduction.state.account
                            instruction
                        in
                        let* instruction =
                          Settlement.settle instruction
                            ~settled_at:market_slice.start_at
                        in
                        Ok
                          ( instruction,
                            account,
                            Audit.Settlement_completed instruction )
                  in
                  let settlement_instructions =
                    List.map
                      (fun (current : Settlement.instruction) ->
                        if
                          String.equal current.instruction_id
                            instruction.instruction_id
                        then instruction
                        else current)
                      reduction.state.settlement_instructions
                  in
                  emit
                    (with_causes
                       {
                         reduction with
                         state =
                           {
                             reduction.state with
                             account;
                             settlement_instructions;
                           };
                       }
                       (Option.to_list reduction.slice_event_id))
                    event)
          (Ok reduction) reduction.state.settlement_instructions

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
            let* () =
              Risk.check_position_for state.config.risk target.instrument_id
                target.quantity
            in
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
      let* valuation = value state in
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
            let* () =
              Risk.check_position_for state.config.risk target.instrument_id
                quantity
            in
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
    if Pending_queue.is_empty reduction.pending then Ok (Drained reduction)
    else if reduction.processed >= reduction.state.config.max_internal_events
    then
      Error
        (Printf.sprintf "internal event count exceeds configured limit of %d"
           reduction.state.config.max_internal_events)
    else
      match Pending_queue.pop reduction.pending with
      | None -> Ok (Drained reduction)
      | Some (item, pending) -> (
          let reduction =
            { reduction with pending; processed = reduction.processed + 1 }
          in
          match item with
          | Notify (causation_ids, event) ->
              let reduction = with_causes reduction causation_ids in
              let* context = strategy_context reduction.state reduction.now in
              Ok
                (Strategy_requested { reduction; causation_ids; context; event })
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
    let borrow_observations_valid =
      List.for_all
        (fun (observation : Financing.borrow_observation) ->
          Option.is_some
            (Risk.instrument state.config.risk observation.instrument_id)
          && Ptime.compare observation.effective_at market_slice.start_at <= 0
          &&
          match
            Id.Instrument.Map.find_opt observation.instrument_id
              state.latest_borrow
          with
          | None -> true
          | Some previous ->
              Ptime.compare observation.effective_at previous.effective_at > 0)
        market_slice.borrow_observations
    in
    let cash_observations_valid =
      List.for_all
        (fun (observation : Financing.cash_rate_observation) ->
          List.mem observation.currency expected_currencies
          && Ptime.compare observation.effective_at market_slice.start_at <= 0
          &&
          match
            Currency_map.find_opt observation.currency state.latest_cash_rates
          with
          | None -> true
          | Some previous ->
              Ptime.compare observation.effective_at previous.effective_at > 0)
        market_slice.cash_rate_observations
    in
    let settlement_failures_valid =
      match state.config.settlement with
      | None -> market_slice.settlement_failures = []
      | Some _ ->
          List.for_all
            (fun (failure : Settlement.failure) ->
              List.exists
                (fun (instruction : Settlement.instruction) ->
                  String.equal instruction.instruction_id failure.instruction_id
                  && instruction.status = Settlement.Pending
                  && Settlement.is_due instruction market_slice.start_at)
                state.settlement_instructions)
            market_slice.settlement_failures
    in
    if List.length ids <> List.length actual || actual <> expected then
      Error "market slice must contain each configured instrument exactly once"
    else if
      not
        (List.for_all
           (fun currency -> List.mem currency actual_currencies)
           expected_currencies)
    then Error "market slice must contain each configured currency FX rate"
    else if
      not
        (Option.exists
           (fun rate -> Scalar.Price.equal rate one)
           (Market_slice.fx_rate market_slice
              (Risk.base_currency state.config.risk)))
    then Error "market slice base-currency FX rate must equal one"
    else if not actions_valid then
      Error "corporate action is unknown or was already applied"
    else if not borrow_observations_valid then
      Error "borrow observations must be known and advance effective time"
    else if not cash_observations_valid then
      Error "cash rate observations must be known and advance effective time"
    else if not settlement_failures_valid then
      Error "settlement failures must reference due pending instructions"
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

  let fill_fee execution instrument market_slice liquidity price quantity =
    let* notional = Scalar.Money.notional price quantity in
    Execution.calculate_fee execution ~instrument ~notional ~quantity ~liquidity
      ~fx_rates:
        (List.map
           (fun mark -> (mark.Market_slice.currency, mark.rate))
           market_slice.Market_slice.fx_rates)

  let create_execution_fill execution ~id ~order_id ~instrument_id
      ~quote_currency ~side ~quantity ~price ~fee ~fee_components ~executed_at
      ~slice_sequence =
    if Execution.fee_schedules execution = [] then
      Fill.create ~id ~order_id ~instrument_id ~quote_currency ~side ~quantity
        ~price ~fee ~executed_at ~slice_sequence
    else
      Fill.create_v9 ~id ~order_id ~instrument_id ~quote_currency ~side
        ~quantity ~price ~fee ~fee_components ~executed_at ~slice_sequence

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
        let* fee_components, fee =
          fill_fee state.config.execution instrument market_slice
            proposed.Execution.liquidity proposed.price quantity
        in
        let* fill =
          create_execution_fill state.config.execution ~id:(fill_id state)
            ~order_id:order.Order.id ~instrument_id:instrument.id
            ~quote_currency:instrument.quote_currency ~side:order.request.side
            ~quantity ~price:proposed.price ~fee ~fee_components
            ~executed_at:proposed.executed_at
            ~slice_sequence:market_slice.Market_slice.slice_sequence
        in
        let* account =
          match state.config.settlement with
          | None -> Account.apply_fill state.account fill
          | Some _ -> Account.apply_unsettled_fill state.account fill
        in
        let after_position = Account.position_quantity account instrument.id in
        let* after =
          Account.value account ~instruments ~marks
            ~fx_rates:state.latest_fx_rates
        in
        Ok (fee_components, fee, account, after_position, after)
      in
      match prepared with
      | Error message -> Error (`Invalid message)
      | Ok (fee_components, fee, account, after_position, after) -> (
          let checked =
            let* () =
              match (state.config.settlement, order.Order.request.side) with
              | Some settlement, Order.Buy -> (
                  let available =
                    match settlement.Settlement.cash_buying_power with
                    | Settlement.Total_cash ->
                        Account.cash state.account instrument.quote_currency
                    | Settlement.Settled_cash ->
                        Account.settled_cash state.account
                          instrument.quote_currency
                  in
                  let available =
                    Option.value available ~default:Scalar.Money.zero
                  in
                  let available =
                    if Scalar.Money.compare available Scalar.Money.zero > 0 then
                      available
                    else Scalar.Money.zero
                  in
                  match
                    let* notional =
                      Scalar.Money.notional proposed.Execution.price quantity
                    in
                    Scalar.Money.add notional fee
                  with
                  | Error message -> Error (Risk.Invalid message)
                  | Ok cost ->
                      if Scalar.Money.compare cost available > 0 then
                        Error
                          (Risk.Limit
                             (Risk.Settlement_cash_buying_power
                                (instrument.quote_currency, available)))
                      else Ok ())
              | Some settlement, Order.Sell
                when settlement.position_availability
                     = Settlement.Settled_positions
                     && Scalar.Quantity.is_positive before_position ->
                  let available =
                    Account.settled_position_quantity state.account
                      instrument.id
                  in
                  let available =
                    if Scalar.Quantity.is_positive available then available
                    else Scalar.Quantity.zero
                  in
                  if Scalar.Quantity.compare quantity available > 0 then
                    Error
                      (Risk.Limit
                         (Risk.Settlement_position_availability
                            (instrument.id, available)))
                  else Ok ()
              | None, _ | Some _, _ -> Ok ()
            in
            let* () =
              Risk.check_post_fill_for state.config.risk
                ~instrument_id:instrument.id ~before_position ~after_position
                ~before ~after
            in
            Risk.check_reserved_fill state.config.risk ~account ~oms:state.oms
              ~marks ~fx_rates:state.latest_fx_rates ~order
              ~filled_quantity:quantity ~after
          in
          match checked with
          | Ok () -> Ok (fee_components, fee)
          | Error (Risk.Limit limit) -> Error (`Limit limit)
          | Error (Risk.Invalid message) -> Error (`Invalid message))
    in
    let lot_value = Scalar.Quantity.to_micros instrument.lot_size in
    let* policy_order_limit =
      match Risk.max_order_quantity_for state.config.risk instrument.id with
      | Some value -> Ok value
      | None -> Error "fill instrument has no risk policy"
    in
    let* borrow_constraint =
      match (state.config.financing, order.Order.request.side) with
      | Some policy, Order.Sell
        when not (Scalar.Quantity.is_positive before_position) ->
          let available =
            match
              Id.Instrument.Map.find_opt instrument.id state.latest_borrow
            with
            | None -> Scalar.Quantity.zero
            | Some observation when observation.Financing.recalled ->
                Scalar.Quantity.zero
            | Some observation -> observation.available_quantity
          in
          let* located = Scalar.Quantity.absolute before_position in
          let remaining =
            match Scalar.Quantity.subtract available located with
            | Ok value -> value
            | Error _ -> Scalar.Quantity.zero
          in
          let limit =
            Risk.Instrument_borrow_availability (instrument.id, remaining)
          in
          Ok
            (Some
               ( remaining,
                 limit,
                 match policy.Financing.locate_policy with
                 | Financing.Reject_order -> true
                 | Financing.Clip_fill -> false ))
      | _ -> Ok None
    in
    let quantity_limit =
      let risk_limit =
        Scalar.Quantity.minimum proposed.quantity policy_order_limit
      in
      match borrow_constraint with
      | None -> risk_limit
      | Some (available, _, reject) ->
          if reject && Scalar.Quantity.compare proposed.quantity available > 0
          then Scalar.Quantity.zero
          else Scalar.Quantity.minimum risk_limit available
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
        match borrow_constraint with
        | Some (available, limit, _)
          when Scalar.Quantity.compare proposed.quantity available > 0 ->
            Ok (Some limit)
        | _ -> Ok (Some (Risk.Maximum_order_quantity policy_order_limit))
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
      Ok (quantity, [], Scalar.Money.zero, limit)
    else
      match candidate quantity with
      | Ok (fee_components, fee) -> Ok (quantity, fee_components, fee, limit)
      | Error (`Invalid message) -> Error message
      | Error (`Limit _) -> Error "permitted fill violates its limiting policy"

  let apply_fill reduction market_slice proposed quantity fee_components fee =
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
                  create_execution_fill reduction.state.config.execution ~id
                    ~order_id:order.id
                    ~instrument_id:order.request.instrument_id
                    ~quote_currency:instrument.Instrument.quote_currency
                    ~side:order.request.side ~quantity ~price:proposed.price
                    ~fee ~fee_components ~executed_at:proposed.executed_at
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
                          match reduction.state.config.settlement with
                          | None ->
                              Account.apply_fill reduction.state.account fill
                          | Some _ ->
                              Account.apply_unsettled_fill
                                reduction.state.account fill
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
                                let* reduction =
                                  match reduction.state.config.settlement with
                                  | None -> Ok reduction
                                  | Some policy ->
                                      let* instruction =
                                        Settlement.instruction policy fill
                                      in
                                      let state =
                                        {
                                          reduction.state with
                                          settlement_instructions =
                                            reduction.state
                                              .settlement_instructions
                                            @ [ instruction ];
                                        }
                                      in
                                      emit
                                        (with_causes { reduction with state }
                                           [ event_id ])
                                        (Audit.Settlement_instruction_created
                                           instruction)
                                in
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
        let* permitted_quantity, fee_components, fee, limit =
          permitted_fill reduction.state market_slice order proposed instrument
        in
        let permitted_quantity =
          if
            Order.is_fok order
            && Scalar.Quantity.compare permitted_quantity
                 (Order.remaining_quantity order)
               < 0
          then Scalar.Quantity.zero
          else permitted_quantity
        in
        let* reduction =
          match limit with
          | None -> Ok reduction
          | Some limit ->
              if
                String.equal reduction.state.config.contract_version
                  Contract.legacy_journal_version
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
          apply_fill reduction market_slice proposed permitted_quantity
            fee_components fee
          |> Result.map (fun reduction -> (reduction, permitted_quantity))

  let cancel_immediate_remainders reduction order_ids =
    let causation_ids = reduction.causation_ids in
    let rec cancel reduction = function
      | [] -> Ok (with_causes reduction causation_ids)
      | order_id :: remaining -> (
          match Oms.find reduction.state.oms order_id with
          | None -> Error "immediate order disappeared during matching"
          | Some order -> (
              let reason =
                if
                  (not
                     (String.equal reduction.state.config.contract_version "8"))
                  && Order.is_market order
                then Audit.Market_ioc
                else if Order.is_fok order then Audit.Fill_or_kill
                else Audit.Immediate_or_cancel
              in
              let result =
                if Order.is_active order then
                  cancel_order
                    (with_causes reduction causation_ids)
                    ~reason order_id
                else Ok reduction
              in
              match result with
              | Error _ as error -> error
              | Ok reduction -> cancel reduction remaining))
    in
    cancel reduction order_ids

  let cancel_expired_gtd reduction (market_slice : Market_slice.t) =
    Oms.active_orders reduction.state.oms
    |> List.filter_map (fun order ->
        match order.Order.request.time_in_force with
        | Order.Gtd expires_at
          when Ptime.compare expires_at market_slice.end_at <= 0 ->
            Some order.id
        | Order.Gtc | Order.Ioc | Order.Fok | Order.Day _ | Order.Gtd _ -> None)
    |> cancel_orders reduction ~reason:Audit.Gtd_expired

  let day_session_closed state (market_slice : Market_slice.t) order =
    match order.Order.request.time_in_force with
    | Order.Day { calendar_id; _ } -> (
        match
          List.find_opt
            (fun calendar ->
              Id.Venue_calendar.equal calendar.Venue_calendar.id calendar_id)
            state.config.venue_calendars
        with
        | None -> false
        | Some calendar ->
            List.exists
              (fun (session : Venue_calendar.session) ->
                match List.rev session.phases with
                | [] -> false
                | phase :: _ ->
                    Ptime.compare phase.closes_at order.Order.created_at > 0
                    && Ptime.compare phase.closes_at market_slice.end_at <= 0)
              calendar.sessions)
    | Order.Gtc | Order.Ioc | Order.Fok | Order.Gtd _ -> false

  let cancel_expired_day reduction market_slice =
    Oms.active_orders reduction.state.oms
    |> List.filter_map (fun order ->
        if day_session_closed reduction.state market_slice order then
          Some order.Order.id
        else None)
    |> cancel_orders reduction ~reason:Audit.Day_expired

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
                let order_limit =
                  Risk.max_order_quantity_for state.config.risk instrument_id
                  |> Option.value
                       ~default:(Risk.max_order_quantity state.config.risk)
                in
                let bounded = Scalar.Quantity.minimum delta order_limit in
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
          let order_limit =
            Risk.max_order_quantity_for reduction.state.config.risk
              instrument.Instrument.id
            |> Option.value
                 ~default:(Risk.max_order_quantity reduction.state.config.risk)
          in
          let bounded = Scalar.Quantity.minimum absolute order_limit in
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

  module Validation_phase = struct
    let run state market_slice =
      if state.completed then
        Error "completed engine cannot process another market slice"
      else validate_slice state market_slice
  end

  module Initialize_phase = struct
    let run state market_slice =
      let applied_action_ids =
        List.fold_left
          (fun ids action ->
            Id.Corporate_action.Set.add action.Corporate_action.id ids)
          state.applied_action_ids market_slice.Market_slice.corporate_actions
      in
      let latest_bars =
        List.fold_left
          (fun bars bar -> Id.Instrument.Map.add bar.Bar.instrument_id bar bars)
          state.latest_bars market_slice.bars
      in
      let latest_marks =
        List.fold_left
          (fun marks bar ->
            Id.Instrument.Map.add bar.Bar.instrument_id bar.close_price marks)
          state.latest_marks market_slice.bars
      in
      let latest_borrow =
        List.fold_left
          (fun observations (observation : Financing.borrow_observation) ->
            Id.Instrument.Map.add observation.instrument_id observation
              observations)
          state.latest_borrow market_slice.borrow_observations
      in
      let latest_cash_rates =
        List.fold_left
          (fun observations (observation : Financing.cash_rate_observation) ->
            Currency_map.add observation.currency observation observations)
          state.latest_cash_rates market_slice.cash_rate_observations
      in
      let reduction =
        {
          state;
          now = market_slice.received_at;
          current_slice_sequence = market_slice.slice_sequence;
          slice_event_id = None;
          causation_ids = [];
          audits_rev = [];
          pending = Pending_queue.empty;
          processed = 0;
        }
      in
      let* reduction = ensure_started reduction in
      let state =
        {
          reduction.state with
          last_slice_sequence = Some market_slice.slice_sequence;
          last_slice_end = Some market_slice.end_at;
          last_received_at = Some market_slice.received_at;
          latest_fx_rates =
            List.map
              (fun mark -> (mark.Market_slice.currency, mark.Market_slice.rate))
              market_slice.fx_rates;
          latest_bars;
          latest_marks;
          latest_borrow;
          latest_cash_rates;
          applied_action_ids;
        }
      in
      let reduction = { reduction with state } in
      let* reduction, slice_event_id =
        emit_with_id (with_causes reduction [])
          (Audit.Market_slice_received market_slice)
      in
      Ok
        {
          reduction with
          slice_event_id = Some slice_event_id;
          causation_ids = [ slice_event_id ];
        }
  end

  module Actions_phase = struct
    let run market_slice reduction =
      let* reduction = cancel_expired_gtd reduction market_slice in
      let* reduction = process_settlements reduction market_slice in
      apply_corporate_actions reduction
        market_slice.Market_slice.corporate_actions
  end

  module Borrow_phase = struct
    let run market_slice reduction = apply_financing reduction market_slice
  end

  module Notifications_phase = struct
    type request = {
      reduction : reduction;
      causation_ids : Id.Event.t list;
      context : Strategy.context;
      event : Strategy.event;
    }

    type outcome = Drained of reduction | Awaiting of request

    let run reduction =
      match drain reduction with
      | Error _ as error -> error
      | Ok result -> (
          match (result : drain_result) with
          | Drained reduction -> Ok (Drained reduction)
          | Strategy_requested { reduction; causation_ids; context; event } ->
              Ok (Awaiting { reduction; causation_ids; context; event }))

    let has_pending reduction = not (Pending_queue.is_empty reduction.pending)
    let payload request = (request.context, request.event)

    let resume request intents =
      let actions =
        List.map (fun intent -> Act (request.causation_ids, intent)) intents
      in
      prepend request.reduction actions
  end

  module Matching_phase = struct
    type outcome =
      | Continue of reduction * Execution.cursor
      | Complete of reduction

    let start market_slice reduction =
      Execution_model.start_slice reduction.state.config.execution_model
        reduction.state.config.execution
        ~instruments:(configured_instruments reduction.state)
        ~oms:reduction.state.oms market_slice

    let run market_slice cursor reduction =
      match Execution.next cursor ~oms:reduction.state.oms with
      | Error _ as error -> error
      | Ok
          (Execution.Triggered
             (order_id, triggered_at, triggered_slice_sequence, cursor)) ->
          let* reduction =
            trigger_order reduction order_id ~triggered_at
              ~triggered_slice_sequence
          in
          Ok (Continue (reduction, cursor))
      | Ok (Execution.Proposed (proposed, advance)) ->
          let* reduction, applied_quantity =
            apply_proposed_fill reduction market_slice proposed
          in
          let* cursor = advance applied_quantity in
          Ok (Continue (reduction, cursor))
      | Ok (Execution.Finished market_ioc_orders) ->
          let* slice_event_id =
            match reduction.slice_event_id with
            | Some value -> Ok value
            | None -> Error "matching slice has no audit event"
          in
          let reduction = with_causes reduction [ slice_event_id ] in
          let* reduction =
            cancel_immediate_remainders reduction market_ioc_orders
          in
          let* reduction = cancel_expired_day reduction market_slice in
          let* pending =
            notification reduction ~causation_ids:[ slice_event_id ]
              (Strategy.Market_slice_closed market_slice)
          in
          Ok (Complete (enqueue reduction [ pending ]))
  end

  module Targets_phase = struct
    let run = reconcile_targets
  end

  module Margin_phase = struct
    let run = assess_margin
  end

  module Valuation_phase = struct
    let run reduction =
      let* reduction = valuation reduction in
      Ok (reduction.state, List.rev reduction.audits_rev)
  end

  module Phase_machine = Reducer_phases.Make (struct
    type nonrec state = t
    type nonrec reduction = reduction
    type market_slice = Market_slice.t
    type cursor = Execution.cursor
    type audit = Audit.t
    type context = Strategy.context
    type event = Strategy.event
    type intent = Strategy.intent

    module Validation = Validation_phase
    module Initialize = Initialize_phase
    module Actions = Actions_phase
    module Borrow = Borrow_phase
    module Notifications = Notifications_phase
    module Matching = Matching_phase
    module Targets = Targets_phase
    module Margin = Margin_phase
    module Valuation = Valuation_phase
  end)

  type progress = Phase_machine.progress

  let strategy_request = Phase_machine.strategy_request
  let slice_result = Phase_machine.slice_result
  let resume = Phase_machine.resume
  let process_slice = Phase_machine.process_slice

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
              pending = Pending_queue.empty;
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

  let create_with_portfolio ~run_id ~scenario_sha256 ~config ~initial_portfolio
      ~strategy_state =
    Interactive.create_with_portfolio ~run_id ~scenario_sha256 ~config
      ~initial_portfolio
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
