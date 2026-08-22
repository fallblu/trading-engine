module Int64_map = Map.Make (Int64)
module String_set = Set.Make (String)

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let fail ~json_path message = Error (Scenario_shape.error ~json_path message)

let at json_path result =
  Result.map_error
    (fun message -> Scenario_shape.error ~json_path message)
    result

let child root field = root ^ "." ^ field

let validate_venue_calendars ~root catalog venue_calendars =
  let ids =
    List.map (fun calendar -> calendar.Venue_calendar.id) venue_calendars
  in
  let unique_ids = List.sort_uniq Id.Venue_calendar.compare ids in
  if List.length ids <> List.length unique_ids then
    fail
      ~json_path:(child root "venue_calendars")
      "venue calendar IDs must be unique"
  else
    let coverage, overlap =
      List.fold_left
        (fun (covered, overlap) calendar ->
          let members = calendar.Venue_calendar.instrument_ids in
          ( Id.Instrument.Set.union covered members,
            overlap
            || not
                 (Id.Instrument.Set.is_empty
                    (Id.Instrument.Set.inter covered members)) ))
        (Id.Instrument.Set.empty, false)
        venue_calendars
    in
    if overlap then
      fail
        ~json_path:(child root "venue_calendars")
        "each instrument must reference exactly one venue calendar"
    else if not (Id.Instrument.Set.equal coverage catalog) then
      fail
        ~json_path:(child root "venue_calendars")
        "venue calendars must cover every configured instrument exactly once"
    else Ok ()

let header ~root ~contract_version ~base_currency ~initial_cash ~instruments
    ~venue_calendars ~max_internal_events =
  let* () =
    if
      List.mem contract_version
        [ "14"; "13"; "12"; "11"; "10"; "9"; "8"; "7"; "6" ]
    then Ok ()
    else
      Account.create ~base_currency ~initial_cash
      |> Result.map (fun _ -> ())
      |> at (child root "initial_cash")
  in
  if instruments = [] then
    fail ~json_path:(child root "instruments")
      "scenario must define at least one instrument"
  else
    let catalog =
      List.map (fun instrument -> instrument.Instrument.id) instruments
      |> Id.Instrument.Set.of_list
    in
    if Id.Instrument.Set.cardinal catalog <> List.length instruments then
      fail ~json_path:(child root "instruments") "instrument IDs must be unique"
    else
      let* () =
        if
          List.mem contract_version
            [ "14"; "13"; "12"; "11"; "10"; "9"; "8"; "7"; "6"; "5" ]
        then validate_venue_calendars ~root catalog venue_calendars
        else Ok ()
      in
      let currencies =
        base_currency
        :: List.map
             (fun instrument -> instrument.Instrument.quote_currency)
             instruments
        |> List.sort_uniq String.compare
      in
      let cash_currencies =
        List.map fst initial_cash |> List.sort_uniq String.compare
      in
      if cash_currencies <> currencies then
        fail
          ~json_path:
            (child root
               (if
                  List.mem contract_version
                    [ "14"; "13"; "12"; "11"; "10"; "9"; "8"; "7"; "6" ]
                then "initial_portfolio.cash"
                else "initial_cash"))
          "initial cash must contain every scenario currency exactly once"
      else if max_internal_events <= 0 then
        fail
          ~json_path:(child root "max_internal_events")
          "max_internal_events must be positive"
      else if max_internal_events > Resource_limits.internal_events then
        fail
          ~json_path:(child root "max_internal_events")
          (Printf.sprintf "internal event count is %d; limit is %d"
             max_internal_events Resource_limits.internal_events)
      else Ok (currencies, catalog)

let initial_portfolio ~root ~currencies ~catalog ~instruments ~risk initial =
  let path = child root "initial_portfolio" in
  let cash_currencies = List.map fst initial.Initial_portfolio.cash in
  let fx_currencies = List.map fst initial.fx_rates in
  let expected_currencies = List.sort String.compare currencies in
  if List.sort String.compare cash_currencies <> expected_currencies then
    fail ~json_path:(child path "cash")
      "initial cash must contain every scenario currency exactly once"
  else if
    not
      (List.for_all
         (fun currency -> List.mem currency fx_currencies)
         expected_currencies)
  then
    fail ~json_path:(child path "fx_rates")
      "initial FX rates must contain every scenario currency"
  else
    let instrument_map =
      List.fold_left
        (fun map instrument ->
          Id.Instrument.Map.add instrument.Instrument.id instrument map)
        Id.Instrument.Map.empty instruments
    in
    let* () =
      List.fold_left
        (fun result (position : Initial_portfolio.position) ->
          let* () = result in
          if not (Id.Instrument.Set.mem position.instrument_id catalog) then
            fail ~json_path:(child path "positions")
              "initial position refers to an unknown instrument"
          else
            match
              Id.Instrument.Map.find_opt position.instrument_id instrument_map
            with
            | None -> assert false
            | Some instrument ->
                if
                  not
                    (Scalar.Quantity.is_multiple position.quantity
                       ~lot:instrument.Instrument.lot_size)
                then
                  fail ~json_path:(child path "positions")
                    "initial position quantity is not aligned to its \
                     instrument lot"
                else
                  Risk.check_position_for risk position.instrument_id
                    position.quantity
                  |> at (child path "positions"))
        (Ok ()) initial.positions
    in
    let* () =
      List.fold_left
        (fun result (instrument_id, mark) ->
          let* () = result in
          if not (Id.Instrument.Set.mem instrument_id catalog) then
            fail ~json_path:(child path "marks")
              "initial mark refers to an unknown instrument"
          else
            match Id.Instrument.Map.find_opt instrument_id instrument_map with
            | None -> assert false
            | Some instrument ->
                if Scalar.Price.is_multiple mark ~tick:instrument.tick_size then
                  Ok ()
                else
                  fail ~json_path:(child path "marks")
                    "initial mark is not aligned to its instrument tick size")
        (Ok ()) initial.marks
    in
    let* account = Account.of_initial_portfolio initial |> at path in
    let* valuation =
      Account.value account ~instruments ~marks:initial.marks
        ~fx_rates:initial.fx_rates
      |> at path
    in
    Risk.check_initial risk valuation |> at path

let changes_orders = function
  | Strategy.Target_weights _ | Strategy.Target_quantities _
  | Strategy.Submit_order _ | Strategy.Cancel_order _ ->
      true
  | Strategy.Emit_metric _ -> false

let validate_portfolio_target ~json_path risk catalog = function
  | Strategy.Target_weights targets ->
      let ids =
        List.map
          (fun (target : Strategy.weight_target) -> target.instrument_id)
          targets
      in
      let unique = List.sort_uniq Id.Instrument.compare ids in
      if List.length unique <> List.length ids then
        fail ~json_path
          "target_weights must contain each instrument exactly once"
      else if
        not (Id.Instrument.Set.equal catalog (Id.Instrument.Set.of_list ids))
      then
        fail ~json_path "target_weights must cover every configured instrument"
      else
        let gross =
          List.fold_left
            (fun result (target : Strategy.weight_target) ->
              let* total = result in
              let* absolute =
                Scalar.Weight.absolute target.Strategy.weight |> at json_path
              in
              Scalar.Weight.add total absolute |> at json_path)
            (Ok Scalar.Weight.zero) targets
        in
        let* gross = gross in
        if
          Int64.compare
            (Scalar.Weight.to_micros gross)
            (Scalar.Ratio.to_micros (Risk.max_leverage risk))
          > 0
        then fail ~json_path "target gross weight exceeds maximum leverage"
        else Ok ()
  | Strategy.Target_quantities targets ->
      let ids =
        List.map
          (fun (target : Strategy.quantity_target) -> target.instrument_id)
          targets
      in
      let unique = List.sort_uniq Id.Instrument.compare ids in
      if List.length unique <> List.length ids then
        fail ~json_path
          "target_quantities must contain each instrument exactly once"
      else if
        not (Id.Instrument.Set.equal catalog (Id.Instrument.Set.of_list ids))
      then
        fail ~json_path
          "target_quantities must cover every configured instrument"
      else
        List.fold_left
          (fun result (target : Strategy.quantity_target) ->
            let* () = result in
            match Risk.instrument risk target.Strategy.instrument_id with
            | None ->
                fail ~json_path
                  "target quantity refers to an unknown instrument"
            | Some instrument ->
                if
                  not
                    (Scalar.Quantity.is_multiple target.quantity
                       ~lot:instrument.Instrument.lot_size)
                then
                  fail ~json_path
                    "target quantity is not aligned to its instrument lot"
                else
                  Risk.check_position_for risk target.instrument_id
                    target.quantity
                  |> at json_path)
          (Ok ()) targets
  | Strategy.Submit_order request -> (
      if not (Id.Instrument.Set.mem request.Order.instrument_id catalog) then
        fail ~json_path "order refers to an unknown instrument"
      else
        match
          ( Risk.instrument risk request.instrument_id,
            Risk.max_order_quantity_for risk request.instrument_id )
        with
        | None, _ -> fail ~json_path "order refers to an unknown instrument"
        | _, None -> fail ~json_path "order has no instrument risk policy"
        | Some instrument, Some order_limit -> (
            if Scalar.Quantity.compare request.quantity order_limit > 0 then
              fail ~json_path
                "order exceeds the instrument maximum order quantity"
            else if
              not
                (Scalar.Quantity.is_multiple request.quantity
                   ~lot:instrument.Instrument.lot_size)
            then
              fail ~json_path
                "order quantity is not aligned to the instrument lot size"
            else
              match request.kind with
              | Order.Market -> Ok ()
              | Order.Limit price | Order.Stop price ->
                  if Scalar.Price.is_multiple price ~tick:instrument.tick_size
                  then Ok ()
                  else
                    fail ~json_path
                      "order price is not aligned to the instrument tick size"
              | Order.Stop_limit { trigger_price; limit_price } ->
                  if
                    Scalar.Price.is_multiple trigger_price
                      ~tick:instrument.tick_size
                    && Scalar.Price.is_multiple limit_price
                         ~tick:instrument.tick_size
                  then Ok ()
                  else
                    fail ~json_path
                      "order price is not aligned to the instrument tick size"))
  | Strategy.Cancel_order _ | Strategy.Emit_metric _ -> Ok ()

let validate_slices_at ~paths ~base_currency ~currencies ~instruments slices =
  let catalog =
    List.map (fun instrument -> instrument.Instrument.id) instruments
    |> Id.Instrument.Set.of_list
  in
  let instrument_map =
    List.fold_left
      (fun map instrument ->
        Id.Instrument.Map.add instrument.Instrument.id instrument map)
      Id.Instrument.Map.empty instruments
  in
  let expected_currencies = String_set.of_list currencies in
  let one = Scalar.Price.of_decimal_string "1" |> Result.get_ok in
  let rec validate index previous_sequence previous_end previous_received
      action_ids = function
    | [] -> Ok ()
    | market_slice :: remaining ->
        let root = List.nth paths index in
        let ids =
          List.map
            (fun bar -> bar.Bar.instrument_id)
            market_slice.Market_slice.bars
          |> Id.Instrument.Set.of_list
        in
        let fx_currencies =
          List.map
            (fun mark -> mark.Market_slice.currency)
            market_slice.Market_slice.fx_rates
          |> String_set.of_list
        in
        let actions_valid =
          List.for_all
            (fun action ->
              Id.Instrument.Set.mem action.Corporate_action.instrument_id
                catalog
              &&
              match action.kind with
              | Corporate_action.Distribution { destination_instrument_id; _ }
                ->
                  Id.Instrument.Set.mem destination_instrument_id catalog
              | Split _ | Cash_dividend _ -> true)
            market_slice.corporate_actions
        in
        let lifecycle_valid =
          List.for_all
            (fun (event : Instrument_lifecycle.event) ->
              Id.Instrument.Set.mem event.instrument_id catalog)
            market_slice.lifecycle_events
        in
        let duplicate_action =
          let current_ids =
            List.map
              (fun action -> action.Corporate_action.id)
              market_slice.corporate_actions
            @ List.map
                (fun (event : Instrument_lifecycle.event) -> event.id)
                market_slice.lifecycle_events
          in
          List.find_opt
            (fun id -> Id.Corporate_action.Set.mem id action_ids)
            current_ids
        in
        let bars_aligned =
          List.for_all
            (fun bar ->
              match
                Id.Instrument.Map.find_opt bar.Bar.instrument_id instrument_map
              with
              | None -> false
              | Some instrument ->
                  List.for_all
                    (fun price ->
                      Scalar.Price.is_multiple price ~tick:instrument.tick_size)
                    [
                      bar.open_price;
                      bar.high_price;
                      bar.low_price;
                      bar.close_price;
                    ]
                  && Option.for_all
                       (fun volume ->
                         Scalar.Quantity.is_multiple volume
                           ~lot:instrument.lot_size)
                       bar.volume)
            market_slice.bars
        in
        let market_events_valid =
          List.for_all
            (fun (event : Market_event.t) ->
              match
                Id.Instrument.Map.find_opt event.instrument_id instrument_map
              with
              | None -> false
              | Some instrument ->
                  let prices, quantities =
                    match event.kind with
                    | Market_event.Quote
                        { bid_price; bid_quantity; ask_price; ask_quantity } ->
                        ( [ bid_price; ask_price ],
                          [ bid_quantity; ask_quantity ] )
                    | Trade { price; quantity; _ } -> ([ price ], [ quantity ])
                  in
                  List.for_all
                    (fun price ->
                      Scalar.Price.is_multiple price ~tick:instrument.tick_size)
                    prices
                  && List.for_all
                       (fun quantity ->
                         Scalar.Quantity.is_multiple quantity
                           ~lot:instrument.lot_size)
                       quantities
                  && Ptime.compare event.event_at market_slice.start_at >= 0
                  && Ptime.compare event.event_at market_slice.end_at <= 0
                  && Ptime.compare event.available_at market_slice.available_at
                     <= 0
                  && Ptime.compare event.received_at market_slice.received_at
                     <= 0)
            market_slice.market_events
        in
        if not (Id.Instrument.Set.equal catalog ids) then
          fail ~json_path:(child root "bars")
            "each market slice must contain every configured instrument"
        else if not (String_set.subset expected_currencies fx_currencies) then
          fail ~json_path:(child root "fx_rates")
            "each market slice must contain every scenario currency FX rate"
        else if
          not
            (Option.exists
               (fun rate -> Scalar.Price.equal rate one)
               (Market_slice.fx_rate market_slice base_currency))
        then
          fail ~json_path:(child root "fx_rates")
            "the base-currency FX rate must equal one"
        else if not actions_valid then
          fail
            ~json_path:(child root "corporate_actions")
            "corporate action refers to an unknown instrument"
        else if not lifecycle_valid then
          fail
            ~json_path:(child root "lifecycle_events")
            "lifecycle event refers to an unknown instrument"
        else if Option.is_some duplicate_action then
          fail
            ~json_path:(child root "corporate_actions")
            "corporate action IDs must be unique across the scenario"
        else if not bars_aligned then
          fail ~json_path:(child root "bars")
            "market prices and volumes must align with instrument increments"
        else if not market_events_valid then
          fail
            ~json_path:(child root "market_events")
            "market events must be known, aligned, and observable within the \
             slice"
        else if
          Option.exists
            (fun sequence ->
              Int64.compare market_slice.slice_sequence sequence <= 0)
            previous_sequence
        then
          fail
            ~json_path:(child root "slice_sequence")
            "market slice sequence must increase"
        else if
          Option.exists
            (fun end_at -> Ptime.compare market_slice.start_at end_at < 0)
            previous_end
        then
          fail ~json_path:(child root "start_at")
            "market slice start must not precede previous end"
        else if
          Option.exists
            (fun received_at ->
              Ptime.compare market_slice.received_at received_at < 0)
            previous_received
        then
          fail ~json_path:(child root "received_at")
            "market slice receipt time must not move backward"
        else
          let action_ids =
            List.fold_left
              (fun ids action ->
                Id.Corporate_action.Set.add action.Corporate_action.id ids)
              action_ids market_slice.corporate_actions
            |> fun ids ->
            List.fold_left
              (fun ids (event : Instrument_lifecycle.event) ->
                Id.Corporate_action.Set.add event.id ids)
              ids market_slice.lifecycle_events
          in
          validate (index + 1) (Some market_slice.slice_sequence)
            (Some market_slice.end_at) (Some market_slice.received_at)
            action_ids remaining
  in
  validate 0 None None None Id.Corporate_action.Set.empty slices

let validate_schedule ~root risk catalog schedule slices =
  let rec index_slices index = function
    | [] -> index
    | [ anchor ] ->
        Int64_map.add anchor.Market_slice.slice_sequence (anchor, None) index
    | anchor :: (next :: _ as remaining) ->
        let index =
          Int64_map.add anchor.Market_slice.slice_sequence (anchor, Some next)
            index
        in
        index_slices index remaining
  in
  let slice_index = index_slices Int64_map.empty slices in
  let validate_item index sequence intents =
    let item_root = Printf.sprintf "%s[%d]" root index in
    if Int64.compare sequence 0L <= 0 then
      fail
        ~json_path:(child item_root "after_slice_sequence")
        "scheduled slice sequence must be positive"
    else
      match Int64_map.find_opt sequence slice_index with
      | None ->
          fail
            ~json_path:(child item_root "after_slice_sequence")
            (Printf.sprintf
               "scheduled intents refer to missing market slice sequence %Ld"
               sequence)
      | Some (anchor, next) -> (
          let intents_path = child item_root "intents" in
          let* () =
            List.fold_left
              (fun result intent ->
                let* () = result in
                validate_portfolio_target ~json_path:intents_path risk catalog
                  intent)
              (Ok ()) intents
          in
          match next with
          | Some next
            when List.exists changes_orders intents
                 && Ptime.compare anchor.received_at next.start_at > 0 ->
              fail ~json_path:intents_path
                (Printf.sprintf
                   "scheduled order intent after slice %Ld is received after \
                    the next executable market slice starts"
                   sequence)
          | None | Some _ -> Ok ())
  in
  let rec validate index previous = function
    | [] -> Ok ()
    | (sequence, intents) :: remaining ->
        if
          Option.exists
            (fun prior -> Int64.compare sequence prior <= 0)
            previous
        then
          fail
            ~json_path:(Printf.sprintf "%s[%d].after_slice_sequence" root index)
            "schedule sequences must increase"
        else
          let* () = validate_item index sequence intents in
          validate (index + 1) (Some sequence) remaining
  in
  validate 0 None schedule

let batch ~root ~base_currency ~currencies ~instruments ~risk ~catalog ~schedule
    ~slices =
  let slice_paths =
    List.mapi (fun index _ -> Printf.sprintf "%s.slices[%d]" root index) slices
  in
  let* () =
    validate_slices_at ~paths:slice_paths ~base_currency ~currencies
      ~instruments slices
  in
  validate_schedule ~root:(child root "schedule") risk catalog schedule slices

let stream_item ~root ~base_currency ~instruments ~risk ~previous_slice
    ~previous_intents ~prior_action_ids ~(market_slice : Market_slice.t)
    ~intents =
  let catalog =
    List.map (fun instrument -> instrument.Instrument.id) instruments
    |> Id.Instrument.Set.of_list
  in
  let current_slice_path = child root "market_slice" in
  let* action_ids =
    List.fold_left
      (fun result action ->
        let* ids = result in
        if Id.Corporate_action.Set.mem action.Corporate_action.id ids then
          fail
            ~json_path:(child current_slice_path "corporate_actions")
            "corporate action IDs must be unique across the scenario stream"
        else Ok (Id.Corporate_action.Set.add action.id ids))
      (Ok prior_action_ids) market_slice.corporate_actions
  in
  let* action_ids =
    List.fold_left
      (fun result (event : Instrument_lifecycle.event) ->
        let* ids = result in
        if Id.Corporate_action.Set.mem event.id ids then
          fail
            ~json_path:(child current_slice_path "lifecycle_events")
            "action and lifecycle IDs must be unique across the scenario stream"
        else Ok (Id.Corporate_action.Set.add event.id ids))
      (Ok action_ids) market_slice.lifecycle_events
  in
  let currencies =
    base_currency
    :: List.map
         (fun instrument -> instrument.Instrument.quote_currency)
         instruments
    |> List.sort_uniq String.compare
  in
  let slices, paths =
    match previous_slice with
    | None -> ([ market_slice ], [ current_slice_path ])
    | Some previous ->
        ([ previous; market_slice ], [ current_slice_path; current_slice_path ])
  in
  let* () =
    validate_slices_at ~paths ~base_currency ~currencies ~instruments slices
  in
  let intents_path = child root "intents" in
  let* () =
    List.fold_left
      (fun result intent ->
        let* () = result in
        validate_portfolio_target ~json_path:intents_path risk catalog intent)
      (Ok ()) intents
  in
  let* () =
    match previous_slice with
    | Some previous
      when List.exists changes_orders previous_intents
           && Ptime.compare previous.received_at market_slice.start_at > 0 ->
        fail
          ~json_path:(child current_slice_path "start_at")
          (Printf.sprintf
             "scheduled order intent after slice %Ld is received after the \
              next executable market slice starts"
             previous.slice_sequence)
    | None | Some _ -> Ok ()
  in
  Ok action_ids
