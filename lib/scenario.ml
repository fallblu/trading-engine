type t = {
  metadata : Yojson.Safe.t;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : Scalar.Money.t;
  instruments : Instrument.t list;
  risk : Risk.t;
  execution : Execution.t;
  max_internal_events : int;
  schedule : (int64 * Strategy.intent list) list;
  slices : Market_slice.t list;
}

module Int64_set = Set.Make (Int64)

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let object_fields ~name ~expected = function
  | `Assoc fields ->
      let names = List.map fst fields in
      let actual = List.sort_uniq String.compare names in
      let expected = List.sort_uniq String.compare expected in
      if List.length names <> List.length actual then
        let duplicates =
          List.filter
            (fun key -> List.length (List.filter (String.equal key) names) > 1)
            actual
        in
        Error
          (Printf.sprintf "%s has duplicate JSON fields: [%s]" name
             (String.concat "," duplicates))
      else if actual = expected then Ok fields
      else
        let missing =
          List.filter (fun key -> not (List.mem key actual)) expected
        in
        let extra =
          List.filter (fun key -> not (List.mem key expected)) actual
        in
        Error
          (Printf.sprintf "%s fields differ: missing=[%s], extra=[%s]" name
             (String.concat "," missing)
             (String.concat "," extra))
  | _ -> Error (name ^ " must be a JSON object")

let field fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("missing JSON field: " ^ name)

let string ~name = function
  | `String value -> Ok value
  | _ -> Error (name ^ " must be a string")

let integer ~name = function
  | `Int value -> Ok value
  | _ -> Error (name ^ " must be an integer")

let list ~name = function
  | `List values -> Ok values
  | _ -> Error (name ^ " must be an array")

let map_list parse values =
  let step result value =
    let* values = result in
    let* value = parse value in
    Ok (value :: values)
  in
  List.fold_left step (Ok []) values |> Result.map List.rev

let parse_id parse ~name json =
  let* value = string ~name json in
  parse value

let parse_int64 ~name json =
  let* value = string ~name json in
  match Int64.of_string_opt value with
  | Some parsed when not (String.equal (Int64.to_string parsed) value) ->
      Error (name ^ " must use canonical integer form")
  | Some value -> Ok value
  | None -> Error (name ^ " must be an int64 encoded as a string")

let parse_quantity ~name json =
  let* value = string ~name json in
  Scalar.Quantity.of_string value

let parse_price ~name json =
  let* value = string ~name json in
  Scalar.Price.of_decimal_string value

let parse_money ~name json =
  let* value = string ~name json in
  Scalar.Money.of_decimal_string value

let parse_weight ~name json =
  let* value = string ~name json in
  Scalar.Weight.of_decimal_string value

let parse_timestamp ~name json =
  let* value = string ~name json in
  Codec.ptime_of_string value

let rec validate_metadata = function
  | `Assoc fields ->
      let names = List.map fst fields in
      let unique = List.sort_uniq String.compare names in
      if List.length names <> List.length unique then
        Error "metadata must not contain duplicate object keys"
      else
        List.fold_left
          (fun result (_, value) ->
            let* () = result in
            validate_metadata value)
          (Ok ()) fields
  | `List values ->
      List.fold_left
        (fun result value ->
          let* () = result in
          validate_metadata value)
        (Ok ()) values
  | `Float value when not (Float.is_finite value) ->
      Error "metadata numbers must be finite"
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `Floatlit _ | `String _ ->
      Ok ()
  | `Tuple _ | `Variant _ -> Error "metadata must contain only JSON values"

let parse_instrument json =
  let* fields =
    object_fields ~name:"instrument"
      ~expected:
        [ "instrument_id"; "symbol"; "quote_currency"; "tick_size"; "lot_size" ]
      json
  in
  let* id_json = field fields "instrument_id" in
  let* id = parse_id Id.Instrument.of_string ~name:"instrument_id" id_json in
  let* symbol_json = field fields "symbol" in
  let* symbol = string ~name:"symbol" symbol_json in
  let* currency_json = field fields "quote_currency" in
  let* quote_currency = string ~name:"quote_currency" currency_json in
  let* tick_json = field fields "tick_size" in
  let* tick_size = parse_price ~name:"tick_size" tick_json in
  let* lot_json = field fields "lot_size" in
  let* lot_size = parse_quantity ~name:"lot_size" lot_json in
  Instrument.create ~id ~symbol ~quote_currency ~tick_size ~lot_size

let parse_risk base_currency instruments json =
  let* fields =
    object_fields ~name:"risk"
      ~expected:[ "max_order_quantity"; "max_position" ]
      json
  in
  let* order_json = field fields "max_order_quantity" in
  let* max_order_quantity =
    parse_quantity ~name:"max_order_quantity" order_json
  in
  let* position_json = field fields "max_position" in
  let* max_position = parse_quantity ~name:"max_position" position_json in
  Risk.create ~base_currency ~instruments ~max_order_quantity ~max_position

let parse_execution json =
  let* fields =
    object_fields ~name:"execution"
      ~expected:[ "participation_bps"; "fixed_fee"; "fee_bps" ]
      json
  in
  let* participation_json = field fields "participation_bps" in
  let* participation_bps =
    integer ~name:"participation_bps" participation_json
  in
  let* fixed_json = field fields "fixed_fee" in
  let* fixed_fee = parse_money ~name:"fixed_fee" fixed_json in
  let* fee_json = field fields "fee_bps" in
  let* fee_bps = integer ~name:"fee_bps" fee_json in
  Execution.create ~participation_bps ~fixed_fee ~fee_bps

let parse_side json =
  let* value = string ~name:"side" json in
  match value with
  | "buy" -> Ok Order.Buy
  | "sell" -> Ok Order.Sell
  | _ -> Error "invalid side"

let parse_weight_target json =
  let* fields =
    object_fields ~name:"weight target"
      ~expected:[ "instrument_id"; "weight" ]
      json
  in
  let* instrument_json = field fields "instrument_id" in
  let* instrument_id =
    parse_id Id.Instrument.of_string ~name:"instrument_id" instrument_json
  in
  let* weight_json = field fields "weight" in
  let* weight = parse_weight ~name:"weight" weight_json in
  Ok Strategy.{ instrument_id; weight }

let parse_quantity_target json =
  let* fields =
    object_fields ~name:"quantity target"
      ~expected:[ "instrument_id"; "quantity" ]
      json
  in
  let* instrument_json = field fields "instrument_id" in
  let* instrument_id =
    parse_id Id.Instrument.of_string ~name:"instrument_id" instrument_json
  in
  let* quantity_json = field fields "quantity" in
  let* quantity = parse_quantity ~name:"quantity" quantity_json in
  Ok Strategy.{ instrument_id; quantity }

let parse_portfolio_intent ~name ~parse_target make json =
  let* fields =
    object_fields ~name:(name ^ " intent") ~expected:[ "type"; "targets" ] json
  in
  let* targets_json = field fields "targets" in
  let* targets_json = list ~name:"targets" targets_json in
  let* targets = map_list parse_target targets_json in
  Ok (make targets)

let parse_submit_intent json =
  let* fields =
    object_fields ~name:"submit_order intent"
      ~expected:
        [
          "type";
          "instrument_id";
          "side";
          "quantity";
          "order_kind";
          "limit_price";
        ]
      json
  in
  let* instrument_json = field fields "instrument_id" in
  let* instrument_id =
    parse_id Id.Instrument.of_string ~name:"instrument_id" instrument_json
  in
  let* side_json = field fields "side" in
  let* side = parse_side side_json in
  let* quantity_json = field fields "quantity" in
  let* quantity = parse_quantity ~name:"quantity" quantity_json in
  let* kind_json = field fields "order_kind" in
  let* kind_name = string ~name:"order_kind" kind_json in
  let* limit_json = field fields "limit_price" in
  let* kind =
    match (kind_name, limit_json) with
    | "market", `Null -> Ok Order.Market
    | "limit", value ->
        let* limit = parse_price ~name:"limit_price" value in
        Ok (Order.Limit limit)
    | "market", _ -> Error "market order limit_price must be null"
    | _ -> Error "invalid order_kind"
  in
  let* request =
    Order.request ~instrument_id ~side ~quantity ~kind ~origin:Order.Direct
  in
  Ok (Strategy.Submit_order request)

let parse_cancel_intent json =
  let* fields =
    object_fields ~name:"cancel_order intent" ~expected:[ "type"; "order_id" ]
      json
  in
  let* order_json = field fields "order_id" in
  let* order_id = parse_id Id.Order.of_string ~name:"order_id" order_json in
  Ok (Strategy.Cancel_order order_id)

let parse_metric_intent json =
  let* fields =
    object_fields ~name:"emit_metric intent"
      ~expected:[ "type"; "name"; "value" ]
      json
  in
  let* name_json = field fields "name" in
  let* name = string ~name:"metric name" name_json in
  let* value_json = field fields "value" in
  let* value = string ~name:"metric value" value_json in
  Ok (Strategy.Emit_metric { name; value })

let parse_intent json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "type" fields with
      | Some (`String "target_weights") ->
          parse_portfolio_intent ~name:"target_weights"
            ~parse_target:parse_weight_target
            (fun targets -> Strategy.Target_weights targets)
            json
      | Some (`String "target_quantities") ->
          parse_portfolio_intent ~name:"target_quantities"
            ~parse_target:parse_quantity_target
            (fun targets -> Strategy.Target_quantities targets)
            json
      | Some (`String "submit_order") -> parse_submit_intent json
      | Some (`String "cancel_order") -> parse_cancel_intent json
      | Some (`String "emit_metric") -> parse_metric_intent json
      | Some _ -> Error "unsupported intent type"
      | None -> Error "intent is missing type")
  | _ -> Error "intent must be a JSON object"

let parse_schedule_item json =
  let* fields =
    object_fields ~name:"schedule item"
      ~expected:[ "after_slice_sequence"; "intents" ]
      json
  in
  let* sequence_json = field fields "after_slice_sequence" in
  let* sequence = parse_int64 ~name:"after_slice_sequence" sequence_json in
  let* intents_json = field fields "intents" in
  let* intents_json = list ~name:"intents" intents_json in
  let* intents = map_list parse_intent intents_json in
  Ok (sequence, intents)

let parse_volume = function
  | `Null -> Ok None
  | json -> parse_quantity ~name:"volume" json |> Result.map Option.some

let parse_bar json =
  let* fields =
    object_fields ~name:"bar"
      ~expected:[ "instrument_id"; "open"; "high"; "low"; "close"; "volume" ]
      json
  in
  let* instrument_json = field fields "instrument_id" in
  let* instrument_id =
    parse_id Id.Instrument.of_string ~name:"instrument_id" instrument_json
  in
  let* open_json = field fields "open" in
  let* open_price = parse_price ~name:"open" open_json in
  let* high_json = field fields "high" in
  let* high_price = parse_price ~name:"high" high_json in
  let* low_json = field fields "low" in
  let* low_price = parse_price ~name:"low" low_json in
  let* close_json = field fields "close" in
  let* close_price = parse_price ~name:"close" close_json in
  let* volume_json = field fields "volume" in
  let* volume = parse_volume volume_json in
  Bar.create ~instrument_id ~open_price ~high_price ~low_price ~close_price
    ~volume

let parse_slice json =
  let* fields =
    object_fields ~name:"market slice"
      ~expected:
        [
          "slice_sequence";
          "start_at";
          "end_at";
          "available_at";
          "received_at";
          "bars";
        ]
      json
  in
  let* sequence_json = field fields "slice_sequence" in
  let* slice_sequence = parse_int64 ~name:"slice_sequence" sequence_json in
  let* start_json = field fields "start_at" in
  let* start_at = parse_timestamp ~name:"start_at" start_json in
  let* end_json = field fields "end_at" in
  let* end_at = parse_timestamp ~name:"end_at" end_json in
  let* available_json = field fields "available_at" in
  let* available_at = parse_timestamp ~name:"available_at" available_json in
  let* received_json = field fields "received_at" in
  let* received_at = parse_timestamp ~name:"received_at" received_json in
  let* bars_json = field fields "bars" in
  let* bars_json = list ~name:"bars" bars_json in
  let* bars = map_list parse_bar bars_json in
  Market_slice.create ~slice_sequence ~start_at ~end_at ~available_at
    ~received_at ~bars

let changes_orders = function
  | Strategy.Target_weights _ | Strategy.Target_quantities _
  | Strategy.Submit_order _ | Strategy.Cancel_order _ ->
      true
  | Strategy.Emit_metric _ -> false

let validate_portfolio_target risk catalog = function
  | Strategy.Target_weights targets ->
      let ids =
        List.map
          (fun (target : Strategy.weight_target) -> target.instrument_id)
          targets
      in
      let unique = List.sort_uniq Id.Instrument.compare ids in
      if List.length unique <> List.length ids then
        Error "target_weights must contain each instrument exactly once"
      else if
        not (Id.Instrument.Set.equal catalog (Id.Instrument.Set.of_list ids))
      then Error "target_weights must cover every configured instrument"
      else
        let sum =
          List.fold_left
            (fun result (target : Strategy.weight_target) ->
              let* total = result in
              Scalar.Weight.add total target.Strategy.weight)
            (Ok Scalar.Weight.zero) targets
        in
        let* sum = sum in
        if Scalar.Weight.compare sum Scalar.Weight.one > 0 then
          Error "target weights must sum to at most one"
        else Ok ()
  | Strategy.Target_quantities targets ->
      let ids =
        List.map
          (fun (target : Strategy.quantity_target) -> target.instrument_id)
          targets
      in
      let unique = List.sort_uniq Id.Instrument.compare ids in
      if List.length unique <> List.length ids then
        Error "target_quantities must contain each instrument exactly once"
      else if
        not (Id.Instrument.Set.equal catalog (Id.Instrument.Set.of_list ids))
      then Error "target_quantities must cover every configured instrument"
      else
        List.fold_left
          (fun result (target : Strategy.quantity_target) ->
            let* () = result in
            match Risk.instrument risk target.Strategy.instrument_id with
            | None -> Error "target quantity refers to an unknown instrument"
            | Some instrument ->
                if
                  not
                    (Scalar.Quantity.is_multiple target.quantity
                       ~lot:instrument.Instrument.lot_size)
                then
                  Error "target quantity is not aligned to its instrument lot"
                else if
                  Scalar.Quantity.compare target.quantity
                    (Risk.max_position risk)
                  > 0
                then Error "target quantity exceeds the maximum position"
                else Ok ())
          (Ok ()) targets
  | Strategy.Submit_order request -> (
      if not (Id.Instrument.Set.mem request.Order.instrument_id catalog) then
        Error "order refers to an unknown instrument"
      else if
        Scalar.Quantity.compare request.quantity (Risk.max_order_quantity risk)
        > 0
      then Error "order exceeds the maximum order quantity"
      else
        match Risk.instrument risk request.instrument_id with
        | None -> Error "order refers to an unknown instrument"
        | Some instrument -> (
            if
              not
                (Scalar.Quantity.is_multiple request.quantity
                   ~lot:instrument.Instrument.lot_size)
            then
              Error "order quantity is not aligned to the instrument lot size"
            else
              match request.kind with
              | Order.Market -> Ok ()
              | Order.Limit price ->
                  if Scalar.Price.is_multiple price ~tick:instrument.tick_size
                  then Ok ()
                  else
                    Error
                      "limit price is not aligned to the instrument tick size"))
  | Strategy.Cancel_order _ | Strategy.Emit_metric _ -> Ok ()

let validate_slices catalog slices =
  let rec validate previous_sequence previous_end previous_received = function
    | [] -> Ok ()
    | market_slice :: remaining ->
        let ids =
          List.map
            (fun bar -> bar.Bar.instrument_id)
            market_slice.Market_slice.bars
          |> Id.Instrument.Set.of_list
        in
        if not (Id.Instrument.Set.equal catalog ids) then
          Error "each market slice must contain every configured instrument"
        else if
          Option.exists
            (fun sequence ->
              Int64.compare market_slice.slice_sequence sequence <= 0)
            previous_sequence
        then Error "market slice sequence must increase"
        else if
          Option.exists
            (fun end_at -> Ptime.compare market_slice.end_at end_at <= 0)
            previous_end
        then Error "market slice end must increase"
        else if
          Option.exists
            (fun received_at ->
              Ptime.compare market_slice.received_at received_at < 0)
            previous_received
        then Error "market slice receipt time must not move backward"
        else
          validate (Some market_slice.slice_sequence) (Some market_slice.end_at)
            (Some market_slice.received_at) remaining
  in
  validate None None None slices

let validate_schedule risk catalog schedule slices =
  let slice_sequences =
    List.fold_left
      (fun sequences market_slice ->
        Int64_set.add market_slice.Market_slice.slice_sequence sequences)
      Int64_set.empty slices
  in
  let slice_at sequence =
    List.find_opt
      (fun market_slice ->
        Int64.equal market_slice.Market_slice.slice_sequence sequence)
      slices
  in
  let next_slice sequence =
    List.find_opt
      (fun market_slice ->
        Int64.compare market_slice.Market_slice.slice_sequence sequence > 0)
      slices
  in
  let validate_item sequence intents =
    if Int64.compare sequence 0L <= 0 then
      Error "scheduled slice sequence must be positive"
    else if not (Int64_set.mem sequence slice_sequences) then
      Error
        (Printf.sprintf
           "scheduled intents refer to missing market slice sequence %Ld"
           sequence)
    else
      let* () =
        List.fold_left
          (fun result intent ->
            let* () = result in
            validate_portfolio_target risk catalog intent)
          (Ok ()) intents
      in
      match (slice_at sequence, next_slice sequence) with
      | Some anchor, Some next
        when List.exists changes_orders intents
             && Ptime.compare anchor.received_at next.start_at > 0 ->
          Error
            (Printf.sprintf
               "scheduled order intent after slice %Ld is received after the \
                next executable market slice starts"
               sequence)
      | _ -> Ok ()
  in
  let rec validate previous = function
    | [] -> Ok ()
    | (sequence, intents) :: remaining ->
        if
          Option.exists
            (fun prior -> Int64.compare sequence prior <= 0)
            previous
        then Error "schedule sequences must increase"
        else
          let* () = validate_item sequence intents in
          validate (Some sequence) remaining
  in
  validate None schedule

let of_yojson json =
  let* fields =
    object_fields ~name:"scenario"
      ~expected:
        [
          "metadata";
          "run_id";
          "base_currency";
          "initial_cash";
          "instruments";
          "risk";
          "execution";
          "max_internal_events";
          "schedule";
          "slices";
        ]
      json
  in
  let* metadata = field fields "metadata" in
  let* () =
    match metadata with
    | `Assoc _ -> validate_metadata metadata
    | _ -> Error "metadata must be a JSON object"
  in
  let* run_json = field fields "run_id" in
  let* run_id = parse_id Id.Run.of_string ~name:"run_id" run_json in
  let* currency_json = field fields "base_currency" in
  let* base_currency = string ~name:"base_currency" currency_json in
  let* cash_json = field fields "initial_cash" in
  let* initial_cash = parse_money ~name:"initial_cash" cash_json in
  if Scalar.Money.compare initial_cash Scalar.Money.zero < 0 then
    Error "initial_cash must be nonnegative"
  else
    let* instruments_json = field fields "instruments" in
    let* instruments_json = list ~name:"instruments" instruments_json in
    let* instruments = map_list parse_instrument instruments_json in
    if instruments = [] then
      Error "scenario must define at least one instrument"
    else
      let catalog =
        List.map (fun instrument -> instrument.Instrument.id) instruments
        |> Id.Instrument.Set.of_list
      in
      let* risk_json = field fields "risk" in
      let* risk = parse_risk base_currency instruments risk_json in
      let* execution_json = field fields "execution" in
      let* execution = parse_execution execution_json in
      let* maximum_json = field fields "max_internal_events" in
      let* max_internal_events =
        integer ~name:"max_internal_events" maximum_json
      in
      if max_internal_events <= 0 then
        Error "max_internal_events must be positive"
      else
        let* schedule_json = field fields "schedule" in
        let* schedule_json = list ~name:"schedule" schedule_json in
        let* schedule = map_list parse_schedule_item schedule_json in
        let* slices_json = field fields "slices" in
        let* slices_json = list ~name:"slices" slices_json in
        let* slices = map_list parse_slice slices_json in
        let* () = validate_slices catalog slices in
        let* () = validate_schedule risk catalog schedule slices in
        Ok
          {
            metadata;
            run_id;
            base_currency;
            initial_cash;
            instruments;
            risk;
            execution;
            max_internal_events;
            schedule;
            slices;
          }

let of_string document =
  try Yojson.Safe.from_string document |> of_yojson
  with Yojson.Json_error message -> Error ("invalid scenario JSON: " ^ message)

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all |> of_string
  with Sys_error message -> Error ("could not read scenario: " ^ message)
