type t = {
  contract_version : string;
  metadata : Yojson.Safe.t;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : (string * Scalar.Money.t) list;
  instruments : Instrument.t list;
  venue_calendars : Venue_calendar.t list;
  risk : Risk.t;
  execution_model : Execution_model.t;
  execution : Execution.t;
  max_internal_events : int;
  schedule : (int64 * Strategy.intent list) list;
  slices : Market_slice.t list;
}

type stream_header = {
  contract_version : string;
  metadata : Yojson.Safe.t;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : (string * Scalar.Money.t) list;
  instruments : Instrument.t list;
  venue_calendars : Venue_calendar.t list;
  risk : Risk.t;
  execution_model : Execution_model.t;
  execution : Execution.t;
  max_internal_events : int;
}

type stream_item = {
  market_slice : Market_slice.t;
  intents : Strategy.intent list;
  action_ids : Id.Corporate_action.Set.t;
}

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let resource_limit ~json_path ~name ~observed ~allowed =
  Diagnostic.make ~code:Diagnostic.Resource_limit ~phase:Diagnostic.Validation
    ~json_path
    (Printf.sprintf "%s count is %d; limit is %d" name observed allowed)

let check_list_limit fields field_name ~json_path ~name allowed =
  match List.assoc_opt field_name fields with
  | Some (`List values) when List.length values > allowed ->
      Error
        (resource_limit ~json_path ~name ~observed:(List.length values) ~allowed)
  | _ -> Ok ()

let check_internal_event_limit fields ~json_path =
  match List.assoc_opt "max_internal_events" fields with
  | Some (`Int observed) when observed > Resource_limits.internal_events ->
      Error
        (resource_limit ~json_path ~name:"internal event" ~observed
           ~allowed:Resource_limits.internal_events)
  | _ -> Ok ()

let check_batch_limits = function
  | `Assoc fields -> (
      let* () =
        check_list_limit fields "instruments" ~json_path:"$.instruments"
          ~name:"catalog instrument" Resource_limits.catalog_instruments
      in
      let* () =
        check_internal_event_limit fields ~json_path:"$.max_internal_events"
      in
      match List.assoc_opt "schedule" fields with
      | Some (`List items) ->
          let rec check index = function
            | [] -> Ok ()
            | `Assoc item_fields :: remaining ->
                let* () =
                  check_list_limit item_fields "intents"
                    ~json_path:(Printf.sprintf "$.schedule[%d].intents" index)
                    ~name:"intent" Resource_limits.intents_per_batch
                in
                check (index + 1) remaining
            | _ :: remaining -> check (index + 1) remaining
          in
          check 0 items
      | _ -> Ok ())
  | _ -> Ok ()

let check_stream_header_limits = function
  | `Assoc fields ->
      let* () =
        check_list_limit fields "instruments" ~json_path:"$.instruments"
          ~name:"catalog instrument" Resource_limits.catalog_instruments
      in
      check_internal_event_limit fields ~json_path:"$.max_internal_events"
  | _ -> Ok ()

let check_stream_item_limits = function
  | `Assoc fields ->
      check_list_limit fields "intents" ~json_path:"$.intents" ~name:"intent"
        Resource_limits.intents_per_batch
  | _ -> Ok ()

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

let at json_path result =
  Result.map_error
    (fun message -> Scenario_shape.error ~json_path message)
    result

let map_list_at root parse values =
  let step result (index, value) =
    let* values = result in
    let* value = parse value |> at (Printf.sprintf "%s[%d]" root index) in
    Ok (value :: values)
  in
  values
  |> List.mapi (fun index value -> (index, value))
  |> List.fold_left step (Ok [])
  |> Result.map List.rev

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
  Scalar.Quantity.of_decimal_string value

let parse_price ~name json =
  let* value = string ~name json in
  Scalar.Price.of_decimal_string value

let parse_money ~name json =
  let* value = string ~name json in
  Scalar.Money.of_decimal_string value

let parse_weight ~name json =
  let* value = string ~name json in
  Scalar.Weight.of_decimal_string value

let parse_ratio ~name json =
  let* value = string ~name json in
  Scalar.Ratio.of_decimal_string value

let parse_cash_balance json =
  let* fields =
    object_fields ~name:"initial cash balance"
      ~expected:[ "currency"; "amount" ] json
  in
  let* currency_json = field fields "currency" in
  let* currency = string ~name:"cash currency" currency_json in
  let* amount_json = field fields "amount" in
  let* amount = parse_money ~name:"initial cash amount" amount_json in
  Ok (currency, amount)

let parse_timestamp ~name json =
  let* value = string ~name json in
  Codec.ptime_of_string value

let parse_venue_phase json =
  let* fields =
    object_fields ~name:"venue phase"
      ~expected:[ "phase"; "opens_at"; "closes_at" ]
      json
  in
  let* kind_json = field fields "phase" in
  let* kind_name = string ~name:"venue phase" kind_json in
  let* kind = Venue_calendar.phase_kind_of_string kind_name in
  let* opens_json = field fields "opens_at" in
  let* opens_at = parse_timestamp ~name:"venue phase opens_at" opens_json in
  let* closes_json = field fields "closes_at" in
  let* closes_at = parse_timestamp ~name:"venue phase closes_at" closes_json in
  Venue_calendar.create_phase ~kind ~opens_at ~closes_at

let parse_venue_session json =
  let* fields =
    object_fields ~name:"venue session policy"
      ~expected:[ "session_date"; "policy"; "phases" ]
      json
  in
  let* date_json = field fields "session_date" in
  let* session_date = string ~name:"session_date" date_json in
  let* policy_json = field fields "policy" in
  let* policy_name = string ~name:"session policy" policy_json in
  let* kind = Venue_calendar.session_kind_of_string policy_name in
  let* phases_json = field fields "phases" in
  let* phases_json = list ~name:"venue phases" phases_json in
  let* phases = map_list parse_venue_phase phases_json in
  Venue_calendar.create_session ~session_date ~kind ~phases

let parse_venue_calendar json =
  let* fields =
    object_fields ~name:"venue calendar"
      ~expected:
        [
          "calendar_id";
          "calendar_version";
          "venue_id";
          "instrument_ids";
          "sessions";
        ]
      json
  in
  let* id_json = field fields "calendar_id" in
  let* id = parse_id Id.Venue_calendar.of_string ~name:"calendar_id" id_json in
  let* version_json = field fields "calendar_version" in
  let* version = string ~name:"calendar_version" version_json in
  let* venue_json = field fields "venue_id" in
  let* venue_id = parse_id Id.Venue.of_string ~name:"venue_id" venue_json in
  let* instruments_json = field fields "instrument_ids" in
  let* instruments_json =
    list ~name:"calendar instrument_ids" instruments_json
  in
  let* instrument_ids =
    map_list
      (parse_id Id.Instrument.of_string ~name:"calendar instrument_id")
      instruments_json
  in
  let* sessions_json = field fields "sessions" in
  let* sessions_json = list ~name:"venue sessions" sessions_json in
  let* sessions = map_list parse_venue_session sessions_json in
  Venue_calendar.create ~id ~version ~venue_id ~instrument_ids ~sessions

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
      ~expected:
        [
          "max_order_quantity";
          "max_long_position";
          "max_short_position";
          "max_gross_exposure";
          "max_leverage";
          "initial_margin_bps";
          "maintenance_margin_bps";
          "short_borrow_bps";
        ]
      json
  in
  let* order_json = field fields "max_order_quantity" in
  let* max_order_quantity =
    parse_quantity ~name:"max_order_quantity" order_json
  in
  let* long_json = field fields "max_long_position" in
  let* max_long_position = parse_quantity ~name:"max_long_position" long_json in
  let* short_json = field fields "max_short_position" in
  let* max_short_position =
    parse_quantity ~name:"max_short_position" short_json
  in
  let* gross_json = field fields "max_gross_exposure" in
  let* max_gross_exposure = parse_money ~name:"max_gross_exposure" gross_json in
  let* leverage_json = field fields "max_leverage" in
  let* max_leverage = parse_ratio ~name:"max_leverage" leverage_json in
  let* initial_json = field fields "initial_margin_bps" in
  let* initial_margin_bps = integer ~name:"initial_margin_bps" initial_json in
  let* maintenance_json = field fields "maintenance_margin_bps" in
  let* maintenance_margin_bps =
    integer ~name:"maintenance_margin_bps" maintenance_json
  in
  let* borrow_json = field fields "short_borrow_bps" in
  let* short_borrow_bps = integer ~name:"short_borrow_bps" borrow_json in
  Risk.create ~base_currency ~instruments ~max_order_quantity ~max_long_position
    ~max_short_position ~max_gross_exposure ~max_leverage ~initial_margin_bps
    ~maintenance_margin_bps ~short_borrow_bps

let parse_execution_values fields =
  let* participation_json = field fields "participation_bps" in
  let* participation_bps =
    integer ~name:"participation_bps" participation_json
  in
  let* fixed_json = field fields "fixed_fee" in
  let* fixed_fee = parse_money ~name:"fixed_fee" fixed_json in
  let* fee_json = field fields "fee_bps" in
  let* fee_bps = integer ~name:"fee_bps" fee_json in
  Execution.create ~participation_bps ~fixed_fee ~fee_bps

let parse_legacy_execution ~contract_version json =
  let* fields =
    object_fields ~name:"execution"
      ~expected:[ "model"; "participation_bps"; "fixed_fee"; "fee_bps" ]
      json
  in
  let* model_json = field fields "model" in
  let* model_name = string ~name:"execution model" model_json in
  let* execution_model = Execution_model.find model_name in
  let* () =
    if Execution_model.supports_contract execution_model contract_version then
      Ok ()
    else
      Error
        (Printf.sprintf
           "execution model %S does not support scenario contract %S" model_name
           contract_version)
  in
  let* execution = parse_execution_values fields in
  Ok (execution_model, execution)

let parse_versioned_execution ~contract_version json =
  let* fields =
    object_fields ~name:"execution" ~expected:[ "model"; "configuration" ] json
  in
  let* model_json = field fields "model" in
  let* model_name = string ~name:"execution model" model_json in
  let* execution_model = Execution_model.find model_name in
  let* () =
    if Execution_model.supports_contract execution_model contract_version then
      Ok ()
    else
      Error
        (Printf.sprintf
           "execution model %S does not support scenario contract %S" model_name
           contract_version)
  in
  let* configuration_json = field fields "configuration" in
  let expected =
    (Execution_model.configuration_contract execution_model).required_fields
  in
  let* configuration =
    object_fields
      ~name:(model_name ^ " execution configuration")
      ~expected configuration_json
  in
  let* version_json = field configuration "version" in
  let* version = string ~name:"execution configuration version" version_json in
  if not (Execution_model.supports_configuration execution_model version) then
    Error
      (Printf.sprintf
         "unsupported execution configuration version %S for model %S" version
         model_name)
  else
    let* execution = parse_execution_values configuration in
    Ok (execution_model, execution)

let parse_execution ~contract_version json =
  if String.equal contract_version "5" then
    parse_versioned_execution ~contract_version json
  else parse_legacy_execution ~contract_version json

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

let intent_of_yojson json =
  parse_intent json
  |> Result.map_error (fun message ->
      Diagnostic.make ~code:Diagnostic.Scenario_invalid
        ~phase:Diagnostic.Validation ~json_path:"$" message)

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
  if List.length intents_json > Resource_limits.intents_per_batch then
    Error
      (Printf.sprintf "intent count is %d; limit is %d"
         (List.length intents_json) Resource_limits.intents_per_batch)
  else
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

let parse_fx_mark json =
  let* fields =
    object_fields ~name:"FX rate" ~expected:[ "currency"; "rate" ] json
  in
  let* currency_json = field fields "currency" in
  let* currency = string ~name:"currency" currency_json in
  let* rate_json = field fields "rate" in
  let* rate = parse_price ~name:"FX rate" rate_json in
  Market_slice.fx_mark ~currency ~rate

let parse_corporate_action json =
  let* fields =
    match json with
    | `Assoc fields -> Ok fields
    | _ -> Error "corporate action must be a JSON object"
  in
  let* type_json = field fields "type" in
  let* action_type = string ~name:"corporate action type" type_json in
  let* id_json = field fields "action_id" in
  let* id = parse_id Id.Corporate_action.of_string ~name:"action_id" id_json in
  let* instrument_json = field fields "instrument_id" in
  let* instrument_id =
    parse_id Id.Instrument.of_string ~name:"instrument_id" instrument_json
  in
  match action_type with
  | "split" ->
      let* () =
        object_fields ~name:"split corporate action"
          ~expected:
            [ "type"; "action_id"; "instrument_id"; "numerator"; "denominator" ]
          json
        |> Result.map (fun _ -> ())
      in
      let* numerator_json = field fields "numerator" in
      let* numerator = parse_int64 ~name:"split numerator" numerator_json in
      let* denominator_json = field fields "denominator" in
      let* denominator =
        parse_int64 ~name:"split denominator" denominator_json
      in
      Corporate_action.split ~id ~instrument_id ~numerator ~denominator
  | "cash_dividend" ->
      let* () =
        object_fields ~name:"cash dividend corporate action"
          ~expected:[ "type"; "action_id"; "instrument_id"; "amount_per_unit" ]
          json
        |> Result.map (fun _ -> ())
      in
      let* amount_json = field fields "amount_per_unit" in
      let* amount_per_unit = parse_money ~name:"amount_per_unit" amount_json in
      Corporate_action.cash_dividend ~id ~instrument_id ~amount_per_unit
  | _ -> Error "unsupported corporate action type"

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
          "fx_rates";
          "corporate_actions";
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
  let* fx_json = field fields "fx_rates" in
  let* fx_json = list ~name:"fx_rates" fx_json in
  let* fx_rates = map_list parse_fx_mark fx_json in
  let* actions_json = field fields "corporate_actions" in
  let* actions_json = list ~name:"corporate_actions" actions_json in
  let* corporate_actions = map_list parse_corporate_action actions_json in
  Market_slice.create ~slice_sequence ~start_at ~end_at ~available_at
    ~received_at ~bars ~fx_rates ~corporate_actions

let child root field = root ^ "." ^ field

let construct_header ~root ~contract_path ~contract_version
    (shape : Scenario_shape.common) =
  if not (Contract.is_supported contract_version) then
    Error
      (Scenario_shape.error ~json_path:contract_path
         (Printf.sprintf
            "unsupported scenario contract_version %S (expected one of %s)"
            contract_version
            (String.concat ", " Contract.supported_versions)))
  else
    let* () =
      match shape.metadata with
      | `Assoc _ ->
          validate_metadata shape.metadata |> at (child root "metadata")
      | _ ->
          Error
            (Scenario_shape.error ~json_path:(child root "metadata")
               "metadata must be a JSON object")
    in
    let metadata = shape.metadata in
    let* run_id =
      parse_id Id.Run.of_string ~name:"run_id" shape.run_id
      |> at (child root "run_id")
    in
    let* base_currency =
      string ~name:"base_currency" shape.base_currency
      |> at (child root "base_currency")
    in
    let* initial_cash_json =
      list ~name:"initial_cash" shape.initial_cash
      |> at (child root "initial_cash")
    in
    let* initial_cash =
      map_list_at
        (child root "initial_cash")
        parse_cash_balance initial_cash_json
    in
    let* instruments_json =
      list ~name:"instruments" shape.instruments
      |> at (child root "instruments")
    in
    let* instruments =
      map_list_at (child root "instruments") parse_instrument instruments_json
    in
    let* venue_calendars =
      match shape.venue_calendars with
      | None -> Ok []
      | Some calendars_json ->
          let* calendars_json =
            list ~name:"venue_calendars" calendars_json
            |> at (child root "venue_calendars")
          in
          map_list_at
            (child root "venue_calendars")
            parse_venue_calendar calendars_json
    in
    let* max_internal_events =
      integer ~name:"max_internal_events" shape.max_internal_events
      |> at (child root "max_internal_events")
    in
    let* currencies, catalog =
      Scenario_validation.header ~root ~contract_version ~base_currency
        ~initial_cash ~instruments ~venue_calendars ~max_internal_events
    in
    let* risk =
      parse_risk base_currency instruments shape.risk |> at (child root "risk")
    in
    let* execution_model, execution =
      parse_execution ~contract_version shape.execution
      |> at (child root "execution")
    in
    let header : stream_header =
      {
        contract_version;
        metadata;
        run_id;
        base_currency;
        initial_cash;
        instruments;
        venue_calendars;
        risk;
        execution_model;
        execution;
        max_internal_events;
      }
    in
    Ok (header, currencies, catalog)

let construct_batch (shape : Scenario_shape.batch) =
  let root = "$" in
  let contract_path = "$.contract_version" in
  let* contract_version =
    string ~name:"contract_version" shape.contract_version |> at contract_path
  in
  let* header, currencies, catalog =
    construct_header ~root ~contract_path ~contract_version shape.common
  in
  let* schedule_json =
    list ~name:"schedule" shape.schedule |> at "$.schedule"
  in
  let* schedule = map_list_at "$.schedule" parse_schedule_item schedule_json in
  let* slices_json = list ~name:"slices" shape.slices |> at "$.slices" in
  let* slices = map_list_at "$.slices" parse_slice slices_json in
  let* () =
    Scenario_validation.batch ~root ~base_currency:header.base_currency
      ~currencies ~instruments:header.instruments ~risk:header.risk ~catalog
      ~schedule ~slices
  in
  Ok
    {
      contract_version = header.contract_version;
      metadata = header.metadata;
      run_id = header.run_id;
      base_currency = header.base_currency;
      initial_cash = header.initial_cash;
      instruments = header.instruments;
      venue_calendars = header.venue_calendars;
      risk = header.risk;
      execution_model = header.execution_model;
      execution = header.execution;
      max_internal_events = header.max_internal_events;
      schedule;
      slices;
    }

let diagnostic code (error : Scenario_shape.error) =
  Diagnostic.make ~code ~phase:Diagnostic.Validation ~json_path:error.json_path
    error.message

let of_yojson json =
  let supplied_version =
    match json with
    | `Assoc fields -> List.assoc_opt "contract_version" fields
    | _ -> None
  in
  let* () =
    match supplied_version with
    | Some (`String supplied) when not (Contract.is_supported supplied) ->
        Error
          (Diagnostic.make ~code:Diagnostic.Scenario_unsupported_contract
             ~phase:Diagnostic.Validation ~json_path:"$.contract_version"
             (Printf.sprintf
                "unsupported scenario contract_version %S (expected one of %s)"
                supplied
                (String.concat ", " Contract.supported_versions)))
    | _ -> Ok ()
  in
  let code = Diagnostic.Scenario_invalid in
  let* () = check_batch_limits json in
  let* shape =
    Scenario_shape.batch json |> Result.map_error (diagnostic code)
  in
  construct_batch shape |> Result.map_error (diagnostic code)

let of_string document =
  try Yojson.Safe.from_string document |> of_yojson
  with Yojson.Json_error message as exception_ ->
    Error
      (Diagnostic.of_exception ~code:Diagnostic.Scenario_invalid_json
         ~phase:Diagnostic.Input ~json_path:"$"
         ~message:("invalid scenario JSON: " ^ message)
         exception_)

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all |> of_string
  with Sys_error message as exception_ ->
    Error
      (Diagnostic.of_exception ~code:Diagnostic.Input_io ~phase:Diagnostic.Input
         ~message:("could not read scenario: " ^ message)
         exception_)

let stream_header_of_yojson ~contract_version json =
  let code =
    if Contract.is_supported contract_version then
      Diagnostic.Scenario_stream_invalid
    else Diagnostic.Scenario_unsupported_contract
  in
  let* () = check_stream_header_limits json in
  let* shape =
    Scenario_shape.stream_header ~contract_version json
    |> Result.map_error (diagnostic code)
  in
  construct_header ~root:"$.payload" ~contract_path:"$.contract_version"
    ~contract_version shape
  |> Result.map (fun (header, _, _) -> header)
  |> Result.map_error (diagnostic code)

let stream_item_of_yojson header ~previous json =
  let* () = check_stream_item_limits json in
  let code = Diagnostic.Scenario_stream_invalid in
  let* shape =
    Scenario_shape.stream_item json |> Result.map_error (diagnostic code)
  in
  let* market_slice =
    parse_slice shape.market_slice
    |> at "$.payload.market_slice"
    |> Result.map_error (diagnostic code)
  in
  let* intents_json =
    list ~name:"intents" shape.intents
    |> at "$.payload.intents"
    |> Result.map_error (diagnostic code)
  in
  let* intents =
    map_list_at "$.payload.intents" parse_intent intents_json
    |> Result.map_error (diagnostic code)
  in
  let previous_slice, previous_intents, prior_action_ids =
    match previous with
    | None -> (None, [], Id.Corporate_action.Set.empty)
    | Some item -> (Some item.market_slice, item.intents, item.action_ids)
  in
  let* action_ids =
    Scenario_validation.stream_item ~root:"$.payload"
      ~base_currency:header.base_currency ~instruments:header.instruments
      ~risk:header.risk ~previous_slice ~previous_intents ~prior_action_ids
      ~market_slice ~intents
    |> Result.map_error (diagnostic code)
  in
  Ok { market_slice; intents; action_ids }
