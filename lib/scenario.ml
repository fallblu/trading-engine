type t = {
  contract_version : string;
  metadata : Yojson.Safe.t;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : (string * Scalar.Money.t) list;
  initial_portfolio : Initial_portfolio.t option;
  instruments : Instrument.t list;
  venue_calendars : Venue_calendar.t list;
  risk : Risk.t;
  execution_model : Execution_model.t;
  execution : Execution.t;
  financing : Financing.policy option;
  settlement : Settlement.policy option;
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
  initial_portfolio : Initial_portfolio.t option;
  instruments : Instrument.t list;
  venue_calendars : Venue_calendar.t list;
  risk : Risk.t;
  execution_model : Execution_model.t;
  execution : Execution.t;
  financing : Financing.policy option;
  settlement : Settlement.policy option;
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

let parse_legacy_risk base_currency instruments json =
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

let parse_nullable parse ~name = function
  | `Null -> Ok None
  | json -> parse ~name json |> Result.map Option.some

let parse_group_kind = function
  | "issuer" -> Ok Risk.Issuer
  | "sector" -> Ok Risk.Sector
  | "currency" -> Ok Risk.Currency
  | "country" -> Ok Risk.Country
  | "asset_class" -> Ok Risk.Asset_class
  | "custom" -> Ok Risk.Custom
  | _ -> Error "group_type is unsupported"

let parse_instrument_policy instrument_map json =
  let* fields =
    object_fields ~name:"instrument risk policy"
      ~expected:
        [
          "instrument_id";
          "max_order_quantity";
          "max_long_position";
          "max_short_position";
          "max_notional_exposure";
          "initial_margin_bps";
          "maintenance_margin_bps";
          "shorting_allowed";
        ]
      json
  in
  let* id_json = field fields "instrument_id" in
  let* instrument_id =
    parse_id Id.Instrument.of_string ~name:"instrument_id" id_json
  in
  let* instrument =
    match Id.Instrument.Map.find_opt instrument_id instrument_map with
    | Some instrument -> Ok instrument
    | None -> Error "instrument policy refers to an unknown instrument"
  in
  let* max_order_quantity =
    field fields "max_order_quantity" |> fun result ->
    Result.bind result (parse_quantity ~name:"max_order_quantity")
  in
  let* max_long_position =
    field fields "max_long_position" |> fun result ->
    Result.bind result (parse_quantity ~name:"max_long_position")
  in
  let* max_short_position =
    field fields "max_short_position" |> fun result ->
    Result.bind result (parse_quantity ~name:"max_short_position")
  in
  let* max_notional_exposure =
    field fields "max_notional_exposure" |> fun result ->
    Result.bind result (parse_money ~name:"max_notional_exposure")
  in
  let* initial_margin_bps =
    field fields "initial_margin_bps" |> fun result ->
    Result.bind result (integer ~name:"initial_margin_bps")
  in
  let* maintenance_margin_bps =
    field fields "maintenance_margin_bps" |> fun result ->
    Result.bind result (integer ~name:"maintenance_margin_bps")
  in
  let* shorting_allowed =
    match List.assoc "shorting_allowed" fields with
    | `Bool value -> Ok value
    | _ -> Error "shorting_allowed must be a boolean"
  in
  Risk.create_instrument_policy ~instrument ~max_order_quantity
    ~max_long_position ~max_short_position
    ~max_notional_exposure:(Some max_notional_exposure) ~initial_margin_bps
    ~maintenance_margin_bps ~shorting_allowed

let parse_group json =
  let* fields =
    object_fields ~name:"risk group"
      ~expected:
        [
          "group_id"; "group_version"; "group_type"; "instrument_ids"; "limits";
        ]
      json
  in
  let* group_id =
    field fields "group_id" |> fun result ->
    Result.bind result (parse_id Id.Risk_group.of_string ~name:"group_id")
  in
  let* group_version =
    field fields "group_version" |> fun result ->
    Result.bind result (string ~name:"group_version")
  in
  let* () =
    if String.equal group_version "1" then Ok ()
    else Error "group_version must be 1"
  in
  let* group_kind =
    field fields "group_type" |> fun result ->
    Result.bind result (string ~name:"group_type") |> fun result ->
    Result.bind result parse_group_kind
  in
  let* instrument_ids_json =
    field fields "instrument_ids" |> fun result ->
    Result.bind result (list ~name:"instrument_ids")
  in
  let* instrument_ids =
    map_list
      (parse_id Id.Instrument.of_string ~name:"instrument_id")
      instrument_ids_json
  in
  let* limits_json = field fields "limits" in
  let* limits_fields =
    object_fields ~name:"risk group limits"
      ~expected:
        [
          "max_gross_exposure";
          "max_long_exposure";
          "max_short_exposure";
          "max_absolute_net_exposure";
          "max_concentration";
        ]
      limits_json
  in
  let money_limit name =
    field limits_fields name |> fun result ->
    Result.bind result (parse_nullable parse_money ~name)
  in
  let* max_gross_exposure = money_limit "max_gross_exposure" in
  let* max_long_exposure = money_limit "max_long_exposure" in
  let* max_short_exposure = money_limit "max_short_exposure" in
  let* max_absolute_net_exposure = money_limit "max_absolute_net_exposure" in
  let* max_concentration =
    field limits_fields "max_concentration" |> fun result ->
    Result.bind result (parse_nullable parse_ratio ~name:"max_concentration")
  in
  let* limits =
    Risk.create_group_limits ~max_gross_exposure ~max_long_exposure
      ~max_short_exposure ~max_absolute_net_exposure ~max_concentration
  in
  Risk.create_group ~group_id ~group_kind ~instrument_ids ~limits

let parse_v7_risk base_currency instruments json =
  let* fields =
    object_fields ~name:"risk"
      ~expected:
        [
          "max_gross_exposure";
          "max_leverage";
          "short_borrow_bps";
          "instrument_policies";
          "groups";
        ]
      json
  in
  let instrument_map =
    List.fold_left
      (fun map instrument ->
        Id.Instrument.Map.add instrument.Instrument.id instrument map)
      Id.Instrument.Map.empty instruments
  in
  let* policies_json =
    field fields "instrument_policies" |> fun result ->
    Result.bind result (list ~name:"instrument_policies")
  in
  let* instrument_policies =
    map_list (parse_instrument_policy instrument_map) policies_json
  in
  let* groups_json =
    field fields "groups" |> fun result ->
    Result.bind result (list ~name:"groups")
  in
  let* groups = map_list parse_group groups_json in
  let* max_gross_exposure =
    field fields "max_gross_exposure" |> fun result ->
    Result.bind result (parse_money ~name:"max_gross_exposure")
  in
  let* max_leverage =
    field fields "max_leverage" |> fun result ->
    Result.bind result (parse_ratio ~name:"max_leverage")
  in
  let* short_borrow_bps =
    field fields "short_borrow_bps" |> fun result ->
    Result.bind result (integer ~name:"short_borrow_bps")
  in
  Risk.create_v7 ~base_currency ~instruments ~instrument_policies ~groups
    ~max_gross_exposure ~max_leverage ~short_borrow_bps

let parse_risk ~contract_version base_currency instruments json =
  if
    List.mem contract_version
      [ "16"; "15"; "14"; "13"; "12"; "11"; "10"; "9"; "8"; "7" ]
  then parse_v7_risk base_currency instruments json
  else parse_legacy_risk base_currency instruments json

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

let parse_fee_component json =
  let* fields =
    object_fields ~name:"fee component"
      ~expected:
        [ "name"; "currency"; "kind"; "value"; "rounding"; "applies_to" ]
      json
  in
  let* name_json = field fields "name" in
  let* name = string ~name:"fee component name" name_json in
  let* currency_json = field fields "currency" in
  let* currency = string ~name:"fee component currency" currency_json in
  let* kind_json = field fields "kind" in
  let* kind = string ~name:"fee component kind" kind_json in
  let* value = field fields "value" in
  let* basis =
    match kind with
    | "fixed" ->
        Result.map
          (fun value -> Fee_schedule.Fixed value)
          (parse_money ~name:"fixed fee value" value)
    | "notional_bps" ->
        Result.map
          (fun value -> Fee_schedule.Notional_bps value)
          (integer ~name:"notional fee basis points" value)
    | "per_unit" ->
        Result.map
          (fun value -> Fee_schedule.Per_unit value)
          (parse_money ~name:"per-unit fee value" value)
    | value -> Error (Printf.sprintf "unsupported fee component kind %S" value)
  in
  let* rounding_json = field fields "rounding" in
  let* rounding_name = string ~name:"fee rounding" rounding_json in
  let* rounding = Fee_schedule.rounding_of_string rounding_name in
  let* applicability_json = field fields "applies_to" in
  let* applicability_name =
    string ~name:"fee applicability" applicability_json
  in
  let* applicability =
    Fee_schedule.applicability_of_string applicability_name
  in
  Fee_schedule.create_component ~name ~currency ~basis ~rounding ~applicability

let parse_optional_money ~name = function
  | `Null -> Ok None
  | json -> Result.map Option.some (parse_money ~name json)

let parse_fee_schedule instrument_ids json =
  let* fields =
    object_fields ~name:"fee schedule"
      ~expected:
        [
          "schedule_id";
          "instrument_id";
          "settlement_currency";
          "minimum";
          "maximum";
          "components";
        ]
      json
  in
  let* schedule_id_json = field fields "schedule_id" in
  let* schedule_id = string ~name:"fee schedule ID" schedule_id_json in
  let* instrument_id_json = field fields "instrument_id" in
  let* instrument_id =
    parse_id Id.Instrument.of_string ~name:"fee schedule instrument_id"
      instrument_id_json
  in
  let* () =
    if Id.Instrument.Set.mem instrument_id instrument_ids then Ok ()
    else Error "fee schedule refers to an unknown instrument"
  in
  let* settlement_currency_json = field fields "settlement_currency" in
  let* settlement_currency =
    string ~name:"fee settlement currency" settlement_currency_json
  in
  let* minimum_json = field fields "minimum" in
  let* minimum = parse_optional_money ~name:"fee minimum" minimum_json in
  let* maximum_json = field fields "maximum" in
  let* maximum = parse_optional_money ~name:"fee maximum" maximum_json in
  let* components_value = field fields "components" in
  let* components_json = list ~name:"fee components" components_value in
  let* components = map_list parse_fee_component components_json in
  Fee_schedule.create ~schedule_id ~instrument_id ~settlement_currency ~minimum
    ~maximum ~components

let parse_execution_common instruments fields =
  let* participation_json = field fields "participation_bps" in
  let* participation_bps =
    integer ~name:"participation_bps" participation_json
  in
  let instrument_ids =
    List.fold_left
      (fun ids instrument -> Id.Instrument.Set.add instrument.Instrument.id ids)
      Id.Instrument.Set.empty instruments
  in
  let* schedules_value = field fields "fee_schedules" in
  let* schedules_json = list ~name:"fee_schedules" schedules_value in
  let* schedules =
    map_list (parse_fee_schedule instrument_ids) schedules_json
  in
  let scheduled =
    List.map Fee_schedule.instrument_id schedules
    |> List.sort_uniq Id.Instrument.compare
  in
  let expected = Id.Instrument.Set.elements instrument_ids in
  let* () =
    if scheduled = expected then Ok ()
    else
      Error "fee schedules must cover every configured instrument exactly once"
  in
  Ok (participation_bps, schedules)

let parse_execution_v2 instruments fields =
  let* participation_bps, schedules =
    parse_execution_common instruments fields
  in
  Execution.create_v2 ~participation_bps ~fee_schedules:schedules

let parse_order_book_execution instruments fields =
  let* participation_bps, fee_schedules =
    parse_execution_common instruments fields
  in
  let* max_depth_levels =
    Result.bind
      (field fields "max_depth_levels")
      (integer ~name:"max_depth_levels")
  in
  Execution.create_order_book ~participation_bps ~fee_schedules
    ~max_depth_levels

let parse_conservative_execution instruments fields =
  let* participation_bps, fee_schedules =
    parse_execution_common instruments fields
  in
  let* spread_json = field fields "spread_model" in
  let* spread_fields =
    object_fields ~name:"spread model"
      ~expected:[ "model"; "half_spread_bps" ]
      spread_json
  in
  let* spread_name =
    Result.bind (field spread_fields "model") (string ~name:"spread model")
  in
  let* () =
    if String.equal spread_name "fixed_half_spread_v1" then Ok ()
    else Error "unsupported spread model"
  in
  let* half_spread_bps =
    Result.bind
      (field spread_fields "half_spread_bps")
      (integer ~name:"half_spread_bps")
  in
  let* impact_json = field fields "impact_model" in
  let* impact_fields =
    object_fields ~name:"impact model"
      ~expected:[ "model"; "coefficient_bps"; "missing_volume_policy" ]
      impact_json
  in
  let* impact_name =
    Result.bind (field impact_fields "model") (string ~name:"impact model")
  in
  let* () =
    if String.equal impact_name "linear_participation_v1" then Ok ()
    else Error "unsupported impact model"
  in
  let* impact_coefficient_bps =
    Result.bind
      (field impact_fields "coefficient_bps")
      (integer ~name:"impact coefficient_bps")
  in
  let* missing_name =
    Result.bind
      (field impact_fields "missing_volume_policy")
      (string ~name:"missing_volume_policy")
  in
  let* missing_volume_policy =
    match missing_name with
    | "reject" -> Ok Execution.Reject_missing_volume
    | "zero_impact" -> Ok Execution.Zero_impact
    | _ -> Error "missing_volume_policy must be reject or zero_impact"
  in
  Execution.create_conservative ~participation_bps ~fee_schedules
    ~half_spread_bps ~impact_coefficient_bps ~missing_volume_policy

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

let parse_versioned_execution ~contract_version ~instruments json =
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
  let* loose_fields =
    match configuration_json with
    | `Assoc fields -> Ok fields
    | _ -> Error (model_name ^ " execution configuration must be a JSON object")
  in
  let* version_json = field loose_fields "version" in
  let* version = string ~name:"execution configuration version" version_json in
  let* expected = Execution_model.required_fields execution_model version in
  let* configuration =
    object_fields
      ~name:(model_name ^ " execution configuration")
      ~expected configuration_json
  in
  if not (Execution_model.supports_configuration execution_model version) then
    Error
      (Printf.sprintf
         "unsupported execution configuration version %S for model %S" version
         model_name)
  else
    let* execution =
      if
        List.mem model_name
          [ "completed_bar_next_open_v1"; "completed_bar_adverse_touch_v1" ]
      then parse_conservative_execution instruments configuration
      else if String.equal model_name "order_book_v1" then
        parse_order_book_execution instruments configuration
      else if
        String.equal model_name "quote_trade_v1" || String.equal version "2"
      then parse_execution_v2 instruments configuration
      else parse_execution_values configuration
    in
    Ok (execution_model, execution)

let parse_execution ~contract_version ~instruments json =
  if
    List.mem contract_version
      [ "16"; "15"; "14"; "13"; "12"; "11"; "10"; "9"; "8"; "7"; "6"; "5" ]
  then parse_versioned_execution ~contract_version ~instruments json
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

let parse_submit_intent ~contract_version json =
  let versioned =
    List.mem contract_version
      [ "16"; "15"; "14"; "13"; "12"; "11"; "10"; "9"; "8" ]
  in
  let* fields =
    object_fields ~name:"submit_order intent"
      ~expected:
        (if versioned then
           [
             "type";
             "instrument_id";
             "side";
             "quantity";
             "order_kind";
             "trigger_price";
             "limit_price";
             "time_in_force";
             "venue_id";
             "calendar_id";
             "expires_at";
           ]
         else
           [
             "type";
             "instrument_id";
             "side";
             "quantity";
             "order_kind";
             "limit_price";
           ])
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
  let* trigger_json =
    if versioned then field fields "trigger_price" else Ok `Null
  in
  let* kind =
    match (kind_name, trigger_json, limit_json) with
    | "market", `Null, `Null -> Ok Order.Market
    | "limit", `Null, value ->
        let* limit = parse_price ~name:"limit_price" value in
        Ok (Order.Limit limit)
    | "stop", trigger, `Null when versioned ->
        let* trigger = parse_price ~name:"trigger_price" trigger in
        Ok (Order.Stop trigger)
    | "stop_limit", trigger, limit when versioned ->
        let* trigger_price = parse_price ~name:"trigger_price" trigger in
        let* limit_price = parse_price ~name:"limit_price" limit in
        Ok (Order.Stop_limit { trigger_price; limit_price })
    | "market", _, _ ->
        Error "market order trigger_price and limit_price must be null"
    | _ -> Error "invalid order_kind"
  in
  let* time_in_force =
    if not versioned then Ok (Order.compatibility_time_in_force kind)
    else
      let* tif_json = field fields "time_in_force" in
      let* tif = string ~name:"time_in_force" tif_json in
      let* venue_json = field fields "venue_id" in
      let* calendar_json = field fields "calendar_id" in
      let* expires_json = field fields "expires_at" in
      match (tif, venue_json, calendar_json, expires_json) with
      | "gtc", `Null, `Null, `Null -> Ok Order.Gtc
      | "ioc", `Null, `Null, `Null -> Ok Order.Ioc
      | "fok", `Null, `Null, `Null -> Ok Order.Fok
      | "day", venue, calendar, `Null ->
          let* venue_id = parse_id Id.Venue.of_string ~name:"venue_id" venue in
          let* calendar_id =
            parse_id Id.Venue_calendar.of_string ~name:"calendar_id" calendar
          in
          Ok (Order.Day { venue_id; calendar_id })
      | "gtd", `Null, `Null, expires ->
          let* value = string ~name:"expires_at" expires in
          let* expires_at = Codec.ptime_of_string value in
          Ok (Order.Gtd expires_at)
      | _ -> Error "time_in_force companion fields are inconsistent"
  in
  let* request =
    Order.request_v8 ~instrument_id ~side ~quantity ~kind ~time_in_force
      ~origin:Order.Direct
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

let parse_metric_intent ~contract_version json =
  if not (String.equal contract_version "16") then
    let* fields =
      object_fields ~name:"emit_metric intent"
        ~expected:[ "type"; "name"; "value" ]
        json
    in
    let* name_json = field fields "name" in
    let* name = string ~name:"metric name" name_json in
    let* value_json = field fields "value" in
    let* value = string ~name:"metric value" value_json in
    let* metric = Metric.create ~name ~value:(Metric.String value) () in
    Ok (Strategy.Emit_metric metric)
  else
    let* fields =
      match json with
      | `Assoc fields ->
          let names = List.map fst fields in
          let unique = List.sort_uniq String.compare names in
          let allowed =
            [ "aggregation"; "dimensions"; "name"; "type"; "unit"; "value" ]
          in
          if List.length names <> List.length unique then
            Error "emit_metric intent must not contain duplicate fields"
          else if
            not
              (List.for_all (fun name -> List.mem name allowed) unique
              && List.for_all
                   (fun name -> List.mem name unique)
                   [ "type"; "name"; "value" ])
          then Error "emit_metric intent has unknown or missing fields"
          else Ok fields
      | _ -> Error "emit_metric intent must be a JSON object"
    in
    let* name_json = field fields "name" in
    let* name = string ~name:"metric name" name_json in
    let* value =
      let* json = field fields "value" in
      let* value_fields =
        object_fields ~name:"metric value" ~expected:[ "type"; "value" ] json
      in
      let* type_json = field value_fields "type" in
      let* value_type = string ~name:"metric value type" type_json in
      let* value_json = field value_fields "value" in
      match (value_type, value_json) with
      | "numeric", `String value ->
          Metric.numeric_of_string value
          |> Result.map (fun value -> Metric.Numeric value)
      | "string", `String value -> Ok (Metric.String value)
      | "boolean", `Bool value -> Ok (Metric.Boolean value)
      | _ -> Error "metric value does not match its declared type"
    in
    let* unit_ =
      match List.assoc_opt "unit" fields with
      | None -> Ok None
      | Some json -> string ~name:"metric unit" json |> Result.map Option.some
    in
    let* dimensions =
      match List.assoc_opt "dimensions" fields with
      | None -> Ok []
      | Some (`Assoc dimensions) ->
          List.fold_left
            (fun result (key, json) ->
              let* values = result in
              let* value = string ~name:"metric dimension value" json in
              Ok ((key, value) :: values))
            (Ok []) dimensions
      | Some _ -> Error "metric dimensions must be an object"
    in
    let* aggregation =
      match List.assoc_opt "aggregation" fields with
      | None -> Ok None
      | Some json ->
          let* value = string ~name:"metric aggregation" json in
          Metric.aggregation_of_string value |> Result.map Option.some
    in
    let* metric =
      Metric.create ~name ~value ?unit_ ~dimensions ?aggregation ()
    in
    Ok (Strategy.Emit_metric metric)

let parse_intent ~contract_version json =
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
      | Some (`String "submit_order") ->
          parse_submit_intent ~contract_version json
      | Some (`String "cancel_order") -> parse_cancel_intent json
      | Some (`String "emit_metric") ->
          parse_metric_intent ~contract_version json
      | Some _ -> Error "unsupported intent type"
      | None -> Error "intent is missing type")
  | _ -> Error "intent must be a JSON object"

let intent_of_yojson ?(contract_version = Contract.previous_version) json =
  parse_intent ~contract_version json
  |> Result.map_error (fun message ->
      Diagnostic.make ~code:Diagnostic.Scenario_invalid
        ~phase:Diagnostic.Validation ~json_path:"$" message)

let parse_schedule_item ~contract_version json =
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
    let* intents = map_list (parse_intent ~contract_version) intents_json in
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

let parse_initial_position json =
  let* fields =
    object_fields ~name:"initial position"
      ~expected:
        [
          "instrument_id";
          "quantity";
          "cost_basis";
          "realized_pnl";
          "dividend_pnl";
          "execution_fees";
          "borrow_fees";
        ]
      json
  in
  let* instrument_json = field fields "instrument_id" in
  let* instrument_id =
    parse_id Id.Instrument.of_string ~name:"instrument_id" instrument_json
  in
  let* quantity_json = field fields "quantity" in
  let* quantity =
    parse_quantity ~name:"initial position quantity" quantity_json
  in
  let* basis_json = field fields "cost_basis" in
  let* cost_basis =
    parse_money ~name:"initial position cost_basis" basis_json
  in
  let* realized_json = field fields "realized_pnl" in
  let* realized_pnl =
    parse_money ~name:"initial position realized_pnl" realized_json
  in
  let* dividend_json = field fields "dividend_pnl" in
  let* dividend_pnl =
    parse_money ~name:"initial position dividend_pnl" dividend_json
  in
  let* execution_json = field fields "execution_fees" in
  let* execution_fees =
    parse_money ~name:"initial position execution_fees" execution_json
  in
  let* borrow_json = field fields "borrow_fees" in
  let* borrow_fees =
    parse_money ~name:"initial position borrow_fees" borrow_json
  in
  Initial_portfolio.position ~instrument_id ~quantity ~cost_basis ~realized_pnl
    ~dividend_pnl ~execution_fees ~borrow_fees

let parse_initial_mark json =
  let* fields =
    object_fields ~name:"initial mark"
      ~expected:[ "instrument_id"; "price" ]
      json
  in
  let* instrument_json = field fields "instrument_id" in
  let* instrument_id =
    parse_id Id.Instrument.of_string ~name:"instrument_id" instrument_json
  in
  let* price_json = field fields "price" in
  let* price = parse_price ~name:"initial mark price" price_json in
  Ok (instrument_id, price)

let parse_initial_fx_rate json =
  let* mark = parse_fx_mark json in
  Ok (mark.Market_slice.currency, mark.rate)

let parse_initial_portfolio ~base_currency json =
  let* fields =
    object_fields ~name:"initial portfolio"
      ~expected:[ "cash"; "positions"; "marks"; "fx_rates" ]
      json
  in
  let* cash_json = field fields "cash" in
  let* cash_json = list ~name:"initial portfolio cash" cash_json in
  let* cash = map_list parse_cash_balance cash_json in
  let* positions_json = field fields "positions" in
  let* positions_json =
    list ~name:"initial portfolio positions" positions_json
  in
  let* positions = map_list parse_initial_position positions_json in
  let* marks_json = field fields "marks" in
  let* marks_json = list ~name:"initial portfolio marks" marks_json in
  let* marks = map_list parse_initial_mark marks_json in
  let* fx_json = field fields "fx_rates" in
  let* fx_json = list ~name:"initial portfolio FX rates" fx_json in
  let* fx_rates = map_list parse_initial_fx_rate fx_json in
  Initial_portfolio.create ~base_currency ~cash ~positions ~marks ~fx_rates

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
  | ("stock_dividend" | "rights" | "spin_off") as distribution_name ->
      let* () =
        object_fields ~name:"distribution corporate action"
          ~expected:
            [
              "type";
              "action_id";
              "instrument_id";
              "destination_instrument_id";
              "numerator";
              "denominator";
              "basis_allocation_bps";
              "fractional_policy";
            ]
          json
        |> Result.map (fun _ -> ())
      in
      let* destination_json = field fields "destination_instrument_id" in
      let* destination_instrument_id =
        parse_id Id.Instrument.of_string ~name:"destination_instrument_id"
          destination_json
      in
      let* numerator =
        Result.bind (field fields "numerator")
          (parse_int64 ~name:"distribution numerator")
      in
      let* denominator =
        Result.bind
          (field fields "denominator")
          (parse_int64 ~name:"distribution denominator")
      in
      let* basis_allocation_bps =
        Result.bind
          (field fields "basis_allocation_bps")
          (integer ~name:"basis_allocation_bps")
      in
      let* fractional_json = field fields "fractional_policy" in
      let* fractional_fields =
        match fractional_json with
        | `Assoc fields -> Ok fields
        | _ -> Error "fractional_policy must be a JSON object"
      in
      let* policy_name =
        Result.bind
          (field fractional_fields "policy")
          (string ~name:"fractional policy")
      in
      let* fractional_policy =
        match policy_name with
        | "reject" ->
            object_fields ~name:"reject fractional policy"
              ~expected:[ "policy" ] fractional_json
            |> Result.map (fun _ -> Corporate_action.Reject_fractional)
        | "cash_in_lieu" ->
            let* () =
              object_fields ~name:"cash-in-lieu fractional policy"
                ~expected:[ "policy"; "price"; "currency" ]
                fractional_json
              |> Result.map (fun _ -> ())
            in
            let* price =
              Result.bind
                (field fractional_fields "price")
                (parse_price ~name:"cash-in-lieu price")
            in
            let* currency =
              Result.bind
                (field fractional_fields "currency")
                (string ~name:"cash-in-lieu currency")
            in
            Ok (Corporate_action.Cash_in_lieu { price; currency })
        | _ -> Error "fractional policy must be reject or cash_in_lieu"
      in
      let distribution_type =
        match distribution_name with
        | "stock_dividend" -> Corporate_action.Stock_dividend
        | "rights" -> Rights
        | "spin_off" -> Spin_off
        | _ -> assert false
      in
      Corporate_action.distribution ~id ~instrument_id ~distribution_type
        ~destination_instrument_id ~numerator ~denominator ~basis_allocation_bps
        ~fractional_policy
  | _ -> Error "unsupported corporate action type"

let parse_terminal_policy json =
  let* fields =
    match json with
    | `Assoc fields -> Ok fields
    | _ -> Error "terminal_policy must be a JSON object"
  in
  let* policy =
    Result.bind (field fields "policy") (string ~name:"terminal policy")
  in
  match policy with
  | "hold" ->
      object_fields ~name:"hold terminal policy" ~expected:[ "policy" ] json
      |> Result.map (fun _ -> Instrument_lifecycle.Hold)
  | "cash_out" ->
      let* () =
        object_fields ~name:"cash-out terminal policy"
          ~expected:[ "policy"; "price"; "currency" ]
          json
        |> Result.map (fun _ -> ())
      in
      let* price =
        Result.bind (field fields "price") (parse_price ~name:"terminal price")
      in
      let* currency =
        Result.bind (field fields "currency") (string ~name:"terminal currency")
      in
      Ok (Instrument_lifecycle.Cash_out { price; currency })
  | _ -> Error "terminal policy must be hold or cash_out"

let parse_lifecycle_event json =
  let* fields =
    match json with
    | `Assoc fields -> Ok fields
    | _ -> Error "lifecycle event must be a JSON object"
  in
  let* kind_name =
    Result.bind (field fields "type") (string ~name:"lifecycle event type")
  in
  let* id =
    Result.bind (field fields "event_id")
      (parse_id Id.Corporate_action.of_string ~name:"event_id")
  in
  let* instrument_id =
    Result.bind
      (field fields "instrument_id")
      (parse_id Id.Instrument.of_string ~name:"instrument_id")
  in
  let* kind =
    match kind_name with
    | "halt" ->
        let* () =
          object_fields ~name:"halt lifecycle event"
            ~expected:[ "type"; "event_id"; "instrument_id"; "reason" ]
            json
          |> Result.map (fun _ -> ())
        in
        Result.bind (field fields "reason") (string ~name:"halt reason")
        |> Result.map (fun reason -> Instrument_lifecycle.Halt { reason })
    | "resume" ->
        object_fields ~name:"resume lifecycle event"
          ~expected:[ "type"; "event_id"; "instrument_id" ]
          json
        |> Result.map (fun _ -> Instrument_lifecycle.Resume)
    | "identifier_change" ->
        let* () =
          object_fields ~name:"identifier-change lifecycle event"
            ~expected:
              [
                "type";
                "event_id";
                "instrument_id";
                "symbol";
                "provider";
                "provider_instrument_id";
              ]
            json
          |> Result.map (fun _ -> ())
        in
        let text name = Result.bind (field fields name) (string ~name) in
        let* symbol = text "symbol" in
        let* provider = text "provider" in
        let* provider_instrument_id = text "provider_instrument_id" in
        Ok
          (Instrument_lifecycle.Identifier_change
             { symbol; provider; provider_instrument_id })
    | "expiration" | "delisting" ->
        let delisting = String.equal kind_name "delisting" in
        let expected =
          [ "type"; "event_id"; "instrument_id"; "terminal_policy" ]
          @ if delisting then [ "reason" ] else []
        in
        let* () =
          object_fields ~name:"terminal lifecycle event" ~expected json
          |> Result.map (fun _ -> ())
        in
        let* terminal_policy =
          Result.bind (field fields "terminal_policy") parse_terminal_policy
        in
        if delisting then
          Result.bind (field fields "reason") (string ~name:"delisting reason")
          |> Result.map (fun reason ->
              Instrument_lifecycle.Delisting { terminal_policy; reason })
        else Ok (Instrument_lifecycle.Expiration { terminal_policy })
    | _ -> Error "unsupported lifecycle event type"
  in
  Instrument_lifecycle.create_event ~id ~instrument_id ~kind

let parse_financing json =
  let* fields =
    object_fields ~name:"financing policy"
      ~expected:
        [
          "day_count";
          "compounding";
          "borrow_missing_data";
          "cash_missing_data";
          "locate_policy";
          "recall_policy";
        ]
      json
  in
  let text name = Result.bind (field fields name) (string ~name) in
  let* day_count =
    match text "day_count" with
    | Ok "actual_365" -> Ok Financing.Actual_365
    | Ok "actual_360" -> Ok Financing.Actual_360
    | Ok _ -> Error "day_count must be actual_365 or actual_360"
    | Error _ as error -> error
  in
  let* compounding =
    match text "compounding" with
    | Ok "simple" -> Ok Financing.Simple
    | Ok "daily" -> Ok Financing.Daily
    | Ok _ -> Error "compounding must be simple or daily"
    | Error _ as error -> error
  in
  let missing name =
    match text name with
    | Ok "reject" -> Ok Financing.Reject
    | Ok "zero" -> Ok Financing.Zero
    | Ok _ -> Error (name ^ " must be reject or zero")
    | Error _ as error -> error
  in
  let* borrow_missing_data = missing "borrow_missing_data" in
  let* cash_missing_data = missing "cash_missing_data" in
  let* locate_policy =
    match text "locate_policy" with
    | Ok "reject_order" -> Ok Financing.Reject_order
    | Ok "clip_fill" -> Ok Financing.Clip_fill
    | Ok _ -> Error "locate_policy must be reject_order or clip_fill"
    | Error _ as error -> error
  in
  let* recall_policy =
    match text "recall_policy" with
    | Ok "reject_new_shorts" -> Ok Financing.Reject_new_shorts
    | Ok "close_out" -> Ok Financing.Close_out
    | Ok _ -> Error "recall_policy must be reject_new_shorts or close_out"
    | Error _ as error -> error
  in
  Ok
    (Financing.policy ~day_count ~compounding ~borrow_missing_data
       ~cash_missing_data ~locate_policy ~recall_policy)

let parse_settlement_calendar json =
  let* fields =
    object_fields ~name:"settlement calendar"
      ~expected:[ "calendar_id"; "version"; "business_dates" ]
      json
  in
  let* calendar_id =
    Result.bind
      (field fields "calendar_id")
      (string ~name:"settlement calendar_id")
  in
  let* version =
    Result.bind (field fields "version")
      (string ~name:"settlement calendar version")
  in
  let* dates_json =
    Result.bind
      (field fields "business_dates")
      (list ~name:"settlement business_dates")
  in
  let* business_dates =
    map_list (string ~name:"settlement business date") dates_json
  in
  Settlement.calendar ~calendar_id ~version ~business_dates

let parse_settlement_rule json =
  let* fields =
    object_fields ~name:"settlement rule"
      ~expected:[ "instrument_id"; "calendar_id"; "lag_business_days" ]
      json
  in
  let* instrument_id =
    Result.bind
      (field fields "instrument_id")
      (parse_id Id.Instrument.of_string ~name:"settlement instrument_id")
  in
  let* calendar_id =
    Result.bind
      (field fields "calendar_id")
      (string ~name:"settlement calendar_id")
  in
  let* lag_business_days =
    Result.bind
      (field fields "lag_business_days")
      (integer ~name:"lag_business_days")
  in
  Settlement.rule ~instrument_id ~calendar_id ~lag_business_days

let parse_settlement json =
  let* fields =
    object_fields ~name:"settlement policy"
      ~expected:
        [ "cash_buying_power"; "position_availability"; "calendars"; "rules" ]
      json
  in
  let text name = Result.bind (field fields name) (string ~name) in
  let* cash_buying_power =
    match text "cash_buying_power" with
    | Ok "total_cash" -> Ok Settlement.Total_cash
    | Ok "settled_cash" -> Ok Settlement.Settled_cash
    | Ok _ -> Error "cash_buying_power must be total_cash or settled_cash"
    | Error _ as error -> error
  in
  let* position_availability =
    match text "position_availability" with
    | Ok "total_positions" -> Ok Settlement.Total_positions
    | Ok "settled_positions" -> Ok Settlement.Settled_positions
    | Ok _ ->
        Error
          "position_availability must be total_positions or settled_positions"
    | Error _ as error -> error
  in
  let* calendars_json =
    Result.bind (field fields "calendars") (list ~name:"settlement calendars")
  in
  let* calendars = map_list parse_settlement_calendar calendars_json in
  let* rules_json =
    Result.bind (field fields "rules") (list ~name:"settlement rules")
  in
  let* rules = map_list parse_settlement_rule rules_json in
  Settlement.policy ~cash_buying_power ~position_availability ~calendars ~rules

let parse_settlement_failure json =
  let* fields =
    object_fields ~name:"settlement failure"
      ~expected:[ "instruction_id"; "reason" ]
      json
  in
  let* instruction_id =
    Result.bind
      (field fields "instruction_id")
      (string ~name:"settlement instruction_id")
  in
  let* reason =
    Result.bind (field fields "reason")
      (string ~name:"settlement failure reason")
  in
  Settlement.failure ~instruction_id ~reason

let parse_borrow_observation json =
  let* fields =
    object_fields ~name:"borrow observation"
      ~expected:
        [
          "instrument_id";
          "effective_at";
          "available_quantity";
          "annual_rate_bps";
          "recalled";
        ]
      json
  in
  let* instrument_id =
    Result.bind
      (field fields "instrument_id")
      (parse_id Id.Instrument.of_string ~name:"borrow instrument_id")
  in
  let* effective_at =
    Result.bind
      (field fields "effective_at")
      (parse_timestamp ~name:"borrow effective_at")
  in
  let* available_quantity =
    Result.bind
      (field fields "available_quantity")
      (parse_quantity ~name:"borrow available_quantity")
  in
  let* annual_rate_bps =
    Result.bind
      (field fields "annual_rate_bps")
      (integer ~name:"borrow annual_rate_bps")
  in
  let* recalled =
    match field fields "recalled" with
    | Ok (`Bool value) -> Ok value
    | Ok _ -> Error "borrow recalled must be a boolean"
    | Error _ as error -> error
  in
  Financing.borrow_observation ~instrument_id ~effective_at ~available_quantity
    ~annual_rate_bps ~recalled

let parse_cash_rate_observation json =
  let* fields =
    object_fields ~name:"cash rate observation"
      ~expected:
        [ "currency"; "effective_at"; "credit_rate_bps"; "debit_rate_bps" ]
      json
  in
  let* currency =
    Result.bind (field fields "currency") (string ~name:"cash rate currency")
  in
  let* effective_at =
    Result.bind
      (field fields "effective_at")
      (parse_timestamp ~name:"cash rate effective_at")
  in
  let* credit_rate_bps =
    Result.bind
      (field fields "credit_rate_bps")
      (integer ~name:"credit_rate_bps")
  in
  let* debit_rate_bps =
    Result.bind (field fields "debit_rate_bps") (integer ~name:"debit_rate_bps")
  in
  Financing.cash_rate_observation ~currency ~effective_at ~credit_rate_bps
    ~debit_rate_bps

let parse_market_event json =
  let* loose_fields =
    match json with
    | `Assoc fields -> Ok fields
    | _ -> Error "market event must be a JSON object"
  in
  let* type_name =
    Result.bind (field loose_fields "type") (string ~name:"market event type")
  in
  let common_fields =
    [
      "type";
      "instrument_id";
      "event_at";
      "available_at";
      "received_at";
      "ingest_sequence";
    ]
  in
  let specific_fields =
    match type_name with
    | "quote" -> [ "bid_price"; "bid_quantity"; "ask_price"; "ask_quantity" ]
    | "trade" -> [ "price"; "quantity"; "aggressor_side" ]
    | _ -> []
  in
  let* () =
    if specific_fields = [] then
      Error "market event type must be quote or trade"
    else Ok ()
  in
  let* fields =
    object_fields
      ~name:(type_name ^ " market event")
      ~expected:(common_fields @ specific_fields)
      json
  in
  let* instrument_id =
    Result.bind
      (field fields "instrument_id")
      (parse_id Id.Instrument.of_string ~name:"market event instrument_id")
  in
  let* event_at =
    Result.bind (field fields "event_at")
      (parse_timestamp ~name:"market event_at")
  in
  let* available_at =
    Result.bind
      (field fields "available_at")
      (parse_timestamp ~name:"market available_at")
  in
  let* received_at =
    Result.bind
      (field fields "received_at")
      (parse_timestamp ~name:"market received_at")
  in
  let* ingest_sequence =
    Result.bind
      (field fields "ingest_sequence")
      (parse_int64 ~name:"market ingest_sequence")
  in
  match type_name with
  | "quote" ->
      let* bid_price =
        Result.bind (field fields "bid_price") (parse_price ~name:"bid_price")
      in
      let* bid_quantity =
        Result.bind
          (field fields "bid_quantity")
          (parse_quantity ~name:"bid_quantity")
      in
      let* ask_price =
        Result.bind (field fields "ask_price") (parse_price ~name:"ask_price")
      in
      let* ask_quantity =
        Result.bind
          (field fields "ask_quantity")
          (parse_quantity ~name:"ask_quantity")
      in
      Market_event.quote ~instrument_id ~event_at ~available_at ~received_at
        ~ingest_sequence ~bid_price ~bid_quantity ~ask_price ~ask_quantity
  | "trade" ->
      let* price =
        Result.bind (field fields "price") (parse_price ~name:"trade price")
      in
      let* quantity =
        Result.bind (field fields "quantity")
          (parse_quantity ~name:"trade quantity")
      in
      let* aggressor_side =
        Result.bind
          (Result.bind
             (field fields "aggressor_side")
             (string ~name:"aggressor_side"))
          Market_event.aggressor_side_of_string
      in
      Market_event.trade ~instrument_id ~event_at ~available_at ~received_at
        ~ingest_sequence ~price ~quantity ~aggressor_side
  | _ -> assert false

let parse_order_book_level json =
  let* fields =
    object_fields ~name:"order-book level" ~expected:[ "price"; "quantity" ]
      json
  in
  let* price =
    Result.bind (field fields "price") (parse_price ~name:"book level price")
  in
  let* quantity =
    Result.bind (field fields "quantity")
      (parse_quantity ~name:"book level quantity")
  in
  Order_book_event.level ~price ~quantity

let parse_order_book_event json =
  let* loose_fields =
    match json with
    | `Assoc fields -> Ok fields
    | _ -> Error "order-book event must be a JSON object"
  in
  let* type_name =
    Result.bind
      (field loose_fields "type")
      (string ~name:"order-book event type")
  in
  let common =
    [
      "type";
      "instrument_id";
      "event_at";
      "available_at";
      "received_at";
      "ingest_sequence";
      "book_sequence";
    ]
  in
  let specific =
    match type_name with
    | "snapshot" -> [ "bids"; "asks" ]
    | "set" -> [ "side"; "price"; "quantity" ]
    | "delete" -> [ "side"; "price" ]
    | "trade" -> [ "price"; "quantity"; "aggressor_side" ]
    | _ -> []
  in
  let* () =
    if specific = [] then
      Error "order-book event type must be snapshot, set, delete, or trade"
    else Ok ()
  in
  let* fields =
    object_fields
      ~name:(type_name ^ " order-book event")
      ~expected:(common @ specific) json
  in
  let* instrument_id =
    Result.bind
      (field fields "instrument_id")
      (parse_id Id.Instrument.of_string ~name:"order-book instrument_id")
  in
  let* event_at =
    Result.bind (field fields "event_at")
      (parse_timestamp ~name:"order-book event_at")
  in
  let* available_at =
    Result.bind
      (field fields "available_at")
      (parse_timestamp ~name:"order-book available_at")
  in
  let* received_at =
    Result.bind
      (field fields "received_at")
      (parse_timestamp ~name:"order-book received_at")
  in
  let* ingest_sequence =
    Result.bind
      (field fields "ingest_sequence")
      (parse_int64 ~name:"order-book ingest_sequence")
  in
  let* book_sequence =
    Result.bind
      (field fields "book_sequence")
      (parse_int64 ~name:"order-book book_sequence")
  in
  let side () =
    let* value =
      Result.bind (field fields "side") (string ~name:"order-book side")
    in
    Order_book_event.side_of_string value
  in
  let price () =
    Result.bind (field fields "price") (parse_price ~name:"order-book price")
  in
  let quantity () =
    Result.bind (field fields "quantity")
      (parse_quantity ~name:"order-book quantity")
  in
  match type_name with
  | "snapshot" ->
      let* bids_json =
        Result.bind (field fields "bids") (list ~name:"order-book bids")
      in
      let* asks_json =
        Result.bind (field fields "asks") (list ~name:"order-book asks")
      in
      let* bids = map_list parse_order_book_level bids_json in
      let* asks = map_list parse_order_book_level asks_json in
      Order_book_event.snapshot ~instrument_id ~event_at ~available_at
        ~received_at ~ingest_sequence ~book_sequence ~bids ~asks
  | "set" ->
      let* side = side () in
      let* price = price () in
      let* quantity = quantity () in
      Order_book_event.set ~instrument_id ~event_at ~available_at ~received_at
        ~ingest_sequence ~book_sequence ~side ~price ~quantity
  | "delete" ->
      let* side = side () in
      let* price = price () in
      Order_book_event.delete ~instrument_id ~event_at ~available_at
        ~received_at ~ingest_sequence ~book_sequence ~side ~price
  | "trade" ->
      let* price = price () in
      let* quantity = quantity () in
      let* aggressor_name =
        Result.bind
          (field fields "aggressor_side")
          (string ~name:"order-book aggressor_side")
      in
      let* aggressor_side =
        Market_event.aggressor_side_of_string aggressor_name
      in
      Order_book_event.trade ~instrument_id ~event_at ~available_at ~received_at
        ~ingest_sequence ~book_sequence ~price ~quantity ~aggressor_side
  | _ -> assert false

let parse_slice ~contract_version json =
  let financing_fields =
    if List.mem contract_version [ "16"; "15"; "14"; "13"; "12"; "11"; "10" ]
    then [ "borrow_observations"; "cash_rate_observations" ]
    else []
  in
  let settlement_fields =
    if List.mem contract_version [ "16"; "15"; "14"; "13"; "12"; "11" ] then
      [ "settlement_failures" ]
    else []
  in
  let lifecycle_fields =
    if List.mem contract_version [ "16"; "15"; "14"; "13"; "12" ] then
      [ "lifecycle_events" ]
    else []
  in
  let market_event_fields =
    if List.mem contract_version [ "16"; "15"; "14" ] then [ "market_events" ]
    else []
  in
  let order_book_event_fields =
    if List.mem contract_version [ "16"; "15" ] then [ "order_book_events" ]
    else []
  in
  let* fields =
    object_fields ~name:"market slice"
      ~expected:
        ([
           "slice_sequence";
           "start_at";
           "end_at";
           "available_at";
           "received_at";
           "bars";
           "fx_rates";
           "corporate_actions";
         ]
        @ financing_fields @ settlement_fields @ lifecycle_fields
        @ market_event_fields @ order_book_event_fields)
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
  if List.mem contract_version [ "16"; "15"; "14"; "13"; "12"; "11"; "10" ] then
    let* borrow_json =
      Result.bind
        (field fields "borrow_observations")
        (list ~name:"borrow_observations")
    in
    let* borrow_observations = map_list parse_borrow_observation borrow_json in
    let* cash_json =
      Result.bind
        (field fields "cash_rate_observations")
        (list ~name:"cash_rate_observations")
    in
    let* cash_rate_observations =
      map_list parse_cash_rate_observation cash_json
    in
    if List.mem contract_version [ "16"; "15"; "14"; "13"; "12"; "11" ] then
      let* failures_json =
        Result.bind
          (field fields "settlement_failures")
          (list ~name:"settlement_failures")
      in
      let* settlement_failures =
        map_list parse_settlement_failure failures_json
      in
      if List.mem contract_version [ "16"; "15"; "14"; "13"; "12" ] then
        let* lifecycle_json =
          Result.bind
            (field fields "lifecycle_events")
            (list ~name:"lifecycle_events")
        in
        let* lifecycle_events = map_list parse_lifecycle_event lifecycle_json in
        if List.mem contract_version [ "16"; "15"; "14" ] then
          let* events_json =
            Result.bind
              (field fields "market_events")
              (list ~name:"market_events")
          in
          let* market_events = map_list parse_market_event events_json in
          if List.mem contract_version [ "16"; "15" ] then
            let* book_events_json =
              Result.bind
                (field fields "order_book_events")
                (list ~name:"order_book_events")
            in
            let* order_book_events =
              map_list parse_order_book_event book_events_json
            in
            Market_slice.create_v15 ~slice_sequence ~start_at ~end_at
              ~available_at ~received_at ~bars ~fx_rates ~corporate_actions
              ~borrow_observations ~cash_rate_observations ~settlement_failures
              ~lifecycle_events ~market_events ~order_book_events
          else
            Market_slice.create_v14 ~slice_sequence ~start_at ~end_at
              ~available_at ~received_at ~bars ~fx_rates ~corporate_actions
              ~borrow_observations ~cash_rate_observations ~settlement_failures
              ~lifecycle_events ~market_events
        else
          let create =
            if String.equal contract_version "13" then Market_slice.create_v13
            else Market_slice.create_v12
          in
          create ~slice_sequence ~start_at ~end_at ~available_at ~received_at
            ~bars ~fx_rates ~corporate_actions ~borrow_observations
            ~cash_rate_observations ~settlement_failures ~lifecycle_events
      else
        Market_slice.create_v11 ~slice_sequence ~start_at ~end_at ~available_at
          ~received_at ~bars ~fx_rates ~corporate_actions ~borrow_observations
          ~cash_rate_observations ~settlement_failures
    else
      Market_slice.create_v10 ~slice_sequence ~start_at ~end_at ~available_at
        ~received_at ~bars ~fx_rates ~corporate_actions ~borrow_observations
        ~cash_rate_observations
  else
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
    let* initial_cash, initial_portfolio =
      if
        List.mem contract_version
          [ "16"; "15"; "14"; "13"; "12"; "11"; "10"; "9"; "8"; "7"; "6" ]
      then
        let* portfolio =
          parse_initial_portfolio ~base_currency shape.initial_state
          |> at (child root "initial_portfolio")
        in
        Ok (portfolio.Initial_portfolio.cash, Some portfolio)
      else
        let* initial_cash_json =
          list ~name:"initial_cash" shape.initial_state
          |> at (child root "initial_cash")
        in
        let* initial_cash =
          map_list_at
            (child root "initial_cash")
            parse_cash_balance initial_cash_json
        in
        Ok (initial_cash, None)
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
      parse_risk ~contract_version base_currency instruments shape.risk
      |> at (child root "risk")
    in
    let* () =
      match initial_portfolio with
      | None -> Ok ()
      | Some portfolio ->
          Scenario_validation.initial_portfolio ~root ~currencies ~catalog
            ~instruments ~risk portfolio
    in
    let* execution_model, execution =
      parse_execution ~contract_version ~instruments shape.execution
      |> at (child root "execution")
    in
    let* financing =
      match (contract_version, shape.financing) with
      | ("12" | "11" | "10"), Some json ->
          parse_financing json |> at (child root "financing")
      | ("12" | "11" | "10"), None ->
          Error "missing financing policy" |> at (child root "financing")
      | _, _ -> Ok Financing.legacy_policy
    in
    let financing =
      if List.mem contract_version [ "16"; "15"; "14"; "13"; "12"; "11"; "10" ]
      then Some financing
      else None
    in
    let* settlement =
      match (contract_version, shape.settlement) with
      | ("12" | "11"), Some json ->
          let* policy = parse_settlement json |> at (child root "settlement") in
          Ok (Some policy)
      | ("12" | "11"), None ->
          Error "missing settlement policy" |> at (child root "settlement")
      | _, _ -> Ok None
    in
    let header : stream_header =
      {
        contract_version;
        metadata;
        run_id;
        base_currency;
        initial_cash;
        initial_portfolio;
        instruments;
        venue_calendars;
        risk;
        execution_model;
        execution;
        financing;
        settlement;
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
  let* schedule =
    map_list_at "$.schedule"
      (parse_schedule_item ~contract_version)
      schedule_json
  in
  let* slices_json = list ~name:"slices" shape.slices |> at "$.slices" in
  let* slices =
    map_list_at "$.slices" (parse_slice ~contract_version) slices_json
  in
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
      initial_portfolio = header.initial_portfolio;
      instruments = header.instruments;
      venue_calendars = header.venue_calendars;
      risk = header.risk;
      execution_model = header.execution_model;
      execution = header.execution;
      financing = header.financing;
      settlement = header.settlement;
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
    parse_slice ~contract_version:header.contract_version shape.market_slice
    |> at "$.payload.market_slice"
    |> Result.map_error (diagnostic code)
  in
  let* intents_json =
    list ~name:"intents" shape.intents
    |> at "$.payload.intents"
    |> Result.map_error (diagnostic code)
  in
  let* intents =
    map_list_at "$.payload.intents"
      (parse_intent ~contract_version:header.contract_version)
      intents_json
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
