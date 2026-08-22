let version = Contract.strategy_protocol_version
let max_message_bytes = Resource_limits.strategy_message_bytes

type initialization = {
  scenario_contract_version : string;
  scenario_sha256 : string;
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
}

type identity = { name : Id.Strategy.t; version : string option }

type response =
  | Ready of identity
  | Intents of Strategy.intent list
  | Stopped
  | Failed of string

type direction = Engine_to_strategy | Strategy_to_engine

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let string value = `String value
let sequence value = string (Int64.to_string value)
let price value = string (Scalar.Price.to_decimal_string value)
let quantity value = string (Scalar.Quantity.to_decimal_string value)
let money value = string (Scalar.Money.to_decimal_string value)
let weight value = string (Scalar.Weight.to_decimal_string value)
let ratio value = string (Scalar.Ratio.to_decimal_string value)
let timestamp value = string (Codec.ptime_to_string value)
let instrument_id value = string (Id.Instrument.to_string value)

let message ~protocol_version ~sequence:message_sequence ~message_type payload =
  `Assoc
    [
      ("strategy_protocol_version", string protocol_version);
      ("strategy_sequence", sequence message_sequence);
      ("message_type", string message_type);
      ("payload", payload);
    ]

let cash_balance_to_yojson (currency, amount) =
  `Assoc [ ("currency", string currency); ("amount", money amount) ]

let instrument_to_yojson instrument =
  `Assoc
    [
      ("instrument_id", instrument_id instrument.Instrument.id);
      ("symbol", string instrument.symbol);
      ("quote_currency", string instrument.quote_currency);
      ("tick_size", price instrument.tick_size);
      ("lot_size", quantity instrument.lot_size);
    ]

let phase_to_yojson (phase : Venue_calendar.phase) =
  `Assoc
    [
      ("phase", string (Venue_calendar.phase_kind_to_string phase.kind));
      ("opens_at", timestamp phase.opens_at);
      ("closes_at", timestamp phase.closes_at);
    ]

let session_to_yojson (session : Venue_calendar.session) =
  `Assoc
    [
      ("session_date", string session.session_date);
      ("policy", string (Venue_calendar.session_kind_to_string session.kind));
      ("phases", `List (List.map phase_to_yojson session.phases));
    ]

let venue_calendar_to_yojson (calendar : Venue_calendar.t) =
  `Assoc
    [
      ("calendar_id", string (Id.Venue_calendar.to_string calendar.id));
      ("calendar_version", string calendar.version);
      ("venue_id", string (Id.Venue.to_string calendar.venue_id));
      ( "instrument_ids",
        `List
          (calendar.instrument_ids |> Id.Instrument.Set.elements
         |> List.map instrument_id) );
      ("sessions", `List (List.map session_to_yojson calendar.sessions));
    ]

let group_kind_to_string = function
  | Risk.Issuer -> "issuer"
  | Risk.Sector -> "sector"
  | Risk.Currency -> "currency"
  | Risk.Country -> "country"
  | Risk.Asset_class -> "asset_class"
  | Risk.Custom -> "custom"

let nullable render = Option.fold ~none:`Null ~some:render

let modern_protocol protocol_version =
  List.mem protocol_version [ "12"; "11"; "10"; "9"; "8"; "7"; "6"; "5" ]

let financing_to_yojson policy =
  `Assoc
    [
      ( "day_count",
        string (Financing.day_count_to_string policy.Financing.day_count) );
      ( "compounding",
        string (Financing.compounding_to_string policy.compounding) );
      ( "borrow_missing_data",
        string (Financing.missing_data_to_string policy.borrow_missing_data) );
      ( "cash_missing_data",
        string (Financing.missing_data_to_string policy.cash_missing_data) );
      ( "locate_policy",
        string (Financing.locate_policy_to_string policy.locate_policy) );
      ( "recall_policy",
        string (Financing.recall_policy_to_string policy.recall_policy) );
    ]

let settlement_to_yojson (policy : Settlement.policy) =
  let calendar (calendar : Settlement.calendar) =
    `Assoc
      [
        ("calendar_id", string calendar.calendar_id);
        ("version", string calendar.version);
        ("business_dates", `List (List.map string calendar.business_dates));
      ]
  in
  let rule (rule : Settlement.rule) =
    `Assoc
      [
        ("instrument_id", instrument_id rule.instrument_id);
        ("calendar_id", string rule.calendar_id);
        ("lag_business_days", `Int rule.lag_business_days);
      ]
  in
  `Assoc
    [
      ( "cash_buying_power",
        string (Settlement.cash_buying_power_to_string policy.cash_buying_power)
      );
      ( "position_availability",
        string
          (Settlement.position_availability_to_string
             policy.position_availability) );
      ("calendars", `List (List.map calendar policy.calendars));
      ("rules", `List (List.map rule policy.rules));
    ]

let instrument_policy_to_yojson (policy : Risk.instrument_policy) =
  `Assoc
    [
      ("instrument_id", instrument_id policy.instrument_id);
      ("max_order_quantity", quantity policy.max_order_quantity);
      ("max_long_position", quantity policy.max_long_position);
      ("max_short_position", quantity policy.max_short_position);
      ("max_notional_exposure", nullable money policy.max_notional_exposure);
      ("initial_margin_bps", `Int policy.initial_margin_bps);
      ("maintenance_margin_bps", `Int policy.maintenance_margin_bps);
      ("shorting_allowed", `Bool policy.shorting_allowed);
    ]

let group_to_yojson (group : Risk.group) =
  let limits = group.limits in
  `Assoc
    [
      ("group_id", string (Id.Risk_group.to_string group.group_id));
      ("group_version", string "1");
      ("group_type", string (group_kind_to_string group.group_kind));
      ("instrument_ids", `List (List.map instrument_id group.instrument_ids));
      ( "limits",
        `Assoc
          [
            ("max_gross_exposure", nullable money limits.max_gross_exposure);
            ("max_long_exposure", nullable money limits.max_long_exposure);
            ("max_short_exposure", nullable money limits.max_short_exposure);
            ( "max_absolute_net_exposure",
              nullable money limits.max_absolute_net_exposure );
            ("max_concentration", nullable ratio limits.max_concentration);
          ] );
    ]

let risk_to_yojson ~protocol_version risk =
  if modern_protocol protocol_version then
    `Assoc
      [
        ("max_gross_exposure", money (Risk.max_gross_exposure risk));
        ("max_leverage", ratio (Risk.max_leverage risk));
        ("short_borrow_bps", `Int (Risk.short_borrow_bps risk));
        ( "instrument_policies",
          `List
            (List.map instrument_policy_to_yojson
               (Risk.instrument_policies risk)) );
        ("groups", `List (List.map group_to_yojson (Risk.groups risk)));
      ]
  else
    `Assoc
      [
        ("max_order_quantity", quantity (Risk.max_order_quantity risk));
        ("max_long_position", quantity (Risk.max_long_position risk));
        ("max_short_position", quantity (Risk.max_short_position risk));
        ("max_gross_exposure", money (Risk.max_gross_exposure risk));
        ("max_leverage", ratio (Risk.max_leverage risk));
        ("initial_margin_bps", `Int (Risk.initial_margin_bps risk));
        ("maintenance_margin_bps", `Int (Risk.maintenance_margin_bps risk));
        ("short_borrow_bps", `Int (Risk.short_borrow_bps risk));
      ]

let execution_to_yojson ~protocol_version model execution =
  let fee_component_to_yojson component =
    let value =
      match Fee_schedule.component_basis component with
      | Fee_schedule.Fixed value | Fee_schedule.Per_unit value -> money value
      | Fee_schedule.Notional_bps value -> `Int value
    in
    `Assoc
      [
        ("name", string (Fee_schedule.component_name component));
        ("currency", string (Fee_schedule.component_currency component));
        ( "kind",
          string
            (Fee_schedule.basis_kind (Fee_schedule.component_basis component))
        );
        ("value", value);
        ( "rounding",
          string
            (Fee_schedule.rounding_to_string
               (Fee_schedule.component_rounding component)) );
        ( "applies_to",
          string
            (Fee_schedule.applicability_to_string
               (Fee_schedule.component_applicability component)) );
      ]
  in
  let fee_schedule_to_yojson schedule =
    `Assoc
      [
        ("schedule_id", string (Fee_schedule.schedule_id schedule));
        ("instrument_id", instrument_id (Fee_schedule.instrument_id schedule));
        ( "settlement_currency",
          string (Fee_schedule.settlement_currency schedule) );
        ("minimum", nullable money (Fee_schedule.minimum schedule));
        ("maximum", nullable money (Fee_schedule.maximum schedule));
        ( "components",
          `List
            (List.map fee_component_to_yojson
               (Fee_schedule.components schedule)) );
      ]
  in
  if
    List.mem protocol_version [ "12"; "11" ]
    && List.mem
         (Execution_model.name model)
         [ "completed_bar_next_open_v1"; "completed_bar_adverse_touch_v1" ]
  then
    let costs = Execution.cost_model execution |> Option.get in
    `Assoc
      [
        ("model", string (Execution_model.name model));
        ( "configuration",
          `Assoc
            [
              ("version", string "1");
              ("participation_bps", `Int (Execution.participation_bps execution));
              ( "fee_schedules",
                `List
                  (List.map fee_schedule_to_yojson
                     (Execution.fee_schedules execution)) );
              ( "spread_model",
                `Assoc
                  [
                    ("model", string "fixed_half_spread_v1");
                    ("half_spread_bps", `Int costs.half_spread_bps);
                  ] );
              ( "impact_model",
                `Assoc
                  [
                    ("model", string "linear_participation_v1");
                    ("coefficient_bps", `Int costs.impact_coefficient_bps);
                    ( "missing_volume_policy",
                      string
                        (match costs.missing_volume_policy with
                        | Execution.Reject_missing_volume -> "reject"
                        | Zero_impact -> "zero_impact") );
                  ] );
            ] );
      ]
  else if
    String.equal protocol_version "12"
    && String.equal (Execution_model.name model) "quote_trade_v1"
  then
    `Assoc
      [
        ("model", string (Execution_model.name model));
        ( "configuration",
          `Assoc
            [
              ("version", string "1");
              ("participation_bps", `Int (Execution.participation_bps execution));
              ( "fee_schedules",
                `List
                  (List.map fee_schedule_to_yojson
                     (Execution.fee_schedules execution)) );
            ] );
      ]
  else if List.mem protocol_version [ "12"; "11"; "10"; "9"; "8"; "7" ] then
    `Assoc
      [
        ("model", string (Execution_model.name model));
        ( "configuration",
          `Assoc
            [
              ("version", string "2");
              ("participation_bps", `Int (Execution.participation_bps execution));
              ( "fee_schedules",
                `List
                  (List.map fee_schedule_to_yojson
                     (Execution.fee_schedules execution)) );
            ] );
      ]
  else if modern_protocol protocol_version then
    `Assoc
      [
        ("model", string (Execution_model.name model));
        ( "configuration",
          `Assoc
            [
              ("version", string "1");
              ("participation_bps", `Int (Execution.participation_bps execution));
              ("fixed_fee", money (Execution.fixed_fee execution));
              ("fee_bps", `Int (Execution.fee_bps execution));
            ] );
      ]
  else
    `Assoc
      [
        ("model", string (Execution_model.name model));
        ("participation_bps", `Int (Execution.participation_bps execution));
        ("fixed_fee", money (Execution.fixed_fee execution));
        ("fee_bps", `Int (Execution.fee_bps execution));
      ]

let protocol_version initialization =
  match initialization.scenario_contract_version with
  | "14" -> "12"
  | "13" -> "11"
  | "12" -> "10"
  | "11" -> "9"
  | "10" -> "8"
  | "9" -> "7"
  | "8" -> "6"
  | "7" -> "5"
  | "6" -> "4"
  | _ -> "3"

let initialize_message ~sequence:message_sequence initialization =
  let protocol_version = protocol_version initialization in
  let instruments =
    List.sort
      (fun left right ->
        Id.Instrument.compare left.Instrument.id right.Instrument.id)
      initialization.instruments
  in
  let initial_cash =
    List.sort
      (fun (left, _) (right, _) -> String.compare left right)
      initialization.initial_cash
  in
  let venue_calendars =
    List.sort
      (fun left right ->
        Id.Venue_calendar.compare left.Venue_calendar.id right.Venue_calendar.id)
      initialization.venue_calendars
  in
  let fields =
    [
      ("engine_version", string Contract.engine_version);
      ( "scenario_contract_version",
        string initialization.scenario_contract_version );
      ("scenario_sha256", string initialization.scenario_sha256);
      ("run_id", string (Id.Run.to_string initialization.run_id));
      ("base_currency", string initialization.base_currency);
      ("initial_cash", `List (List.map cash_balance_to_yojson initial_cash));
      ("instruments", `List (List.map instrument_to_yojson instruments));
      ("risk", risk_to_yojson ~protocol_version initialization.risk);
      ( "execution",
        execution_to_yojson ~protocol_version initialization.execution_model
          initialization.execution );
      ("metadata", initialization.metadata);
    ]
  in
  let fields =
    if List.mem protocol_version [ "12"; "11"; "10"; "9"; "8"; "7"; "6" ] then
      let initial_portfolio =
        Option.fold ~none:`Null ~some:Codec.initial_portfolio_to_yojson
          initialization.initial_portfolio
      in
      List.concat
        [
          List.take 6 fields;
          [
            ("initial_portfolio", initial_portfolio);
            ( "venue_calendars",
              `List (List.map venue_calendar_to_yojson venue_calendars) );
          ];
          (if List.mem protocol_version [ "12"; "11"; "10"; "9"; "8" ] then
             [
               ( "financing",
                 Option.fold ~none:`Null ~some:financing_to_yojson
                   initialization.financing );
             ]
           else []);
          (if List.mem protocol_version [ "12"; "11"; "10"; "9" ] then
             [
               ( "settlement",
                 Option.fold ~none:`Null ~some:settlement_to_yojson
                   initialization.settlement );
             ]
           else []);
          List.drop 6 fields;
        ]
    else if modern_protocol protocol_version then
      let initial_portfolio =
        Option.fold ~none:`Null ~some:Codec.initial_portfolio_to_yojson
          initialization.initial_portfolio
      in
      List.concat
        [
          List.take 6 fields;
          [ ("initial_portfolio", initial_portfolio) ];
          List.drop 6 fields;
        ]
    else fields
  in
  message ~protocol_version ~sequence:message_sequence
    ~message_type:"initialize" (`Assoc fields)

let cash_attribution_to_yojson ~protocol_version
    (balance : Account.cash_attribution) =
  `Assoc
    ([
       ("currency", string balance.currency);
       ("amount", money balance.amount);
       ("fx_rate", price balance.fx_rate);
       ("base_value", money balance.base_value);
     ]
    @ (if List.mem protocol_version [ "12"; "11"; "10"; "9"; "8" ] then
         [
           ("interest", money balance.interest);
           ("base_interest", money balance.base_interest);
         ]
       else [])
    @
    if List.mem protocol_version [ "12"; "11"; "10"; "9" ] then
      [
        ("settled_amount", money balance.settled_amount);
        ("unsettled_amount", money balance.unsettled_amount);
        ("base_settled_value", money balance.base_settled_value);
        ("base_unsettled_value", money balance.base_unsettled_value);
      ]
    else [])

let marked_position_to_yojson ~protocol_version
    (position : Strategy.marked_position) =
  `Assoc
    ([
       ("instrument_id", instrument_id position.instrument_id);
       ("quantity", quantity position.quantity);
       ("mark", price position.mark);
       ("base_market_value", money position.base_market_value);
       ("weight", Option.fold ~none:`Null ~some:weight position.weight);
     ]
    @
    if List.mem protocol_version [ "12"; "11"; "10"; "9" ] then
      [
        ("settled_quantity", quantity position.settled_quantity);
        ("unsettled_quantity", quantity position.unsettled_quantity);
      ]
    else [])

let group_exposure_to_yojson (exposure : Risk.group_exposure) =
  `Assoc
    [
      ("group_id", string (Id.Risk_group.to_string exposure.group_id));
      ("gross_exposure", money exposure.gross_exposure);
      ("net_exposure", money exposure.net_exposure);
      ("long_exposure", money exposure.long_exposure);
      ("short_exposure", money exposure.short_exposure);
      ( "concentration",
        Option.fold ~none:`Null ~some:weight exposure.concentration );
    ]

let context_to_yojson ~protocol_version context =
  let portfolio = Strategy.portfolio context in
  let cash_balances =
    List.sort
      (fun (left : Account.cash_attribution) right ->
        String.compare left.currency right.currency)
      portfolio.cash_balances
  in
  let positions =
    List.sort
      (fun (left : Strategy.marked_position) right ->
        Id.Instrument.compare left.instrument_id right.instrument_id)
      portfolio.positions
  in
  let working_orders =
    Strategy.working_orders context
    |> List.sort (fun left right ->
        Id.Order.compare left.Order.id right.Order.id)
  in
  let latest_bars =
    List.map
      (fun (position : Strategy.marked_position) ->
        Strategy.latest_bar context position.instrument_id)
      positions
    |> List.filter_map Fun.id
  in
  let portfolio_fields =
    [
      ("base_currency", string portfolio.base_currency);
      ("cash", money portfolio.cash);
      ("net_market_value", money portfolio.net_market_value);
      ("long_market_value", money portfolio.long_market_value);
      ("short_market_value", money portfolio.short_market_value);
      ("gross_exposure", money portfolio.gross_exposure);
      ("equity", money portfolio.equity);
      ("weights_available", `Bool (Option.is_some portfolio.cash_weight));
      ("cash_weight", Option.fold ~none:`Null ~some:weight portfolio.cash_weight);
      ( "cash_balances",
        `List
          (List.map
             (cash_attribution_to_yojson ~protocol_version)
             cash_balances) );
      ( "positions",
        `List (List.map (marked_position_to_yojson ~protocol_version) positions)
      );
    ]
  in
  let portfolio_fields =
    if modern_protocol protocol_version then
      portfolio_fields
      @ [
          ( "group_exposures",
            `List (List.map group_exposure_to_yojson portfolio.group_exposures)
          );
        ]
    else portfolio_fields
  in
  `Assoc
    [
      ("now", timestamp (Strategy.now context));
      ("portfolio", `Assoc portfolio_fields);
      ( "working_orders",
        `List
          (List.map
             (if
                List.mem protocol_version
                  [ "12"; "11"; "10"; "9"; "8"; "7"; "6" ]
              then Codec.order_to_yojson_v8
              else Codec.order_to_yojson)
             working_orders) );
      ("latest_bars", `List (List.map Codec.bar_to_yojson latest_bars));
    ]

let event_to_yojson ~protocol_version = function
  | Strategy.Market_slice_closed market_slice ->
      `Assoc
        [
          ("type", string "market_slice_closed");
          ( "market_slice",
            if String.equal protocol_version "12" then
              Codec.market_slice_to_yojson_v14 market_slice
            else if String.equal protocol_version "11" then
              Codec.market_slice_to_yojson_v13 market_slice
            else if String.equal protocol_version "10" then
              Codec.market_slice_to_yojson_v12 market_slice
            else if String.equal protocol_version "9" then
              Codec.market_slice_to_yojson_v11 market_slice
            else if String.equal protocol_version "8" then
              Codec.market_slice_to_yojson_v10 market_slice
            else Codec.market_slice_to_yojson market_slice );
        ]
  | Strategy.Fill_received fill ->
      `Assoc
        [
          ("type", string "fill_received");
          ( "fill",
            if List.mem protocol_version [ "12"; "11"; "10"; "9"; "8"; "7" ]
            then Codec.fill_to_yojson_v9 fill
            else Codec.fill_to_yojson fill );
        ]
  | Strategy.Order_updated order ->
      `Assoc
        [
          ("type", string "order_updated");
          ( "order",
            if
              List.mem protocol_version [ "12"; "11"; "10"; "9"; "8"; "7"; "6" ]
            then Codec.order_to_yojson_v8 order
            else Codec.order_to_yojson order );
        ]
  | Strategy.Intent_rejected reason ->
      `Assoc [ ("type", string "intent_rejected"); ("reason", string reason) ]

let event_message ?(protocol_version = version) ~sequence:message_sequence
    context event =
  message ~protocol_version ~sequence:message_sequence ~message_type:"event"
    (`Assoc
       [
         ("context", context_to_yojson ~protocol_version context);
         ("event", event_to_yojson ~protocol_version event);
       ])

let shutdown_message_for ~protocol_version ~sequence:message_sequence =
  message ~protocol_version ~sequence:message_sequence ~message_type:"shutdown"
    (`Assoc [])

let shutdown_message ~sequence =
  shutdown_message_for ~protocol_version:version ~sequence

let object_fields ~name ~expected = function
  | `Assoc fields ->
      let names = List.map fst fields in
      let unique = List.sort_uniq String.compare names in
      if List.length names <> List.length unique then
        Error (name ^ " must not contain duplicate fields")
      else
        let expected = List.sort String.compare expected in
        if unique <> expected then
          Error (name ^ " has unknown or missing fields")
        else Ok fields
  | _ -> Error (name ^ " must be a JSON object")

let field fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("missing field: " ^ name)

let required_string ~name = function
  | `String value when String.length value > 0 && String.trim value = value ->
      Ok value
  | `String _ -> Error (name ^ " must be a nonempty trimmed string")
  | _ -> Error (name ^ " must be a string")

let optional_string ~name = function
  | `Null -> Ok None
  | json -> required_string ~name json |> Result.map Option.some

let parse_error_payload json =
  let* fields =
    object_fields ~name:"strategy error payload" ~expected:[ "message" ] json
  in
  let* message = field fields "message" in
  required_string ~name:"strategy error message" message

let parse_ready_payload json =
  let* fields =
    object_fields ~name:"strategy ready payload"
      ~expected:[ "strategy_name"; "strategy_version" ]
      json
  in
  let* name_json = field fields "strategy_name" in
  let* name = required_string ~name:"strategy_name" name_json in
  let* name = Id.Strategy.of_string name in
  let* version_json = field fields "strategy_version" in
  let* version = optional_string ~name:"strategy_version" version_json in
  Ok (Ready { name; version })

let parse_intents_payload ~protocol_version json =
  let* fields =
    object_fields ~name:"strategy intents payload" ~expected:[ "intents" ] json
  in
  let* intents_json = field fields "intents" in
  match intents_json with
  | `List values when List.length values > Resource_limits.intents_per_batch ->
      Error
        (Printf.sprintf "intent count is %d; limit is %d" (List.length values)
           Resource_limits.intents_per_batch)
  | `List values ->
      List.fold_left
        (fun result value ->
          let* intents = result in
          let* intent =
            Scenario.intent_of_yojson
              ~contract_version:
                (if String.equal protocol_version "12" then "14"
                 else if String.equal protocol_version "11" then "13"
                 else if String.equal protocol_version "10" then "12"
                 else if String.equal protocol_version "9" then "11"
                 else if String.equal protocol_version "8" then "10"
                 else if String.equal protocol_version "7" then "9"
                 else if String.equal protocol_version "6" then "8"
                 else "7")
              value
            |> Result.map_error Diagnostic.to_human
          in
          Ok (intent :: intents))
        (Ok []) values
      |> Result.map (fun values -> Intents (List.rev values))
  | _ -> Error "strategy intents must be a JSON array"

let parse_stopped_payload json =
  let* _ = object_fields ~name:"strategy stopped payload" ~expected:[] json in
  Ok Stopped

let response_of_yojson_result ~protocol_version ~expected_sequence json =
  let* fields =
    object_fields ~name:"strategy response"
      ~expected:
        [
          "strategy_protocol_version";
          "strategy_sequence";
          "message_type";
          "payload";
        ]
      json
  in
  let* version_json = field fields "strategy_protocol_version" in
  let* supplied_version =
    required_string ~name:"strategy_protocol_version" version_json
  in
  if not (String.equal supplied_version protocol_version) then
    Error ("unsupported strategy protocol version: " ^ supplied_version)
  else
    let* sequence_json = field fields "strategy_sequence" in
    let* supplied_sequence =
      required_string ~name:"strategy_sequence" sequence_json
    in
    if not (String.equal supplied_sequence (Int64.to_string expected_sequence))
    then
      Error
        (Printf.sprintf "expected strategy sequence %Ld but received %s"
           expected_sequence supplied_sequence)
    else
      let* message_type_json = field fields "message_type" in
      let* message_type =
        required_string ~name:"message_type" message_type_json
      in
      let* payload = field fields "payload" in
      match message_type with
      | "ready" -> parse_ready_payload payload
      | "intents" -> parse_intents_payload ~protocol_version payload
      | "stopped" -> parse_stopped_payload payload
      | "error" ->
          parse_error_payload payload
          |> Result.map (fun message -> Failed message)
      | value -> Error ("unsupported strategy response type: " ^ value)

let response_of_yojson ?(protocol_version = version) ~expected_sequence json =
  let json_path =
    match json with
    | `Assoc fields -> (
        match List.assoc_opt "strategy_protocol_version" fields with
        | Some (`String supplied)
          when not (String.equal supplied protocol_version) ->
            "$.strategy_protocol_version"
        | _ -> (
            match List.assoc_opt "strategy_sequence" fields with
            | Some (`String supplied)
              when not
                     (String.equal supplied (Int64.to_string expected_sequence))
              ->
                "$.strategy_sequence"
            | _ -> "$"))
    | _ -> "$"
  in
  let intent_count =
    match json with
    | `Assoc fields -> (
        match List.assoc_opt "payload" fields with
        | Some (`Assoc payload_fields) -> (
            match List.assoc_opt "intents" payload_fields with
            | Some (`List values) -> Some (List.length values)
            | _ -> None)
        | _ -> None)
    | _ -> None
  in
  match intent_count with
  | Some observed when observed > Resource_limits.intents_per_batch ->
      Error
        (Diagnostic.make ~code:Diagnostic.Resource_limit
           ~phase:Diagnostic.Strategy ~sequence:expected_sequence
           ~json_path:"$.payload.intents"
           (Printf.sprintf "intent count is %d; limit is %d" observed
              Resource_limits.intents_per_batch))
  | _ ->
      response_of_yojson_result ~protocol_version ~expected_sequence json
      |> Result.map_error (fun message ->
          Diagnostic.make ~code:Diagnostic.Strategy_protocol
            ~phase:Diagnostic.Strategy ~sequence:expected_sequence ~json_path
            message)

let response_of_string ?(protocol_version = version) ~expected_sequence document
    =
  if String.length document > max_message_bytes then
    Error
      (Diagnostic.make ~code:Diagnostic.Resource_limit
         ~phase:Diagnostic.Strategy ~sequence:expected_sequence
         (Printf.sprintf "strategy message is %d bytes; limit is %d bytes"
            (String.length document) max_message_bytes))
  else
    try
      let json = Yojson.Safe.from_string document in
      response_of_yojson ~protocol_version ~expected_sequence json
      |> Result.map (fun response -> (response, json))
    with Yojson.Json_error message as exception_ ->
      Error
        (Diagnostic.of_exception ~code:Diagnostic.Strategy_protocol
           ~phase:Diagnostic.Strategy ~sequence:expected_sequence ~json_path:"$"
           ~message:("invalid strategy response JSON: " ^ message)
           exception_)

let direction_to_string = function
  | Engine_to_strategy -> "engine_to_strategy"
  | Strategy_to_engine -> "strategy_to_engine"

let transcript_record ~transcript_sequence ~direction ~message =
  let protocol_version =
    match message with
    | `Assoc fields -> (
        match List.assoc_opt "strategy_protocol_version" fields with
        | Some (`String value) -> value
        | _ -> version)
    | _ -> version
  in
  `Assoc
    [
      ("strategy_protocol_version", string protocol_version);
      ("transcript_sequence", sequence transcript_sequence);
      ("direction", string (direction_to_string direction));
      ("message", message);
    ]

let message_to_string message = Yojson.Safe.to_string message
