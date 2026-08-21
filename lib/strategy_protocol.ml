let version = Contract.strategy_protocol_version
let max_message_bytes = 1_048_576

type initialization = {
  scenario_contract_version : string;
  scenario_sha256 : string;
  metadata : Yojson.Safe.t;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : (string * Scalar.Money.t) list;
  instruments : Instrument.t list;
  risk : Risk.t;
  execution_model : Execution_model.t;
  execution : Execution.t;
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

let message ~sequence:message_sequence ~message_type payload =
  `Assoc
    [
      ("strategy_protocol_version", string version);
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

let risk_to_yojson risk =
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

let execution_to_yojson model execution =
  `Assoc
    [
      ("model", string (Execution_model.name model));
      ("participation_bps", `Int (Execution.participation_bps execution));
      ("fixed_fee", money (Execution.fixed_fee execution));
      ("fee_bps", `Int (Execution.fee_bps execution));
    ]

let initialize_message ~sequence:message_sequence initialization =
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
  message ~sequence:message_sequence ~message_type:"initialize"
    (`Assoc
       [
         ("engine_version", string Contract.engine_version);
         ( "scenario_contract_version",
           string initialization.scenario_contract_version );
         ("scenario_sha256", string initialization.scenario_sha256);
         ("run_id", string (Id.Run.to_string initialization.run_id));
         ("base_currency", string initialization.base_currency);
         ("initial_cash", `List (List.map cash_balance_to_yojson initial_cash));
         ("instruments", `List (List.map instrument_to_yojson instruments));
         ("risk", risk_to_yojson initialization.risk);
         ( "execution",
           execution_to_yojson initialization.execution_model
             initialization.execution );
         ("metadata", initialization.metadata);
       ])

let cash_attribution_to_yojson (balance : Account.cash_attribution) =
  `Assoc
    [
      ("currency", string balance.currency);
      ("amount", money balance.amount);
      ("fx_rate", price balance.fx_rate);
      ("base_value", money balance.base_value);
    ]

let marked_position_to_yojson (position : Strategy.marked_position) =
  `Assoc
    [
      ("instrument_id", instrument_id position.instrument_id);
      ("quantity", quantity position.quantity);
      ("mark", price position.mark);
      ("base_market_value", money position.base_market_value);
      ("weight", Option.fold ~none:`Null ~some:weight position.weight);
    ]

let context_to_yojson context =
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
  `Assoc
    [
      ("now", timestamp (Strategy.now context));
      ( "portfolio",
        `Assoc
          [
            ("base_currency", string portfolio.base_currency);
            ("cash", money portfolio.cash);
            ("net_market_value", money portfolio.net_market_value);
            ("long_market_value", money portfolio.long_market_value);
            ("short_market_value", money portfolio.short_market_value);
            ("gross_exposure", money portfolio.gross_exposure);
            ("equity", money portfolio.equity);
            ("weights_available", `Bool (Option.is_some portfolio.cash_weight));
            ( "cash_weight",
              Option.fold ~none:`Null ~some:weight portfolio.cash_weight );
            ( "cash_balances",
              `List (List.map cash_attribution_to_yojson cash_balances) );
            ("positions", `List (List.map marked_position_to_yojson positions));
          ] );
      ("working_orders", `List (List.map Codec.order_to_yojson working_orders));
      ("latest_bars", `List (List.map Codec.bar_to_yojson latest_bars));
    ]

let event_to_yojson = function
  | Strategy.Market_slice_closed market_slice ->
      `Assoc
        [
          ("type", string "market_slice_closed");
          ("market_slice", Codec.market_slice_to_yojson market_slice);
        ]
  | Strategy.Fill_received fill ->
      `Assoc
        [
          ("type", string "fill_received"); ("fill", Codec.fill_to_yojson fill);
        ]
  | Strategy.Order_updated order ->
      `Assoc
        [
          ("type", string "order_updated");
          ("order", Codec.order_to_yojson order);
        ]
  | Strategy.Intent_rejected reason ->
      `Assoc [ ("type", string "intent_rejected"); ("reason", string reason) ]

let event_message ~sequence:message_sequence context event =
  message ~sequence:message_sequence ~message_type:"event"
    (`Assoc
       [
         ("context", context_to_yojson context); ("event", event_to_yojson event);
       ])

let shutdown_message ~sequence:message_sequence =
  message ~sequence:message_sequence ~message_type:"shutdown" (`Assoc [])

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

let parse_intents_payload json =
  let* fields =
    object_fields ~name:"strategy intents payload" ~expected:[ "intents" ] json
  in
  let* intents_json = field fields "intents" in
  match intents_json with
  | `List values ->
      List.fold_left
        (fun result value ->
          let* intents = result in
          let* intent =
            Scenario.intent_of_yojson value
            |> Result.map_error Diagnostic.to_human
          in
          Ok (intent :: intents))
        (Ok []) values
      |> Result.map (fun values -> Intents (List.rev values))
  | _ -> Error "strategy intents must be a JSON array"

let parse_stopped_payload json =
  let* _ = object_fields ~name:"strategy stopped payload" ~expected:[] json in
  Ok Stopped

let response_of_yojson_result ~expected_sequence json =
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
  if not (String.equal supplied_version version) then
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
      | "intents" -> parse_intents_payload payload
      | "stopped" -> parse_stopped_payload payload
      | "error" ->
          parse_error_payload payload
          |> Result.map (fun message -> Failed message)
      | value -> Error ("unsupported strategy response type: " ^ value)

let response_of_yojson ~expected_sequence json =
  let json_path =
    match json with
    | `Assoc fields -> (
        match List.assoc_opt "strategy_protocol_version" fields with
        | Some (`String supplied) when not (String.equal supplied version) ->
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
  response_of_yojson_result ~expected_sequence json
  |> Result.map_error (fun message ->
      Diagnostic.make ~code:Diagnostic.Strategy_protocol
        ~phase:Diagnostic.Strategy ~sequence:expected_sequence ~json_path
        message)

let response_of_string ~expected_sequence document =
  if String.length document > max_message_bytes then
    Error
      (Diagnostic.make ~code:Diagnostic.Strategy_protocol
         ~phase:Diagnostic.Strategy ~sequence:expected_sequence
         "strategy response exceeds the maximum message size")
  else
    try
      let json = Yojson.Safe.from_string document in
      response_of_yojson ~expected_sequence json
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
  `Assoc
    [
      ("strategy_protocol_version", string version);
      ("transcript_sequence", sequence transcript_sequence);
      ("direction", string (direction_to_string direction));
      ("message", message);
    ]

let message_to_string message = Yojson.Safe.to_string message
