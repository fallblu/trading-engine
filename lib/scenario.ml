type t = {
  schema_version : int;
  run_id : Id.Run.t;
  base_currency : string;
  initial_cash : Scalar.Money.t;
  instruments : Instrument.t list;
  risk : Risk.t;
  execution : Execution.t;
  max_internal_events : int;
  schedule : (int64 * Strategy.intent list) list;
  bars : Bar.t list;
}

module Int64_set = Set.Make (Int64)

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as e -> e

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

let parse_timestamp ~name json =
  let* value = string ~name json in
  Codec.ptime_of_string value

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

let parse_target_intent json =
  let* fields =
    object_fields ~name:"target_position intent"
      ~expected:[ "type"; "instrument_id"; "quantity" ]
      json
  in
  let* instrument_json = field fields "instrument_id" in
  let* instrument_id =
    parse_id Id.Instrument.of_string ~name:"instrument_id" instrument_json
  in
  let* quantity_json = field fields "quantity" in
  let* quantity = parse_quantity ~name:"quantity" quantity_json in
  Ok (Strategy.Target_position { instrument_id; quantity })

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
      | Some (`String "target_position") -> parse_target_intent json
      | Some (`String "submit_order") -> parse_submit_intent json
      | Some (`String "cancel_order") -> parse_cancel_intent json
      | Some (`String "emit_metric") -> parse_metric_intent json
      | Some _ -> Error "unsupported intent type"
      | None -> Error "intent is missing type")
  | _ -> Error "intent must be a JSON object"

let parse_schedule_item json =
  let* fields =
    object_fields ~name:"schedule item"
      ~expected:[ "after_bar_sequence"; "intents" ]
      json
  in
  let* sequence_json = field fields "after_bar_sequence" in
  let* sequence = parse_int64 ~name:"after_bar_sequence" sequence_json in
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
      ~expected:
        [
          "source_sequence";
          "instrument_id";
          "start_at";
          "end_at";
          "available_at";
          "received_at";
          "open";
          "high";
          "low";
          "close";
          "volume";
        ]
      json
  in
  let* sequence_json = field fields "source_sequence" in
  let* source_sequence = parse_int64 ~name:"source_sequence" sequence_json in
  let* instrument_json = field fields "instrument_id" in
  let* instrument_id =
    parse_id Id.Instrument.of_string ~name:"instrument_id" instrument_json
  in
  let* start_json = field fields "start_at" in
  let* start_at = parse_timestamp ~name:"start_at" start_json in
  let* end_json = field fields "end_at" in
  let* end_at = parse_timestamp ~name:"end_at" end_json in
  let* available_json = field fields "available_at" in
  let* available_at = parse_timestamp ~name:"available_at" available_json in
  let* received_json = field fields "received_at" in
  let* received_at = parse_timestamp ~name:"received_at" received_json in
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
  Bar.create ~source_sequence ~instrument_id ~start_at ~end_at ~available_at
    ~received_at ~open_price ~high_price ~low_price ~close_price ~volume

let validate_schedule schedule bars =
  let bar_sequences =
    List.fold_left
      (fun sequences bar -> Int64_set.add bar.Bar.source_sequence sequences)
      Int64_set.empty bars
  in
  let all_instruments =
    List.fold_left
      (fun instruments bar ->
        Id.Instrument.Set.add bar.Bar.instrument_id instruments)
      Id.Instrument.Set.empty bars
  in
  let bar_at sequence =
    List.find_opt (fun bar -> Int64.equal bar.Bar.source_sequence sequence) bars
  in
  let next_bar sequence instrument_id =
    let choose (current : Bar.t option) (bar : Bar.t) =
      if
        Id.Instrument.equal bar.Bar.instrument_id instrument_id
        && Int64.compare bar.source_sequence sequence > 0
      then
        match current with
        | None -> Some bar
        | Some selected
          when Int64.compare bar.source_sequence selected.Bar.source_sequence
               < 0 ->
            Some bar
        | Some _ -> current
      else current
    in
    List.fold_left choose None bars
  in
  let intent_instruments = function
    | Strategy.Target_position { instrument_id; _ } ->
        Id.Instrument.Set.singleton instrument_id
    | Strategy.Submit_order request ->
        Id.Instrument.Set.singleton request.Order.instrument_id
    | Strategy.Cancel_order _ -> all_instruments
    | Strategy.Emit_metric _ -> Id.Instrument.Set.empty
  in
  let validate_causal_start sequence anchor intents =
    let instruments =
      List.fold_left
        (fun instruments intent ->
          Id.Instrument.Set.union instruments (intent_instruments intent))
        Id.Instrument.Set.empty intents
    in
    Id.Instrument.Set.fold
      (fun instrument_id result ->
        let* () = result in
        match next_bar sequence instrument_id with
        | None -> Ok ()
        | Some bar ->
            if Ptime.compare anchor.Bar.received_at bar.start_at <= 0 then Ok ()
            else
              Error
                (Printf.sprintf
                   "scheduled order intent after bar %Ld is received after the \
                    next executable bar starts for instrument %s"
                   sequence
                   (Id.Instrument.to_string instrument_id)))
      instruments (Ok ())
  in
  let validate result (sequence, intents) =
    let* () = result in
    if Int64.compare sequence 0L < 0 then
      Error "scheduled bar sequence must be nonnegative"
    else if intents <> [] && not (Int64_set.mem sequence bar_sequences) then
      Error
        (Printf.sprintf
           "scheduled intents refer to missing bar source sequence %Ld" sequence)
    else
      match bar_at sequence with
      | None -> Ok ()
      | Some anchor -> validate_causal_start sequence anchor intents
  in
  List.fold_left validate (Ok ()) schedule

let of_yojson json =
  let* fields =
    object_fields ~name:"scenario"
      ~expected:
        [
          "schema_version";
          "run_id";
          "base_currency";
          "initial_cash";
          "instruments";
          "risk";
          "execution";
          "max_internal_events";
          "schedule";
          "bars";
        ]
      json
  in
  let* version_json = field fields "schema_version" in
  let* schema_version = integer ~name:"schema_version" version_json in
  if schema_version <> 1 then Error "unsupported scenario schema_version"
  else
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
          let* bars_json = field fields "bars" in
          let* bars_json = list ~name:"bars" bars_json in
          let* bars = map_list parse_bar bars_json in
          let* () = validate_schedule schedule bars in
          Ok
            {
              schema_version;
              run_id;
              base_currency;
              initial_cash;
              instruments;
              risk;
              execution;
              max_internal_events;
              schedule;
              bars;
            }

let of_string document =
  try Yojson.Safe.from_string document |> of_yojson
  with Yojson.Json_error message -> Error ("invalid scenario JSON: " ^ message)

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all |> of_string
  with Sys_error message -> Error ("could not read scenario: " ^ message)
