let ptime_to_string value = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 value
let is_digit = function '0' .. '9' -> true | _ -> false

let digits value ~at ~count =
  let rec check index =
    if index = at + count then true
    else if index >= String.length value || not (is_digit value.[index]) then
      false
    else check (index + 1)
  in
  check at

let fractional_second_digits value =
  match String.index_opt value '.' with
  | None -> 0
  | Some separator ->
      let rec count index =
        if index >= String.length value then index - separator - 1
        else
          match value.[index] with
          | '0' .. '9' -> count (index + 1)
          | _ -> index - separator - 1
      in
      count (separator + 1)

let valid_zone value at =
  let length = String.length value in
  (at + 1 = length && (Char.equal value.[at] 'Z' || Char.equal value.[at] 'z'))
  || at + 6 = length
     && (Char.equal value.[at] '+' || Char.equal value.[at] '-')
     && digits value ~at:(at + 1) ~count:2
     && Char.equal value.[at + 3] ':'
     && digits value ~at:(at + 4) ~count:2

let valid_rfc3339_lexeme value =
  let length = String.length value in
  let valid_prefix =
    length >= 20
    && digits value ~at:0 ~count:4
    && Char.equal value.[4] '-'
    && digits value ~at:5 ~count:2
    && Char.equal value.[7] '-'
    && digits value ~at:8 ~count:2
    && (Char.equal value.[10] 'T' || Char.equal value.[10] 't')
    && digits value ~at:11 ~count:2
    && Char.equal value.[13] ':'
    && digits value ~at:14 ~count:2
    && Char.equal value.[16] ':'
    && value.[17] >= '0'
    && value.[17] <= '5'
    && is_digit value.[18]
  in
  if not valid_prefix then false
  else if Char.equal value.[19] '.' then
    let rec fraction_end index =
      if index < length && is_digit value.[index] then fraction_end (index + 1)
      else index
    in
    let zone_at = fraction_end 20 in
    let fraction_length = zone_at - 20 in
    fraction_length >= 1 && fraction_length <= 6 && valid_zone value zone_at
  else valid_zone value 19

let ptime_of_string value =
  if fractional_second_digits value > 6 then
    Error "RFC3339 timestamp must not exceed microsecond precision"
  else if not (valid_rfc3339_lexeme value) then
    Error "RFC3339 timestamp must use T/t and Z/z or a colonized offset"
  else
    match Ptime.of_rfc3339 value |> Ptime.rfc3339_error_to_msg with
    | Ok (timestamp, _, _) -> Ok timestamp
    | Error (`Msg message) -> Error ("invalid RFC3339 timestamp: " ^ message)

let string value = `String value
let int64 value = `String (Int64.to_string value)
let price value = string (Scalar.Price.to_decimal_string value)
let quantity value = string (Scalar.Quantity.to_string value)
let weight value = string (Scalar.Weight.to_decimal_string value)
let money value = string (Scalar.Money.to_decimal_string value)
let timestamp value = string (ptime_to_string value)
let instrument_id value = string (Id.Instrument.to_string value)
let order_id value = string (Id.Order.to_string value)
let fill_id value = string (Id.Fill.to_string value)

let bar_to_yojson bar =
  `Assoc
    [
      ("instrument_id", instrument_id bar.Bar.instrument_id);
      ("open", price bar.open_price);
      ("high", price bar.high_price);
      ("low", price bar.low_price);
      ("close", price bar.close_price);
      ("volume", Option.fold ~none:`Null ~some:quantity bar.volume);
    ]

let market_slice_to_yojson market_slice =
  `Assoc
    [
      ("slice_sequence", int64 market_slice.Market_slice.slice_sequence);
      ("start_at", timestamp market_slice.start_at);
      ("end_at", timestamp market_slice.end_at);
      ("available_at", timestamp market_slice.available_at);
      ("received_at", timestamp market_slice.received_at);
      ("bars", `List (List.map bar_to_yojson market_slice.bars));
    ]

let request_fields request =
  let kind, limit_price =
    match request.Order.kind with
    | Order.Market -> ("market", `Null)
    | Order.Limit value -> ("limit", price value)
  in
  [
    ("instrument_id", instrument_id request.instrument_id);
    ("side", string (Order.side_to_string request.side));
    ("quantity", quantity request.quantity);
    ("order_kind", string kind);
    ("limit_price", limit_price);
    ("origin", string (Order.origin_to_string request.origin));
  ]

let order_to_yojson order =
  let rejection_reason =
    match order.Order.status with
    | Order.Rejected reason -> string reason
    | _ -> `Null
  in
  `Assoc
    ((("order_id", order_id order.id) :: request_fields order.request)
    @ [
        ("created_event_id", string (Id.Event.to_string order.created_event_id));
        ("created_sequence", int64 order.created_sequence);
        ("created_at", timestamp order.created_at);
        ( "eligible_after_slice_sequence",
          int64 order.eligible_after_slice_sequence );
        ("filled_quantity", quantity order.filled_quantity);
        ("filled_notional", money order.filled_notional);
        ("status", string (Order.status_to_string order.status));
        ("rejection_reason", rejection_reason);
      ])

let fill_to_yojson fill =
  `Assoc
    [
      ("fill_id", fill_id fill.Fill.id);
      ("order_id", order_id fill.order_id);
      ("instrument_id", instrument_id fill.instrument_id);
      ("side", string (Order.side_to_string fill.side));
      ("quantity", quantity fill.quantity);
      ("price", price fill.price);
      ("notional", money fill.notional);
      ("fee", money fill.fee);
      ("executed_at", timestamp fill.executed_at);
      ("slice_sequence", int64 fill.slice_sequence);
    ]

let position_attribution_to_yojson position =
  `Assoc
    [
      ("instrument_id", instrument_id position.Account.instrument_id);
      ("quantity", quantity position.quantity);
      ("mark", price position.mark);
      ("market_value", money position.market_value);
      ("cost_basis", money position.cost_basis);
      ("realized_pnl", money position.realized_pnl);
      ("unrealized_pnl", money position.unrealized_pnl);
      ("total_fees", money position.total_fees);
    ]

let valuation_to_yojson valuation =
  `Assoc
    [
      ("cash", money valuation.Account.cash);
      ("market_value", money valuation.market_value);
      ("cost_basis", money valuation.cost_basis);
      ("realized_pnl", money valuation.realized_pnl);
      ("unrealized_pnl", money valuation.unrealized_pnl);
      ("equity", money valuation.equity);
      ("total_fees", money valuation.total_fees);
      ( "positions",
        `List (List.map position_attribution_to_yojson valuation.positions) );
    ]

let order_counts_to_yojson counts =
  `Assoc
    [
      ("total", `Int counts.Audit.total);
      ("active", `Int counts.active);
      ("filled", `Int counts.filled);
      ("rejected", `Int counts.rejected);
      ("cancelled", `Int counts.cancelled);
    ]

let requested_target_to_yojson target =
  `Assoc
    [
      ("instrument_id", instrument_id target.Audit.instrument_id);
      ("weight", Option.fold ~none:`Null ~some:weight target.weight);
      ("quantity", quantity target.quantity);
      ( "reference_price",
        Option.fold ~none:`Null ~some:price target.reference_price );
    ]

let payload_to_yojson = function
  | Audit.Run_started { scenario_sha256; execution_model } ->
      `Assoc
        [
          ("scenario_sha256", string scenario_sha256);
          ("execution_model", string execution_model);
        ]
  | Audit.Market_slice_received market_slice ->
      market_slice_to_yojson market_slice
  | Audit.Target_portfolio_requested { basis; targets } ->
      `Assoc
        [
          ("basis", string (Audit.target_basis_to_string basis));
          ("targets", `List (List.map requested_target_to_yojson targets));
        ]
  | Audit.Order_accepted order | Audit.Order_rejected order ->
      order_to_yojson order
  | Audit.Order_cancelled { order; reason } ->
      `Assoc
        [
          ("order", order_to_yojson order);
          ("reason", string (Audit.cancellation_reason_to_string reason));
        ]
  | Audit.Fill_applied fill -> fill_to_yojson fill
  | Audit.Cash_limited
      {
        order_id = id;
        instrument_id = instrument;
        requested_quantity;
        affordable_quantity;
        price = fill_price;
      } ->
      `Assoc
        [
          ("order_id", order_id id);
          ("instrument_id", instrument_id instrument);
          ("requested_quantity", quantity requested_quantity);
          ("affordable_quantity", quantity affordable_quantity);
          ("price", price fill_price);
        ]
  | Audit.Intent_rejected reason -> `Assoc [ ("reason", string reason) ]
  | Audit.Metric_emitted { name; value } ->
      `Assoc [ ("name", string name); ("value", string value) ]
  | Audit.Valuation valuation -> valuation_to_yojson valuation
  | Audit.Run_completed
      { scenario_sha256; execution_model; valuation; order_counts } ->
      `Assoc
        [
          ("scenario_sha256", string scenario_sha256);
          ("execution_model", string execution_model);
          ("valuation", valuation_to_yojson valuation);
          ("order_counts", order_counts_to_yojson order_counts);
        ]

let audit_to_yojson audit =
  `Assoc
    [
      ("contract_version", string audit.Audit.contract_version);
      ("engine_sequence", int64 audit.Audit.engine_sequence);
      ("event_id", string (Id.Event.to_string audit.Audit.event_id));
      ( "causation_ids",
        `List
          (List.map
             (fun value -> string (Id.Event.to_string value))
             audit.Audit.causation_ids) );
      ("run_id", string (Id.Run.to_string audit.run_id));
      ("recorded_at", timestamp audit.recorded_at);
      ("event_type", string (Audit.event_name audit.event));
      ("payload", payload_to_yojson audit.event);
    ]

let audit_to_string audit = Yojson.Safe.to_string (audit_to_yojson audit)
