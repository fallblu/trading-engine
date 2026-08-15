let ptime_to_string value = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 value

let ptime_of_string value =
  match Ptime.of_rfc3339 value |> Ptime.rfc3339_error_to_msg with
  | Ok (timestamp, _, _) -> Ok timestamp
  | Error (`Msg message) -> Error ("invalid RFC3339 timestamp: " ^ message)

let string value = `String value
let int64 value = `String (Int64.to_string value)
let price value = string (Scalar.Price.to_decimal_string value)
let quantity value = string (Scalar.Quantity.to_string value)
let money value = string (Scalar.Money.to_decimal_string value)
let timestamp value = string (ptime_to_string value)
let instrument_id value = string (Id.Instrument.to_string value)
let order_id value = string (Id.Order.to_string value)
let fill_id value = string (Id.Fill.to_string value)

let bar_to_yojson bar =
  `Assoc
    [
      ("source_sequence", int64 bar.Bar.source_sequence);
      ("instrument_id", instrument_id bar.instrument_id);
      ("start_at", timestamp bar.start_at);
      ("end_at", timestamp bar.end_at);
      ("available_at", timestamp bar.available_at);
      ("received_at", timestamp bar.received_at);
      ("open", price bar.open_price);
      ("high", price bar.high_price);
      ("low", price bar.low_price);
      ("close", price bar.close_price);
      ("volume", Option.fold ~none:`Null ~some:quantity bar.volume);
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
        ("created_sequence", int64 order.created_sequence);
        ("eligible_after_bar_sequence", int64 order.eligible_after_bar_sequence);
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
      ("bar_sequence", int64 fill.bar_sequence);
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
    ]

let payload_to_yojson = function
  | Audit.Bar_received bar -> bar_to_yojson bar
  | Audit.Target_requested { instrument_id = id; quantity = target } ->
      `Assoc
        [ ("instrument_id", instrument_id id); ("quantity", quantity target) ]
  | Audit.Order_accepted order | Audit.Order_rejected order ->
      order_to_yojson order
  | Audit.Order_cancelled { order; reason } ->
      `Assoc
        [
          ("order", order_to_yojson order);
          ("reason", string (Audit.cancellation_reason_to_string reason));
        ]
  | Audit.Fill_applied fill -> fill_to_yojson fill
  | Audit.Intent_rejected reason -> `Assoc [ ("reason", string reason) ]
  | Audit.Metric_emitted { name; value } ->
      `Assoc [ ("name", string name); ("value", string value) ]
  | Audit.Valuation valuation -> valuation_to_yojson valuation

let audit_to_yojson audit =
  `Assoc
    [
      ("schema_version", `Int audit.Audit.schema_version);
      ("engine_sequence", int64 audit.engine_sequence);
      ("run_id", string (Id.Run.to_string audit.run_id));
      ("recorded_at", timestamp audit.recorded_at);
      ("event_type", string (Audit.event_name audit.event));
      ("payload", payload_to_yojson audit.event);
    ]

let audit_to_string audit = Yojson.Safe.to_string (audit_to_yojson audit)
