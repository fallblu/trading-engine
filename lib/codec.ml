let ptime_to_string value = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 value

let ptime_of_string value =
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
  | Audit.Run_started { scenario_sha256 } ->
      `Assoc [ ("scenario_sha256", string scenario_sha256) ]
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
  | Audit.Run_completed { scenario_sha256; valuation; order_counts } ->
      `Assoc
        [
          ("scenario_sha256", string scenario_sha256);
          ("valuation", valuation_to_yojson valuation);
          ("order_counts", order_counts_to_yojson order_counts);
        ]

let audit_to_yojson audit =
  `Assoc
    [
      ("engine_sequence", int64 audit.Audit.engine_sequence);
      ("run_id", string (Id.Run.to_string audit.run_id));
      ("recorded_at", timestamp audit.recorded_at);
      ("event_type", string (Audit.event_name audit.event));
      ("payload", payload_to_yojson audit.event);
    ]

let audit_to_string audit = Yojson.Safe.to_string (audit_to_yojson audit)
