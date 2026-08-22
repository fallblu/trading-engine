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
let quantity value = string (Scalar.Quantity.to_decimal_string value)
let weight value = string (Scalar.Weight.to_decimal_string value)
let money value = string (Scalar.Money.to_decimal_string value)
let timestamp value = string (ptime_to_string value)
let instrument_id value = string (Id.Instrument.to_string value)
let order_id value = string (Id.Order.to_string value)
let fill_id value = string (Id.Fill.to_string value)

let fill_limit_to_yojson = function
  | Risk.Maximum_order_quantity value ->
      ( "max_order_quantity",
        `Assoc [ ("unit", string "quantity"); ("value", quantity value) ] )
  | Risk.Maximum_long_position value ->
      ( "max_long_position",
        `Assoc [ ("unit", string "quantity"); ("value", quantity value) ] )
  | Risk.Maximum_short_position value ->
      ( "max_short_position",
        `Assoc [ ("unit", string "quantity"); ("value", quantity value) ] )
  | Risk.Maximum_gross_exposure value ->
      ( "max_gross_exposure",
        `Assoc [ ("unit", string "money"); ("value", money value) ] )
  | Risk.Maximum_leverage value ->
      ( "max_leverage",
        `Assoc
          [
            ("unit", string "ratio");
            ("value", string (Scalar.Ratio.to_decimal_string value));
          ] )
  | Risk.Initial_margin value ->
      ( "initial_margin",
        `Assoc [ ("unit", string "basis_points"); ("value", `Int value) ] )
  | Risk.Instrument_maximum_long_position (id, value) ->
      ( "instrument_max_long_position",
        `Assoc
          [
            ("instrument_id", instrument_id id);
            ("unit", string "quantity");
            ("value", quantity value);
          ] )
  | Risk.Instrument_maximum_short_position (id, value) ->
      ( "instrument_max_short_position",
        `Assoc
          [
            ("instrument_id", instrument_id id);
            ("unit", string "quantity");
            ("value", quantity value);
          ] )
  | Risk.Instrument_maximum_notional (id, value) ->
      ( "instrument_max_notional_exposure",
        `Assoc
          [
            ("instrument_id", instrument_id id);
            ("unit", string "money");
            ("value", money value);
          ] )
  | Risk.Instrument_shorting_disabled id ->
      ( "instrument_shorting_disabled",
        `Assoc [ ("instrument_id", instrument_id id); ("value", `Bool false) ]
      )
  | Risk.Instrument_borrow_availability (id, value) ->
      ( "instrument_borrow_availability",
        `Assoc
          [
            ("instrument_id", instrument_id id);
            ("unit", string "quantity");
            ("value", quantity value);
          ] )
  | Risk.Settlement_cash_buying_power (currency, value) ->
      ( "settlement_cash_buying_power",
        `Assoc
          [
            ("currency", string currency);
            ("unit", string "money");
            ("value", money value);
          ] )
  | Risk.Settlement_position_availability (id, value) ->
      ( "settlement_position_availability",
        `Assoc
          [
            ("instrument_id", instrument_id id);
            ("unit", string "quantity");
            ("value", quantity value);
          ] )
  | Risk.Instrument_initial_margin (id, value) ->
      ( "instrument_initial_margin",
        `Assoc
          [
            ("instrument_id", instrument_id id);
            ("unit", string "basis_points");
            ("value", `Int value);
          ] )
  | Risk.Group_maximum_gross (id, value) ->
      ( "group_max_gross_exposure",
        `Assoc
          [
            ("group_id", string (Id.Risk_group.to_string id));
            ("unit", string "money");
            ("value", money value);
          ] )
  | Risk.Group_maximum_long (id, value) ->
      ( "group_max_long_exposure",
        `Assoc
          [
            ("group_id", string (Id.Risk_group.to_string id));
            ("unit", string "money");
            ("value", money value);
          ] )
  | Risk.Group_maximum_short (id, value) ->
      ( "group_max_short_exposure",
        `Assoc
          [
            ("group_id", string (Id.Risk_group.to_string id));
            ("unit", string "money");
            ("value", money value);
          ] )
  | Risk.Group_maximum_absolute_net (id, value) ->
      ( "group_max_absolute_net_exposure",
        `Assoc
          [
            ("group_id", string (Id.Risk_group.to_string id));
            ("unit", string "money");
            ("value", money value);
          ] )
  | Risk.Group_maximum_concentration (id, value) ->
      ( "group_max_concentration",
        `Assoc
          [
            ("group_id", string (Id.Risk_group.to_string id));
            ("unit", string "ratio");
            ("value", string (Scalar.Ratio.to_decimal_string value));
          ] )

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

let fx_mark_to_yojson mark =
  `Assoc
    [
      ("currency", string mark.Market_slice.currency); ("rate", price mark.rate);
    ]

let corporate_action_to_yojson action =
  let common =
    [
      ( "action_id",
        string (Id.Corporate_action.to_string action.Corporate_action.id) );
      ("instrument_id", instrument_id action.instrument_id);
    ]
  in
  match action.kind with
  | Corporate_action.Split { numerator; denominator } ->
      `Assoc
        ((("type", string "split") :: common)
        @ [ ("numerator", int64 numerator); ("denominator", int64 denominator) ]
        )
  | Corporate_action.Cash_dividend { amount_per_unit } ->
      `Assoc
        ((("type", string "cash_dividend") :: common)
        @ [ ("amount_per_unit", money amount_per_unit) ])

let borrow_observation_to_yojson observation =
  `Assoc
    [
      ("instrument_id", instrument_id observation.Financing.instrument_id);
      ("effective_at", timestamp observation.effective_at);
      ("available_quantity", quantity observation.available_quantity);
      ("annual_rate_bps", `Int observation.annual_rate_bps);
      ("recalled", `Bool observation.recalled);
    ]

let cash_rate_observation_to_yojson observation =
  `Assoc
    [
      ("currency", string observation.Financing.currency);
      ("effective_at", timestamp observation.effective_at);
      ("credit_rate_bps", `Int observation.credit_rate_bps);
      ("debit_rate_bps", `Int observation.debit_rate_bps);
    ]

let settlement_failure_to_yojson failure =
  `Assoc
    [
      ("instruction_id", string failure.Settlement.instruction_id);
      ("reason", string failure.reason);
    ]

let versioned_market_slice_to_yojson ~contract_version market_slice =
  `Assoc
    [
      ("slice_sequence", int64 market_slice.Market_slice.slice_sequence);
      ("start_at", timestamp market_slice.start_at);
      ("end_at", timestamp market_slice.end_at);
      ("available_at", timestamp market_slice.available_at);
      ("received_at", timestamp market_slice.received_at);
      ("bars", `List (List.map bar_to_yojson market_slice.bars));
      ("fx_rates", `List (List.map fx_mark_to_yojson market_slice.fx_rates));
      ( "corporate_actions",
        `List
          (List.map corporate_action_to_yojson market_slice.corporate_actions)
      );
    ]
  |> function
  | `Assoc fields when List.mem contract_version [ "11"; "10" ] ->
      let settlement =
        if String.equal contract_version "11" then
          [
            ( "settlement_failures",
              `List
                (List.map settlement_failure_to_yojson
                   market_slice.Market_slice.settlement_failures) );
          ]
        else []
      in
      `Assoc
        (fields
        @ [
            ( "borrow_observations",
              `List
                (List.map borrow_observation_to_yojson
                   market_slice.Market_slice.borrow_observations) );
            ( "cash_rate_observations",
              `List
                (List.map cash_rate_observation_to_yojson
                   market_slice.Market_slice.cash_rate_observations) );
          ]
        @ settlement)
  | json -> json

let market_slice_to_yojson market_slice =
  versioned_market_slice_to_yojson ~contract_version:"9" market_slice

let market_slice_to_yojson_v10 market_slice =
  versioned_market_slice_to_yojson ~contract_version:"10" market_slice

let market_slice_to_yojson_v11 market_slice =
  versioned_market_slice_to_yojson ~contract_version:"11" market_slice

let request_fields request =
  let kind, limit_price =
    match request.Order.kind with
    | Order.Market -> ("market", `Null)
    | Order.Limit value -> ("limit", price value)
    | Order.Stop value -> ("stop", price value)
    | Order.Stop_limit { limit_price; _ } -> ("stop_limit", price limit_price)
  in
  [
    ("instrument_id", instrument_id request.instrument_id);
    ("side", string (Order.side_to_string request.side));
    ("quantity", quantity request.quantity);
    ("order_kind", string kind);
    ("limit_price", limit_price);
    ("origin", string (Order.origin_to_string request.origin));
  ]

let request_fields_v8 request =
  let kind, trigger_price, limit_price =
    match request.Order.kind with
    | Order.Market -> ("market", `Null, `Null)
    | Order.Limit value -> ("limit", `Null, price value)
    | Order.Stop value -> ("stop", price value, `Null)
    | Order.Stop_limit { trigger_price; limit_price } ->
        ("stop_limit", price trigger_price, price limit_price)
  in
  let tif, venue_id, calendar_id, expires_at =
    match request.time_in_force with
    | Order.Gtc -> ("gtc", `Null, `Null, `Null)
    | Order.Ioc -> ("ioc", `Null, `Null, `Null)
    | Order.Fok -> ("fok", `Null, `Null, `Null)
    | Order.Day { venue_id; calendar_id } ->
        ( "day",
          string (Id.Venue.to_string venue_id),
          string (Id.Venue_calendar.to_string calendar_id),
          `Null )
    | Order.Gtd value -> ("gtd", `Null, `Null, timestamp value)
  in
  [
    ("instrument_id", instrument_id request.instrument_id);
    ("side", string (Order.side_to_string request.side));
    ("quantity", quantity request.quantity);
    ("order_kind", string kind);
    ("trigger_price", trigger_price);
    ("limit_price", limit_price);
    ("time_in_force", string tif);
    ("venue_id", venue_id);
    ("calendar_id", calendar_id);
    ("expires_at", expires_at);
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
        ("updated_event_id", string (Id.Event.to_string order.updated_event_id));
        ("created_sequence", int64 order.created_sequence);
        ("created_at", timestamp order.created_at);
        ( "eligible_after_slice_sequence",
          int64 order.eligible_after_slice_sequence );
        ("filled_quantity", quantity order.filled_quantity);
        ("filled_notional", money order.filled_notional);
        ("status", string (Order.status_to_string order.status));
        ("rejection_reason", rejection_reason);
      ])

let order_to_yojson_v8 order =
  let rejection_reason =
    match order.Order.status with
    | Order.Rejected reason -> string reason
    | _ -> `Null
  in
  let triggered_at, triggered_slice_sequence =
    match order.trigger_state with
    | Some (Order.Triggered { triggered_at; triggered_slice_sequence }) ->
        (timestamp triggered_at, int64 triggered_slice_sequence)
    | Some Order.Dormant | None -> (`Null, `Null)
  in
  `Assoc
    ((("order_id", order_id order.id) :: request_fields_v8 order.request)
    @ [
        ("created_event_id", string (Id.Event.to_string order.created_event_id));
        ("updated_event_id", string (Id.Event.to_string order.updated_event_id));
        ("created_sequence", int64 order.created_sequence);
        ("created_at", timestamp order.created_at);
        ( "eligible_after_slice_sequence",
          int64 order.eligible_after_slice_sequence );
        ("triggered_at", triggered_at);
        ("triggered_slice_sequence", triggered_slice_sequence);
        ("filled_quantity", quantity order.filled_quantity);
        ("filled_notional", money order.filled_notional);
        ("status", string (Order.status_to_string order.status));
        ("rejection_reason", rejection_reason);
      ])

let versioned_order_to_yojson ~contract_version order =
  if List.mem contract_version [ "11"; "10"; "9"; "8" ] then
    order_to_yojson_v8 order
  else order_to_yojson order

let fill_to_yojson fill =
  `Assoc
    [
      ("fill_id", fill_id fill.Fill.id);
      ("order_id", order_id fill.order_id);
      ("instrument_id", instrument_id fill.instrument_id);
      ("quote_currency", string fill.quote_currency);
      ("side", string (Order.side_to_string fill.side));
      ("quantity", quantity fill.quantity);
      ("price", price fill.price);
      ("notional", money fill.notional);
      ("fee", money fill.fee);
      ("executed_at", timestamp fill.executed_at);
      ("slice_sequence", int64 fill.slice_sequence);
    ]

let calculated_fee_component_to_yojson component =
  `Assoc
    [
      ("name", string component.Fee_schedule.name);
      ("kind", string component.kind);
      ("currency", string component.currency);
      ("amount", money component.amount);
      ("quote_amount", money component.quote_amount);
    ]

let fill_to_yojson_v9 fill =
  match fill_to_yojson fill with
  | `Assoc fields ->
      `Assoc
        (fields
        @ [
            ( "fee_components",
              `List
                (List.map calculated_fee_component_to_yojson
                   fill.Fill.fee_components) );
          ])
  | _ -> assert false

let initial_position_to_yojson (position : Initial_portfolio.position) =
  `Assoc
    [
      ("instrument_id", instrument_id position.instrument_id);
      ("quantity", quantity position.quantity);
      ("cost_basis", money position.cost_basis);
      ("realized_pnl", money position.realized_pnl);
      ("dividend_pnl", money position.dividend_pnl);
      ("execution_fees", money position.execution_fees);
      ("borrow_fees", money position.borrow_fees);
    ]

let initial_portfolio_to_yojson (portfolio : Initial_portfolio.t) =
  let cash =
    List.map
      (fun (currency, amount) ->
        `Assoc [ ("currency", string currency); ("amount", money amount) ])
      portfolio.cash
  in
  let marks =
    List.map
      (fun (id, value) ->
        `Assoc [ ("instrument_id", instrument_id id); ("price", price value) ])
      portfolio.marks
  in
  let fx_rates =
    List.map
      (fun (currency, rate) ->
        `Assoc [ ("currency", string currency); ("rate", price rate) ])
      portfolio.fx_rates
  in
  `Assoc
    [
      ("cash", `List cash);
      ( "positions",
        `List (List.map initial_position_to_yojson portfolio.positions) );
      ("marks", `List marks);
      ("fx_rates", `List fx_rates);
    ]

let position_attribution_to_yojson position =
  `Assoc
    [
      ("instrument_id", instrument_id position.Account.instrument_id);
      ("quote_currency", string position.quote_currency);
      ("quantity", quantity position.quantity);
      ("mark", price position.mark);
      ("fx_rate", price position.fx_rate);
      ("market_value", money position.market_value);
      ("base_market_value", money position.base_market_value);
      ("cost_basis", money position.cost_basis);
      ("base_cost_basis", money position.base_cost_basis);
      ("realized_pnl", money position.realized_pnl);
      ("base_realized_pnl", money position.base_realized_pnl);
      ("unrealized_pnl", money position.unrealized_pnl);
      ("base_unrealized_pnl", money position.base_unrealized_pnl);
      ("dividend_pnl", money position.dividend_pnl);
      ("base_dividend_pnl", money position.base_dividend_pnl);
      ("execution_fees", money position.execution_fees);
      ("base_execution_fees", money position.base_execution_fees);
      ("borrow_fees", money position.borrow_fees);
      ("base_borrow_fees", money position.base_borrow_fees);
      ("total_fees", money position.total_fees);
      ("base_total_fees", money position.base_total_fees);
    ]

let execution_fee_component_attribution_to_yojson component =
  `Assoc
    [
      ("name", string component.Account.name);
      ("kind", string component.kind);
      ("currency", string component.currency);
      ("amount", money component.amount);
      ("quote_currency", string component.quote_currency);
      ("quote_amount", money component.quote_amount);
      ("base_amount", money component.base_amount);
    ]

let position_attribution_to_yojson_v9 position =
  match position_attribution_to_yojson position with
  | `Assoc fields ->
      `Assoc
        (fields
        @ [
            ( "execution_fee_components",
              `List
                (List.map execution_fee_component_attribution_to_yojson
                   position.Account.execution_fee_components) );
          ])
  | _ -> assert false

let position_attribution_to_yojson_v11 position =
  match position_attribution_to_yojson_v9 position with
  | `Assoc fields ->
      `Assoc
        (fields
        @ [
            ("settled_quantity", quantity position.Account.settled_quantity);
            ("unsettled_quantity", quantity position.unsettled_quantity);
          ])
  | _ -> assert false

let cash_attribution_to_yojson cash =
  `Assoc
    [
      ("currency", string cash.Account.currency);
      ("amount", money cash.amount);
      ("fx_rate", price cash.fx_rate);
      ("base_value", money cash.base_value);
    ]

let cash_attribution_to_yojson_v10 cash =
  match cash_attribution_to_yojson cash with
  | `Assoc fields ->
      `Assoc
        (fields
        @ [
            ("interest", money cash.Account.interest);
            ("base_interest", money cash.base_interest);
          ])
  | _ -> assert false

let cash_attribution_to_yojson_v11 cash =
  match cash_attribution_to_yojson_v10 cash with
  | `Assoc fields ->
      `Assoc
        (fields
        @ [
            ("settled_amount", money cash.Account.settled_amount);
            ("unsettled_amount", money cash.unsettled_amount);
            ("base_settled_value", money cash.base_settled_value);
            ("base_unsettled_value", money cash.base_unsettled_value);
          ])
  | _ -> assert false

let account_valuation_to_yojson ?(contract_version = "8") valuation =
  `Assoc
    [
      ("base_currency", string valuation.Account.base_currency);
      ("cash", money valuation.Account.cash);
      ("net_market_value", money valuation.net_market_value);
      ("long_market_value", money valuation.long_market_value);
      ("short_market_value", money valuation.short_market_value);
      ("gross_exposure", money valuation.gross_exposure);
      ("cost_basis", money valuation.cost_basis);
      ("realized_pnl", money valuation.realized_pnl);
      ("unrealized_pnl", money valuation.unrealized_pnl);
      ("equity", money valuation.equity);
      ("dividend_pnl", money valuation.dividend_pnl);
      ("execution_fees", money valuation.execution_fees);
      ("borrow_fees", money valuation.borrow_fees);
      ("total_fees", money valuation.total_fees);
      ( "cash_balances",
        `List
          (List.map
             (if String.equal contract_version "11" then
                cash_attribution_to_yojson_v11
              else if String.equal contract_version "10" then
                cash_attribution_to_yojson_v10
              else cash_attribution_to_yojson)
             valuation.cash_balances) );
      ( "positions",
        `List
          (List.map
             (if String.equal contract_version "11" then
                position_attribution_to_yojson_v11
              else if
                String.equal contract_version "9"
                || String.equal contract_version "10"
              then position_attribution_to_yojson_v9
              else position_attribution_to_yojson)
             valuation.positions) );
    ]
  |> function
  | `Assoc fields when List.mem contract_version [ "11"; "10"; "9" ] ->
      let financing =
        if List.mem contract_version [ "11"; "10" ] then
          [ ("cash_interest", money valuation.Account.cash_interest) ]
        else []
      in
      let settlement =
        if String.equal contract_version "11" then
          [
            ("settled_cash", money valuation.Account.settled_cash);
            ("unsettled_cash", money valuation.unsettled_cash);
          ]
        else []
      in
      `Assoc
        (fields
        @ [
            ( "execution_fee_components",
              `List
                (List.map execution_fee_component_attribution_to_yojson
                   valuation.Account.execution_fee_components) );
          ]
        @ financing @ settlement)
  | json -> json

let margin_to_yojson margin =
  `Assoc
    [
      ("initial_requirement", money margin.Risk.initial_requirement);
      ("maintenance_requirement", money margin.maintenance_requirement);
      ("initial_excess", money margin.initial_excess);
      ("maintenance_excess", money margin.maintenance_excess);
      ("margin_call", `Bool margin.margin_call);
    ]

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

let valuation_to_yojson ~contract_version valuation =
  match
    account_valuation_to_yojson ~contract_version valuation.Audit.account
  with
  | `Assoc fields ->
      let fields = fields @ [ ("margin", margin_to_yojson valuation.margin) ] in
      let fields =
        if List.mem contract_version [ "11"; "10"; "9"; "8" ] then
          fields
          @ [
              ( "group_exposures",
                `List
                  (List.map group_exposure_to_yojson
                     valuation.margin.Risk.group_exposures) );
            ]
        else fields
      in
      `Assoc fields
  | _ -> assert false

let order_counts_to_yojson counts =
  `Assoc
    [
      ("total", `Int counts.Audit.total);
      ("active", `Int counts.active);
      ("filled", `Int counts.filled);
      ("rejected", `Int counts.rejected);
      ("cancelled", `Int counts.cancelled);
    ]

let settlement_instruction_to_yojson instruction =
  let settled_at, failed_at, failure_reason =
    match instruction.Settlement.status with
    | Settlement.Pending -> (`Null, `Null, `Null)
    | Settlement.Settled value -> (timestamp value, `Null, `Null)
    | Settlement.Failed { failed_at; reason } ->
        (`Null, timestamp failed_at, string reason)
  in
  `Assoc
    [
      ("instruction_id", string instruction.instruction_id);
      ("fill_id", string (Id.Fill.to_string instruction.fill_id));
      ("instrument_id", instrument_id instruction.instrument_id);
      ("currency", string instruction.currency);
      ("cash_movement", money instruction.cash_movement);
      ("position_movement", quantity instruction.position_movement);
      ("trade_date", string instruction.trade_date);
      ("due_date", string instruction.due_date);
      ("status", string (Settlement.status_to_string instruction.status));
      ("settled_at", settled_at);
      ("failed_at", failed_at);
      ("failure_reason", failure_reason);
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

let payload_to_yojson ~contract_version = function
  | Audit.Run_started { scenario_sha256; execution_model } ->
      `Assoc
        [
          ("scenario_sha256", string scenario_sha256);
          ("execution_model", string execution_model);
        ]
  | Audit.Initial_state { portfolio; valuation } ->
      `Assoc
        [
          ("portfolio", initial_portfolio_to_yojson portfolio);
          ("valuation", valuation_to_yojson ~contract_version valuation);
        ]
  | Audit.Market_slice_received market_slice ->
      versioned_market_slice_to_yojson ~contract_version market_slice
  | Audit.Target_portfolio_requested { basis; targets } ->
      `Assoc
        [
          ("basis", string (Audit.target_basis_to_string basis));
          ("targets", `List (List.map requested_target_to_yojson targets));
        ]
  | Audit.Order_accepted order | Audit.Order_rejected order ->
      versioned_order_to_yojson ~contract_version order
  | Audit.Order_triggered order ->
      versioned_order_to_yojson ~contract_version order
  | Audit.Order_cancelled { order; reason } ->
      `Assoc
        [
          ("order", versioned_order_to_yojson ~contract_version order);
          ("reason", string (Audit.cancellation_reason_to_string reason));
        ]
  | Audit.Split_applied { action; previous_quantity; adjusted_quantity } ->
      `Assoc
        [
          ("action", corporate_action_to_yojson action);
          ("previous_quantity", quantity previous_quantity);
          ("adjusted_quantity", quantity adjusted_quantity);
        ]
  | Audit.Cash_dividend_applied { action; quantity = held; cash_amount } ->
      `Assoc
        [
          ("action", corporate_action_to_yojson action);
          ("quantity", quantity held);
          ("cash_amount", money cash_amount);
        ]
  | Audit.Order_adjusted { order; action_id } ->
      `Assoc
        [
          ("order", versioned_order_to_yojson ~contract_version order);
          ("action_id", string (Id.Corporate_action.to_string action_id));
        ]
  | Audit.Fill_applied fill ->
      if List.mem contract_version [ "11"; "10"; "9" ] then
        fill_to_yojson_v9 fill
      else fill_to_yojson fill
  | Audit.Settlement_instruction_created instruction
  | Audit.Settlement_completed instruction
  | Audit.Settlement_failed instruction ->
      settlement_instruction_to_yojson instruction
  | Audit.Margin_limited
      {
        order_id = id;
        instrument_id = instrument;
        requested_quantity;
        permitted_quantity;
        price = fill_price;
      } ->
      `Assoc
        [
          ("order_id", order_id id);
          ("instrument_id", instrument_id instrument);
          ("requested_quantity", quantity requested_quantity);
          ("permitted_quantity", quantity permitted_quantity);
          ("price", price fill_price);
        ]
  | Audit.Fill_clipped
      {
        order_id = id;
        instrument_id = instrument;
        proposed_quantity;
        permitted_quantity;
        price = fill_price;
        limit;
      } ->
      let limiting_policy, threshold = fill_limit_to_yojson limit in
      `Assoc
        [
          ( "reason",
            `Assoc
              [
                ("version", string "1");
                ("policy", string limiting_policy);
                ("threshold", threshold);
              ] );
          ("order_id", order_id id);
          ("instrument_id", instrument_id instrument);
          ("proposed_quantity", quantity proposed_quantity);
          ("permitted_quantity", quantity permitted_quantity);
          ("price", price fill_price);
        ]
  | Audit.Borrow_fee_applied
      {
        instrument_id = instrument;
        quote_currency;
        short_quantity;
        reference_price;
        borrow_bps;
        period_start;
        period_end;
        fee;
      } ->
      `Assoc
        [
          ("instrument_id", instrument_id instrument);
          ("quote_currency", string quote_currency);
          ("short_quantity", quantity short_quantity);
          ("reference_price", price reference_price);
          ("borrow_bps", `Int borrow_bps);
          ("period_start", timestamp period_start);
          ("period_end", timestamp period_end);
          ("fee", money fee);
        ]
  | Audit.Borrow_charge_applied
      {
        observation;
        quote_currency;
        short_quantity;
        reference_price;
        day_count;
        compounding;
        period_start;
        period_end;
        amount;
      } ->
      `Assoc
        [
          ("observation", borrow_observation_to_yojson observation);
          ("quote_currency", string quote_currency);
          ("short_quantity", quantity short_quantity);
          ("reference_price", price reference_price);
          ("day_count", string (Financing.day_count_to_string day_count));
          ("compounding", string (Financing.compounding_to_string compounding));
          ("period_start", timestamp period_start);
          ("period_end", timestamp period_end);
          ("amount", money amount);
        ]
  | Audit.Borrow_recall_received
      { observation; short_quantity; close_out_quantity } ->
      `Assoc
        [
          ("observation", borrow_observation_to_yojson observation);
          ("short_quantity", quantity short_quantity);
          ("close_out_quantity", quantity close_out_quantity);
        ]
  | Audit.Cash_interest_applied
      {
        observation;
        opening_balance;
        applied_rate_bps;
        day_count;
        compounding;
        period_start;
        period_end;
        amount;
        closing_balance;
      } ->
      `Assoc
        [
          ("observation", cash_rate_observation_to_yojson observation);
          ("opening_balance", money opening_balance);
          ("applied_rate_bps", `Int applied_rate_bps);
          ("day_count", string (Financing.day_count_to_string day_count));
          ("compounding", string (Financing.compounding_to_string compounding));
          ("period_start", timestamp period_start);
          ("period_end", timestamp period_end);
          ("amount", money amount);
          ("closing_balance", money closing_balance);
        ]
  | Audit.Margin_call_triggered valuation | Audit.Margin_restored valuation ->
      valuation_to_yojson ~contract_version valuation
  | Audit.Intent_rejected reason -> `Assoc [ ("reason", string reason) ]
  | Audit.Metric_emitted { name; value } ->
      `Assoc [ ("name", string name); ("value", string value) ]
  | Audit.Valuation valuation -> valuation_to_yojson ~contract_version valuation
  | Audit.Run_completed
      { scenario_sha256; execution_model; valuation; order_counts } ->
      `Assoc
        [
          ("scenario_sha256", string scenario_sha256);
          ("execution_model", string execution_model);
          ("valuation", valuation_to_yojson ~contract_version valuation);
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
      ( "payload",
        payload_to_yojson ~contract_version:audit.contract_version audit.event
      );
    ]

let audit_to_string audit = Yojson.Safe.to_string (audit_to_yojson audit)
