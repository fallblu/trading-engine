open Test_support
module T = Trading_engine

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

type asset = Primary | Foreign

type command =
  | Submit of {
      asset : asset;
      side : T.Order.side;
      quantity : int;
      limit : int option;
    }
  | Cancel_working of asset
  | Target_quantities of { primary : int; foreign : int }
  | Target_weights of { primary_bps : int; foreign_bps : int }
  | Emit_metric of int

type dividend = { asset : asset; cents : int }

type step = {
  primary_price : int;
  foreign_price : int;
  euro_rate_bps : int;
  primary_volume : int;
  foreign_volume : int;
  dividend : dividend option;
  commands : command list;
}

type trace = {
  leverage_tenths : int;
  initial_margin_bps : int;
  maintenance_margin_bps : int;
  participation_bps : int;
  split_asset : asset option;
  steps : step list;
}

let primary = instrument ~id:"property-primary" ~symbol:"PRIMARY" ()

let foreign =
  instrument ~id:"property-foreign" ~symbol:"FOREIGN" ~currency:"EUR" ()

let instrument_for_asset = function Primary -> primary | Foreign -> foreign
let asset_name = function Primary -> "primary" | Foreign -> "foreign"
let side_name = function T.Order.Buy -> "buy" | T.Order.Sell -> "sell"

let decimal_of_scaled value scale =
  let sign = if value < 0 then "-" else "" in
  let magnitude = abs value in
  let whole = magnitude / scale in
  let remainder = magnitude mod scale in
  let digits = String.length (string_of_int (scale - 1)) in
  let fraction = Printf.sprintf "%0*d" digits remainder in
  let rec trim index =
    if index < 0 then ""
    else if Char.equal fraction.[index] '0' then trim (index - 1)
    else String.sub fraction 0 (index + 1)
  in
  match trim (String.length fraction - 1) with
  | "" -> Printf.sprintf "%s%d" sign whole
  | fraction -> Printf.sprintf "%s%d.%s" sign whole fraction

let json_of_asset asset = `String (asset_name asset)

let json_of_command = function
  | Submit { asset; side; quantity; limit } ->
      `Assoc
        [
          ("kind", `String "submit_order");
          ("asset", json_of_asset asset);
          ("side", `String (side_name side));
          ("quantity", `Int quantity);
          ( "limit",
            Option.fold ~none:`Null ~some:(fun value -> `Int value) limit );
        ]
  | Cancel_working asset ->
      `Assoc
        [ ("kind", `String "cancel_working"); ("asset", json_of_asset asset) ]
  | Target_quantities { primary; foreign } ->
      `Assoc
        [
          ("kind", `String "target_quantities");
          ("primary", `Int primary);
          ("foreign", `Int foreign);
        ]
  | Target_weights { primary_bps; foreign_bps } ->
      `Assoc
        [
          ("kind", `String "target_weights");
          ("primary_bps", `Int primary_bps);
          ("foreign_bps", `Int foreign_bps);
        ]
  | Emit_metric value ->
      `Assoc [ ("kind", `String "emit_metric"); ("value", `Int value) ]

let json_of_step sequence step =
  let dividend =
    Option.fold step.dividend ~none:`Null ~some:(fun dividend ->
        `Assoc
          [
            ("asset", json_of_asset dividend.asset);
            ("cents", `Int dividend.cents);
          ])
  in
  `Assoc
    [
      ("slice_sequence", `Int sequence);
      ("primary_price", `Int step.primary_price);
      ("foreign_price", `Int step.foreign_price);
      ("euro_rate_bps", `Int step.euro_rate_bps);
      ("primary_volume", `Int step.primary_volume);
      ("foreign_volume", `Int step.foreign_volume);
      ("dividend", dividend);
      ("commands", `List (List.map json_of_command step.commands));
    ]

let print_trace trace =
  let steps = List.mapi (fun index -> json_of_step (index + 1)) trace.steps in
  `Assoc
    [
      ("contract_version", `String T.Contract.version);
      ("leverage_tenths", `Int trace.leverage_tenths);
      ("initial_margin_bps", `Int trace.initial_margin_bps);
      ("maintenance_margin_bps", `Int trace.maintenance_margin_bps);
      ("participation_bps", `Int trace.participation_bps);
      ( "split_on_slice_four",
        Option.fold trace.split_asset ~none:`Null ~some:json_of_asset );
      ("steps", `List steps);
    ]
  |> Yojson.Safe.pretty_to_string

let gen_asset =
  QCheck2.Gen.map
    (fun foreign -> if foreign then Foreign else Primary)
    QCheck2.Gen.bool

let gen_side =
  QCheck2.Gen.map
    (fun sell -> if sell then T.Order.Sell else T.Order.Buy)
    QCheck2.Gen.bool

let gen_command =
  let open QCheck2.Gen in
  oneof_weighted
    [
      ( 7,
        map
          (fun (asset, side, quantity, limit) ->
            Submit { asset; side; quantity; limit })
          (quad gen_asset gen_side (int_range 1 50)
             (option ~ratio:0.45 (int_range 10 240))) );
      (2, map (fun asset -> Cancel_working asset) gen_asset);
      ( 4,
        map
          (fun (primary, foreign) -> Target_quantities { primary; foreign })
          (pair (int_range (-100) 100) (int_range (-100) 100)) );
      ( 2,
        map
          (fun (primary_bps, foreign_bps) ->
            Target_weights { primary_bps; foreign_bps })
          (pair (int_range (-12_500) 12_500) (int_range (-12_500) 12_500)) );
      (1, map (fun value -> Emit_metric value) (int_range (-1000) 1000));
    ]

let gen_dividend =
  let open QCheck2.Gen in
  option ~ratio:0.25
    (map
       (fun (asset, cents) -> { asset; cents })
       (pair gen_asset (int_range 1 250)))

let gen_step =
  let open QCheck2.Gen in
  map
    (fun ( (primary_price, foreign_price, euro_rate_bps, primary_volume),
           (foreign_volume, dividend, commands) ) ->
      {
        primary_price;
        foreign_price;
        euro_rate_bps;
        primary_volume;
        foreign_volume;
        dividend;
        commands;
      })
    (pair
       (quad (int_range 20 220) (int_range 20 220) (int_range 4_000 20_000)
          (int_range 1 80))
       (triple (int_range 1 80) gen_dividend
          (list_size (int_range 0 3) gen_command)))

let gen_trace =
  let open QCheck2.Gen in
  bind (int_range 2_500 10_000) (fun initial_margin_bps ->
      map
        (fun ( ( leverage_tenths,
                 maintenance_margin_bps,
                 participation_bps,
                 split_asset ),
               steps ) ->
          {
            leverage_tenths;
            initial_margin_bps;
            maintenance_margin_bps;
            participation_bps;
            split_asset;
            steps;
          })
        (pair
           (quad (int_range 10 30)
              (int_range 1_000 initial_margin_bps)
              (int_range 2_500 10_000)
              (option ~ratio:0.5 gen_asset))
           (list_size (int_range 4 14) gen_step)))

let make_risk trace =
  T.Risk.create ~base_currency:"USD" ~instruments:[ primary; foreign ]
    ~max_order_quantity:(quantity "50") ~max_long_position:(quantity "100")
    ~max_short_position:(quantity "100") ~max_gross_exposure:(money "50000")
    ~max_leverage:
      (T.Scalar.Ratio.of_decimal_string
         (decimal_of_scaled trace.leverage_tenths 10)
      |> ok)
    ~initial_margin_bps:trace.initial_margin_bps
    ~maintenance_margin_bps:trace.maintenance_margin_bps ~short_borrow_bps:250
  |> ok

let make_config trace risk =
  engine_config ~risk
    ~execution:
      (execution ~participation_bps:trace.participation_bps ~fixed_fee:"0.25"
         ~fee_bps:5 ())
    ~max_internal_events:5000 ()

let initial_cash = [ ("USD", money "10000"); ("EUR", money "5000") ]

let permitted_submit_quantity context asset side requested =
  let instrument_id = (instrument_for_asset asset).T.Instrument.id in
  let has_working_order =
    T.Strategy.working_orders context
    |> List.exists (fun order ->
        T.Id.Instrument.equal order.T.Order.request.instrument_id instrument_id)
  in
  if has_working_order then None
  else
    let current = T.Strategy.position context instrument_id in
    let current_micros = T.Scalar.Quantity.to_micros current in
    let crosses_current =
      (Int64.compare current_micros 0L > 0 && side = T.Order.Sell)
      || (Int64.compare current_micros 0L < 0 && side = T.Order.Buy)
    in
    let permitted =
      if not crosses_current then requested
      else
        min requested
          (Int64.div (Int64.abs current_micros) T.Scalar.Quantity.scale
          |> Int64.to_int)
    in
    if permitted = 0 then None else Some permitted

let command_intents context command =
  match command with
  | Submit { asset; side; quantity = quantity_value; limit } ->
      permitted_submit_quantity context asset side quantity_value
      |> Option.fold ~none:[] ~some:(fun quantity_value ->
          let instrument = (instrument_for_asset asset).T.Instrument.id in
          let kind =
            Option.fold limit ~none:T.Order.Market ~some:(fun value ->
                T.Order.Limit (price (string_of_int value)))
          in
          let request =
            request ~instrument ~side
              ~quantity_value:(string_of_int quantity_value)
              ~kind ()
          in
          [ T.Strategy.Submit_order request ])
  | Cancel_working asset ->
      let instrument_id = (instrument_for_asset asset).id in
      T.Strategy.working_orders context
      |> List.find_opt (fun order ->
          T.Id.Instrument.equal order.T.Order.request.instrument_id
            instrument_id)
      |> Option.fold ~none:[] ~some:(fun order ->
          [ T.Strategy.Cancel_order order.T.Order.id ])
  | Target_quantities { primary = primary_value; foreign = foreign_value } ->
      [
        T.Strategy.Target_quantities
          [
            T.Strategy.
              {
                instrument_id = primary.id;
                quantity = quantity (string_of_int primary_value);
              };
            T.Strategy.
              {
                instrument_id = foreign.id;
                quantity = quantity (string_of_int foreign_value);
              };
          ];
      ]
  | Target_weights { primary_bps; foreign_bps } ->
      [
        T.Strategy.Target_weights
          [
            T.Strategy.
              {
                instrument_id = primary.id;
                weight = weight (decimal_of_scaled primary_bps 10_000);
              };
            T.Strategy.
              {
                instrument_id = foreign.id;
                weight = weight (decimal_of_scaled foreign_bps 10_000);
              };
          ];
      ]
  | Emit_metric value ->
      [
        T.Strategy.Emit_metric
          { name = "generated.reducer.metric"; value = string_of_int value };
      ]

module Asset_set = Set.Make (struct
  type t = asset

  let compare = Stdlib.compare
end)

let commands_to_intents context commands =
  List.fold_left
    (fun (intents, submitted_assets) command ->
      match command with
      | Submit { asset; _ } when Asset_set.mem asset submitted_assets ->
          (intents, submitted_assets)
      | Submit { asset; _ } ->
          ( intents @ command_intents context command,
            Asset_set.add asset submitted_assets )
      | _ -> (intents @ command_intents context command, submitted_assets))
    ([], Asset_set.empty) commands
  |> fst

module Generated_strategy = struct
  type state = (int64 * command list) list

  let name = "generated-script"

  let take sequence schedule =
    let rec loop reversed = function
      | [] -> ([], List.rev reversed)
      | (candidate, commands) :: rest when Int64.equal sequence candidate ->
          (commands, List.rev_append reversed rest)
      | item :: rest -> loop (item :: reversed) rest
    in
    loop [] schedule

  let on_event state context = function
    | T.Strategy.Market_slice_closed slice ->
        let commands, state = take slice.T.Market_slice.slice_sequence state in
        (state, commands_to_intents context commands)
    | T.Strategy.Fill_received _ | T.Strategy.Order_updated _
    | T.Strategy.Intent_rejected _ ->
        (state, [])
end

module Generated_runner = T.Engine.Make (Generated_strategy)

let schedule trace =
  List.mapi
    (fun index step -> (Int64.of_int (index + 1), step.commands))
    trace.steps

let corporate_actions trace index step =
  let dividend =
    Option.to_list step.dividend
    |> List.map (fun dividend ->
        let instrument = instrument_for_asset dividend.asset in
        T.Corporate_action.cash_dividend
          ~id:
            (T.Id.Corporate_action.of_string_exn
               (Printf.sprintf "property-dividend-%d" index))
          ~instrument_id:instrument.id
          ~amount_per_unit:(money (decimal_of_scaled dividend.cents 100))
        |> ok)
  in
  match (index, trace.split_asset) with
  | 4, Some asset ->
      let instrument = instrument_for_asset asset in
      T.Corporate_action.split
        ~id:(T.Id.Corporate_action.of_string_exn "property-split")
        ~instrument_id:instrument.id ~numerator:2L ~denominator:1L
      |> ok
      |> fun split -> split :: dividend
  | _ -> dividend

let make_bar instrument close_value volume_value =
  let low_value = max 1 (close_value - 3) in
  T.Bar.create ~instrument_id:instrument.T.Instrument.id
    ~open_price:(price (string_of_int close_value))
    ~high_price:(price (string_of_int (close_value + 3)))
    ~low_price:(price (string_of_int low_value))
    ~close_price:(price (string_of_int close_value))
    ~volume:(Some (quantity (string_of_int volume_value)))
  |> ok

let make_slice trace index step =
  market_slice
    ~bars:
      [
        make_bar primary step.primary_price step.primary_volume;
        make_bar foreign step.foreign_price step.foreign_volume;
      ]
    ~fx_rates:
      [
        fx_mark ();
        fx_mark ~currency:"EUR"
          ~rate:(decimal_of_scaled step.euro_rate_bps 10_000)
          ();
      ]
    ~corporate_actions:(corporate_actions trace index step)
    (Int64.of_int index)

let ensure condition message = if condition then Ok () else Error message

let money_equal label expected actual =
  ensure
    (T.Scalar.Money.equal expected actual)
    (Format.asprintf "%s: expected %a, got %a" label T.Scalar.Money.pp expected
       T.Scalar.Money.pp actual)

let sum_money values =
  List.fold_left
    (fun result value ->
      let* total = result in
      T.Scalar.Money.add total value)
    (Ok T.Scalar.Money.zero) values

let check_position_attribution position =
  let* market_value =
    T.Scalar.Money.notional position.T.Account.mark position.quantity
  in
  let* () =
    money_equal "position market value" market_value position.market_value
  in
  let* unrealized =
    T.Scalar.Money.subtract position.market_value position.cost_basis
  in
  let* () =
    money_equal "position unrealized P&L" unrealized position.unrealized_pnl
  in
  let* total_fees =
    T.Scalar.Money.add position.execution_fees position.borrow_fees
  in
  let* () = money_equal "position total fees" total_fees position.total_fees in
  let* base_market =
    T.Scalar.Money.convert position.market_value ~rate:position.fx_rate
  in
  let* () =
    money_equal "position base market value" base_market
      position.base_market_value
  in
  let* base_basis =
    T.Scalar.Money.convert position.cost_basis ~rate:position.fx_rate
  in
  let* () =
    money_equal "position base cost basis" base_basis position.base_cost_basis
  in
  let* base_realized =
    T.Scalar.Money.convert position.realized_pnl ~rate:position.fx_rate
  in
  let* () =
    money_equal "position base realized P&L" base_realized
      position.base_realized_pnl
  in
  let* base_unrealized =
    T.Scalar.Money.convert position.unrealized_pnl ~rate:position.fx_rate
  in
  let* () =
    money_equal "position base unrealized P&L" base_unrealized
      position.base_unrealized_pnl
  in
  let* base_total_fees =
    T.Scalar.Money.convert position.total_fees ~rate:position.fx_rate
  in
  let* () =
    money_equal "position base total fees" base_total_fees
      position.base_total_fees
  in
  let* () =
    ensure
      (T.Scalar.Money.compare position.execution_fees T.Scalar.Money.zero >= 0)
      "execution fees became negative"
  in
  ensure
    (T.Scalar.Money.compare position.borrow_fees T.Scalar.Money.zero >= 0)
    "borrow fees became negative"

let check_valuation risk (valuation : T.Audit.valuation) =
  let account = valuation.account in
  let* () =
    List.fold_left
      (fun result position ->
        let* () = result in
        check_position_attribution position)
      (Ok ()) account.positions
  in
  let* cash =
    account.cash_balances
    |> List.map (fun balance -> balance.T.Account.base_value)
    |> sum_money
  in
  let* () = money_equal "cash attribution" cash account.cash in
  let sum_position field = account.positions |> List.map field |> sum_money in
  let* net =
    sum_position (fun position -> position.T.Account.base_market_value)
  in
  let* () = money_equal "net market value" net account.net_market_value in
  let* basis =
    sum_position (fun position -> position.T.Account.base_cost_basis)
  in
  let* () = money_equal "cost basis" basis account.cost_basis in
  let* realized =
    sum_position (fun position -> position.T.Account.base_realized_pnl)
  in
  let* () = money_equal "realized P&L" realized account.realized_pnl in
  let* unrealized =
    sum_position (fun position -> position.T.Account.base_unrealized_pnl)
  in
  let* () = money_equal "unrealized P&L" unrealized account.unrealized_pnl in
  let* execution_fees =
    sum_position (fun position -> position.T.Account.base_execution_fees)
  in
  let* () =
    money_equal "execution fee attribution" execution_fees
      account.execution_fees
  in
  let* borrow_fees =
    sum_position (fun position -> position.T.Account.base_borrow_fees)
  in
  let* () =
    money_equal "borrow fee attribution" borrow_fees account.borrow_fees
  in
  let* total_fees =
    sum_position (fun position -> position.T.Account.base_total_fees)
  in
  let* () = money_equal "total fee attribution" total_fees account.total_fees in
  let* gross =
    T.Scalar.Money.add account.long_market_value account.short_market_value
  in
  let* () = money_equal "gross exposure" gross account.gross_exposure in
  let* negative_short = T.Scalar.Money.negate account.short_market_value in
  let* signed_exposure =
    T.Scalar.Money.add account.long_market_value negative_short
  in
  let* () =
    money_equal "signed exposure" signed_exposure account.net_market_value
  in
  let* equity = T.Scalar.Money.add account.cash account.net_market_value in
  let* () = money_equal "equity" equity account.equity in
  let* expected_margin = T.Risk.margin_snapshot risk account in
  let actual_margin = valuation.margin in
  let* () =
    money_equal "initial margin requirement" expected_margin.initial_requirement
      actual_margin.initial_requirement
  in
  let* () =
    money_equal "maintenance margin requirement"
      expected_margin.maintenance_requirement
      actual_margin.maintenance_requirement
  in
  let* () =
    money_equal "initial margin excess" expected_margin.initial_excess
      actual_margin.initial_excess
  in
  let* () =
    money_equal "maintenance margin excess" expected_margin.maintenance_excess
      actual_margin.maintenance_excess
  in
  ensure
    (Bool.equal expected_margin.margin_call actual_margin.margin_call)
    "margin call state disagrees with the account valuation"

type audit_history = { next_sequence : int64; seen : T.Id.Event.Set.t }

let empty_history = { next_sequence = 1L; seen = T.Id.Event.Set.empty }

let check_audits history audits =
  List.fold_left
    (fun result audit ->
      let* history = result in
      let* () =
        ensure
          (Int64.equal audit.T.Audit.engine_sequence history.next_sequence)
          "audit sequence is not contiguous"
      in
      let expected_id =
        T.Audit.event_id ~run_id:audit.run_id
          ~engine_sequence:audit.engine_sequence
      in
      let* () =
        ensure
          (T.Id.Event.equal expected_id audit.event_id)
          "audit event ID is not derived from its sequence"
      in
      let canonical = List.sort_uniq T.Id.Event.compare audit.causation_ids in
      let* () =
        ensure
          (List.equal T.Id.Event.equal canonical audit.causation_ids)
          "audit causation IDs are not canonical"
      in
      let* () =
        ensure
          (List.for_all
             (fun cause -> T.Id.Event.Set.mem cause history.seen)
             audit.causation_ids)
          "audit causation refers to a non-prior event"
      in
      Ok
        {
          next_sequence = Int64.succ history.next_sequence;
          seen = T.Id.Event.Set.add audit.event_id history.seen;
        })
    (Ok history) audits

let check_order history order =
  let request_quantity = order.T.Order.request.quantity in
  let* () =
    ensure
      (T.Scalar.Quantity.is_positive request_quantity)
      "order request quantity is not positive"
  in
  let* () =
    ensure
      (T.Scalar.Quantity.is_nonnegative order.filled_quantity)
      "order filled quantity is negative"
  in
  let* () =
    ensure
      (T.Scalar.Quantity.compare order.filled_quantity request_quantity <= 0)
      "order filled quantity exceeds its request"
  in
  let* () =
    ensure
      (T.Id.Event.Set.mem order.created_event_id history.seen)
      "order creation event is absent from the audit history"
  in
  let* () =
    ensure
      (T.Id.Event.Set.mem order.updated_event_id history.seen)
      "order update event is absent from the audit history"
  in
  match order.status with
  | T.Order.Working ->
      ensure
        (T.Scalar.Quantity.compare order.filled_quantity request_quantity < 0)
        "working order is completely filled"
  | T.Order.Partially_filled ->
      ensure
        (T.Scalar.Quantity.is_positive order.filled_quantity
        && T.Scalar.Quantity.compare order.filled_quantity request_quantity < 0
        )
        "partial order has an invalid fill quantity"
  | T.Order.Filled ->
      ensure
        (T.Scalar.Quantity.equal order.filled_quantity request_quantity)
        "filled order has a remainder"
  | T.Order.Cancelled | T.Order.Rejected _ -> Ok ()

let check_orders history oms =
  let orders = T.Oms.orders oms in
  let* () =
    List.fold_left
      (fun result order ->
        let* () = result in
        check_order history order)
      (Ok ()) orders
  in
  let expected_active = List.filter T.Order.is_active orders in
  ensure
    (List.length expected_active = List.length (T.Oms.active_orders oms))
    "active order inventory is inconsistent"

let slice_valuation audits =
  List.filter_map
    (fun audit ->
      match audit.T.Audit.event with
      | T.Audit.Valuation value -> Some value
      | _ -> None)
    audits
  |> function
  | [ valuation ] -> Ok valuation
  | _ -> Error "slice did not emit exactly one valuation"

let check_fill oms audit =
  match audit.T.Audit.event with
  | T.Audit.Fill_applied fill ->
      let* expected_notional =
        T.Scalar.Money.notional fill.T.Fill.price fill.quantity
      in
      let* () = money_equal "fill notional" expected_notional fill.notional in
      let* () =
        ensure
          (T.Scalar.Quantity.is_positive fill.quantity)
          "fill quantity is not positive"
      in
      let* () =
        ensure
          (T.Scalar.Money.compare fill.fee T.Scalar.Money.zero >= 0)
          "fill fee is negative"
      in
      ensure
        (Option.is_some (T.Oms.find oms fill.order_id))
        "fill refers to an absent order"
  | _ -> Ok ()

let check_slice risk history state audits =
  let* history = check_audits history audits in
  let* valuation = slice_valuation audits in
  let* () = check_valuation risk valuation in
  let oms = Generated_runner.oms state in
  let* () = check_orders history oms in
  let* () =
    List.fold_left
      (fun result audit ->
        let* () = result in
        check_fill oms audit)
      (Ok ()) audits
  in
  Ok history

let property_failure trace slice message =
  QCheck2.Test.fail_reportf "slice %d: %s\nshrunk scenario:\n%s" slice message
    (print_trace trace)

let reducer_invariants_hold trace =
  let risk = make_risk trace in
  let config = make_config trace risk in
  let state =
    Generated_runner.create ~run_id:(run_id "property-run") ~scenario_sha256
      ~config ~initial_cash ~strategy_state:(schedule trace)
    |> ok
  in
  let rec loop index history state = function
    | [] -> (
        match Generated_runner.complete state with
        | Error message -> property_failure trace index message
        | Ok (_, valuation, audits) -> (
            match check_audits history audits with
            | Error message -> property_failure trace index message
            | Ok _ -> (
                let completed =
                  List.find_map
                    (fun audit ->
                      match audit.T.Audit.event with
                      | T.Audit.Run_completed { valuation; _ } -> Some valuation
                      | _ -> None)
                    audits
                in
                match completed with
                | None ->
                    property_failure trace index "missing completion audit"
                | Some completed -> (
                    match
                      ( money_equal "completion equity" valuation.equity
                          completed.account.equity,
                        check_valuation risk completed )
                    with
                    | Ok (), Ok () -> true
                    | Error message, _ | _, Error message ->
                        property_failure trace index message))))
    | step :: rest -> (
        let slice = make_slice trace index step in
        match Generated_runner.process_slice state slice with
        | Error message -> property_failure trace index message
        | Ok (state, audits) -> (
            match check_slice risk history state audits with
            | Error message -> property_failure trace index message
            | Ok history -> loop (index + 1) history state rest))
  in
  loop 1 empty_history state trace.steps

let account_signature account =
  let cash =
    T.Account.cash_balances account
    |> List.map (fun (currency, amount) ->
        currency ^ "=" ^ T.Scalar.Money.to_decimal_string amount)
  in
  let positions =
    T.Account.positions account
    |> List.map (fun (instrument_id, (position : T.Account.position)) ->
        String.concat ":"
          [
            T.Id.Instrument.to_string instrument_id;
            T.Scalar.Quantity.to_decimal_string position.T.Account.quantity;
            T.Scalar.Money.to_decimal_string position.cost_basis;
            T.Scalar.Money.to_decimal_string position.realized_pnl;
            T.Scalar.Money.to_decimal_string position.dividend_pnl;
            T.Scalar.Money.to_decimal_string position.execution_fees;
            T.Scalar.Money.to_decimal_string position.borrow_fees;
          ])
  in
  cash @ positions

let order_signature oms =
  T.Oms.orders oms
  |> List.map (fun order ->
      String.concat ":"
        [
          T.Id.Order.to_string order.T.Order.id;
          T.Id.Instrument.to_string order.request.instrument_id;
          side_name order.request.side;
          T.Scalar.Quantity.to_decimal_string order.request.quantity;
          T.Scalar.Quantity.to_decimal_string order.filled_quantity;
          T.Scalar.Money.to_decimal_string order.filled_notional;
          T.Order.status_to_string order.status;
        ])

let rec drive_interactive strategy_state progress =
  match T.Engine.Interactive.strategy_request progress with
  | Some (context, event) ->
      let strategy_state, intents =
        Generated_strategy.on_event strategy_state context event
      in
      let* progress = T.Engine.Interactive.resume progress intents in
      drive_interactive strategy_state progress
  | None -> (
      match T.Engine.Interactive.slice_result progress with
      | Some (state, audits) -> Ok (state, strategy_state, audits)
      | None -> Error "interactive reducer reached an invalid progress state")

let reducers_agree trace =
  let risk = make_risk trace in
  let config = make_config trace risk in
  let strategy_state = schedule trace in
  let scripted =
    Generated_runner.create
      ~run_id:(run_id "property-equivalence")
      ~scenario_sha256 ~config ~initial_cash ~strategy_state
    |> ok
  in
  let interactive =
    T.Engine.Interactive.create
      ~run_id:(run_id "property-equivalence")
      ~scenario_sha256 ~config ~initial_cash
    |> ok
  in
  let compare_slice index scripted interactive audits scripted_audits =
    let expected = List.map T.Codec.audit_to_string scripted_audits in
    let actual = List.map T.Codec.audit_to_string audits in
    if not (List.equal String.equal expected actual) then
      property_failure trace index "scripted and interactive audits differ"
    else if
      not
        (List.equal String.equal
           (account_signature (Generated_runner.account scripted))
           (account_signature (T.Engine.Interactive.account interactive)))
    then property_failure trace index "scripted and interactive accounts differ"
    else if
      not
        (List.equal String.equal
           (order_signature (Generated_runner.oms scripted))
           (order_signature (T.Engine.Interactive.oms interactive)))
    then property_failure trace index "scripted and interactive orders differ"
    else true
  in
  let rec loop index scripted interactive strategy_state = function
    | [] -> (
        match
          ( Generated_runner.complete scripted,
            T.Engine.Interactive.complete interactive )
        with
        | ( Ok (_, scripted_valuation, scripted_audits),
            Ok (_, interactive_valuation, interactive_audits) ) ->
            if
              List.equal String.equal
                (List.map T.Codec.audit_to_string scripted_audits)
                (List.map T.Codec.audit_to_string interactive_audits)
              && T.Scalar.Money.equal scripted_valuation.equity
                   interactive_valuation.equity
            then true
            else property_failure trace index "completion results differ"
        | Error message, _ | _, Error message ->
            property_failure trace index message)
    | step :: rest -> (
        let slice = make_slice trace index step in
        match
          ( Generated_runner.process_slice scripted slice,
            T.Engine.Interactive.process_slice interactive slice )
        with
        | Ok (scripted, scripted_audits), Ok progress -> (
            match drive_interactive strategy_state progress with
            | Error message -> property_failure trace index message
            | Ok (interactive, strategy_state, audits) ->
                if
                  compare_slice index scripted interactive audits
                    scripted_audits
                then loop (index + 1) scripted interactive strategy_state rest
                else false)
        | Error message, _ | _, Error message ->
            property_failure trace index message)
  in
  loop 1 scripted interactive strategy_state trace.steps

let trace_shape trace =
  let command_count =
    List.fold_left
      (fun total step -> total + List.length step.commands)
      0 trace.steps
  in
  Printf.sprintf "steps=%d commands=%d split=%b" (List.length trace.steps)
    command_count
    (Option.is_some trace.split_asset)

let property_count default =
  match Sys.getenv_opt "REDUCER_PROPERTY_CASES" with
  | None -> default
  | Some value -> (
      match int_of_string_opt value with
      | Some count when count > 0 -> count
      | _ -> invalid_arg "REDUCER_PROPERTY_CASES must be a positive integer")

let reducer_invariant_property =
  QCheck2.Test.make ~name:"generated reducer traces reconcile after every slice"
    ~count:(property_count 300) ~print:print_trace ~collect:trace_shape
    gen_trace reducer_invariants_hold

let reducer_equivalence_property =
  QCheck2.Test.make
    ~name:"generated scripted and interactive reducer traces agree"
    ~count:(property_count 250) ~print:print_trace ~collect:trace_shape
    gen_trace reducers_agree

let tests =
  [
    QCheck_alcotest.to_alcotest ~speed_level:`Quick reducer_invariant_property;
    QCheck_alcotest.to_alcotest ~speed_level:`Quick reducer_equivalence_property;
  ]
