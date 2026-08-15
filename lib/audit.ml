type cancellation_reason =
  | Strategy_requested
  | Target_replaced
  | Market_ioc
  | Margin_call

type target_basis = Weights | Quantities

type requested_target = {
  instrument_id : Id.Instrument.t;
  weight : Scalar.Weight.t option;
  quantity : Scalar.Quantity.t;
  reference_price : Scalar.Price.t option;
}

type order_counts = {
  total : int;
  active : int;
  filled : int;
  rejected : int;
  cancelled : int;
}

type valuation = { account : Account.valuation; margin : Risk.margin_snapshot }

type event =
  | Run_started of { scenario_sha256 : string; execution_model : string }
  | Market_slice_received of Market_slice.t
  | Target_portfolio_requested of {
      basis : target_basis;
      targets : requested_target list;
    }
  | Order_accepted of Order.t
  | Order_rejected of Order.t
  | Order_cancelled of { order : Order.t; reason : cancellation_reason }
  | Split_applied of {
      action : Corporate_action.t;
      previous_quantity : Scalar.Quantity.t;
      adjusted_quantity : Scalar.Quantity.t;
    }
  | Cash_dividend_applied of {
      action : Corporate_action.t;
      quantity : Scalar.Quantity.t;
      cash_amount : Scalar.Money.t;
    }
  | Order_adjusted of { order : Order.t; action_id : Id.Corporate_action.t }
  | Fill_applied of Fill.t
  | Margin_limited of {
      order_id : Id.Order.t;
      instrument_id : Id.Instrument.t;
      requested_quantity : Scalar.Quantity.t;
      permitted_quantity : Scalar.Quantity.t;
      price : Scalar.Price.t;
    }
  | Borrow_fee_applied of {
      instrument_id : Id.Instrument.t;
      quote_currency : string;
      short_quantity : Scalar.Quantity.t;
      reference_price : Scalar.Price.t;
      borrow_bps : int;
      period_start : Ptime.t;
      period_end : Ptime.t;
      fee : Scalar.Money.t;
    }
  | Margin_call_triggered of valuation
  | Margin_restored of valuation
  | Intent_rejected of string
  | Metric_emitted of { name : string; value : string }
  | Valuation of valuation
  | Run_completed of {
      scenario_sha256 : string;
      execution_model : string;
      valuation : valuation;
      order_counts : order_counts;
    }

type t = {
  contract_version : string;
  engine_sequence : int64;
  event_id : Id.Event.t;
  causation_ids : Id.Event.t list;
  run_id : Id.Run.t;
  recorded_at : Ptime.t;
  event : event;
}

let event_id ~run_id ~engine_sequence =
  Printf.sprintf "%s-event-%012Ld" (Id.Run.to_string run_id) engine_sequence
  |> Id.Event.of_string_exn

let create ~engine_sequence ~causation_ids ~run_id ~recorded_at event =
  {
    contract_version = Contract.version;
    engine_sequence;
    event_id = event_id ~run_id ~engine_sequence;
    causation_ids;
    run_id;
    recorded_at;
    event;
  }

let cancellation_reason_to_string = function
  | Strategy_requested -> "strategy_requested"
  | Target_replaced -> "target_replaced"
  | Market_ioc -> "market_ioc"
  | Margin_call -> "margin_call"

let target_basis_to_string = function
  | Weights -> "weights"
  | Quantities -> "quantities"

let event_name = function
  | Run_started _ -> "run_started"
  | Market_slice_received _ -> "market_slice_received"
  | Target_portfolio_requested _ -> "target_portfolio_requested"
  | Order_accepted _ -> "order_accepted"
  | Order_rejected _ -> "order_rejected"
  | Order_cancelled _ -> "order_cancelled"
  | Split_applied _ -> "split_applied"
  | Cash_dividend_applied _ -> "cash_dividend_applied"
  | Order_adjusted _ -> "order_adjusted"
  | Fill_applied _ -> "fill_applied"
  | Margin_limited _ -> "margin_limited"
  | Borrow_fee_applied _ -> "borrow_fee_applied"
  | Margin_call_triggered _ -> "margin_call"
  | Margin_restored _ -> "margin_restored"
  | Intent_rejected _ -> "intent_rejected"
  | Metric_emitted _ -> "metric_emitted"
  | Valuation _ -> "valuation"
  | Run_completed _ -> "run_completed"
