type cancellation_reason = Strategy_requested | Target_replaced | Market_ioc
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

type event =
  | Run_started of { scenario_sha256 : string }
  | Market_slice_received of Market_slice.t
  | Target_portfolio_requested of {
      basis : target_basis;
      targets : requested_target list;
    }
  | Order_accepted of Order.t
  | Order_rejected of Order.t
  | Order_cancelled of { order : Order.t; reason : cancellation_reason }
  | Fill_applied of Fill.t
  | Cash_limited of {
      order_id : Id.Order.t;
      instrument_id : Id.Instrument.t;
      requested_quantity : Scalar.Quantity.t;
      affordable_quantity : Scalar.Quantity.t;
      price : Scalar.Price.t;
    }
  | Intent_rejected of string
  | Metric_emitted of { name : string; value : string }
  | Valuation of Account.valuation
  | Run_completed of {
      scenario_sha256 : string;
      valuation : Account.valuation;
      order_counts : order_counts;
    }

type t = {
  contract_version : string;
  engine_sequence : int64;
  run_id : Id.Run.t;
  recorded_at : Ptime.t;
  event : event;
}

let create ~engine_sequence ~run_id ~recorded_at event =
  {
    contract_version = Contract.version;
    engine_sequence;
    run_id;
    recorded_at;
    event;
  }

let cancellation_reason_to_string = function
  | Strategy_requested -> "strategy_requested"
  | Target_replaced -> "target_replaced"
  | Market_ioc -> "market_ioc"

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
  | Fill_applied _ -> "fill_applied"
  | Cash_limited _ -> "cash_limited"
  | Intent_rejected _ -> "intent_rejected"
  | Metric_emitted _ -> "metric_emitted"
  | Valuation _ -> "valuation"
  | Run_completed _ -> "run_completed"
