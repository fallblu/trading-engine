type cancellation_reason = Strategy_requested | Target_replaced | Market_ioc

type order_counts = {
  total : int;
  active : int;
  filled : int;
  rejected : int;
  cancelled : int;
}

type event =
  | Bar_received of Bar.t
  | Target_requested of {
      instrument_id : Id.Instrument.t;
      quantity : Scalar.Quantity.t;
    }
  | Order_accepted of Order.t
  | Order_rejected of Order.t
  | Order_cancelled of { order : Order.t; reason : cancellation_reason }
  | Fill_applied of Fill.t
  | Intent_rejected of string
  | Metric_emitted of { name : string; value : string }
  | Valuation of Account.valuation
  | Run_completed of {
      valuation : Account.valuation;
      order_counts : order_counts;
    }

type t = {
  schema_version : int;
  engine_sequence : int64;
  run_id : Id.Run.t;
  recorded_at : Ptime.t;
  event : event;
}

let create ~engine_sequence ~run_id ~recorded_at event =
  { schema_version = 1; engine_sequence; run_id; recorded_at; event }

let cancellation_reason_to_string = function
  | Strategy_requested -> "strategy_requested"
  | Target_replaced -> "target_replaced"
  | Market_ioc -> "market_ioc"

let event_name = function
  | Bar_received _ -> "bar_received"
  | Target_requested _ -> "target_requested"
  | Order_accepted _ -> "order_accepted"
  | Order_rejected _ -> "order_rejected"
  | Order_cancelled _ -> "order_cancelled"
  | Fill_applied _ -> "fill_applied"
  | Intent_rejected _ -> "intent_rejected"
  | Metric_emitted _ -> "metric_emitted"
  | Valuation _ -> "valuation"
  | Run_completed _ -> "run_completed"
