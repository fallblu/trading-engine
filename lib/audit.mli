(** Deterministic audit events emitted by the pure engine. *)

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
  | Run_started of { scenario_sha256 : string; execution_model : string }
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
      execution_model : string;
      valuation : Account.valuation;
      order_counts : order_counts;
    }

type t = private {
  contract_version : string;
  engine_sequence : int64;
  event_id : Id.Event.t;
  causation_ids : Id.Event.t list;
  run_id : Id.Run.t;
  recorded_at : Ptime.t;
  event : event;
}

val event_id : run_id:Id.Run.t -> engine_sequence:int64 -> Id.Event.t

val create :
  engine_sequence:int64 ->
  causation_ids:Id.Event.t list ->
  run_id:Id.Run.t ->
  recorded_at:Ptime.t ->
  event ->
  t

val cancellation_reason_to_string : cancellation_reason -> string
val target_basis_to_string : target_basis -> string
val event_name : event -> string
