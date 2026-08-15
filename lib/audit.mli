(** Versioned deterministic audit events emitted by the pure engine. *)

type cancellation_reason = Strategy_requested | Target_replaced | Market_ioc

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

type t = private {
  schema_version : int;
  engine_sequence : int64;
  run_id : Id.Run.t;
  recorded_at : Ptime.t;
  event : event;
}

val create :
  engine_sequence:int64 -> run_id:Id.Run.t -> recorded_at:Ptime.t -> event -> t

val cancellation_reason_to_string : cancellation_reason -> string
val event_name : event -> string
