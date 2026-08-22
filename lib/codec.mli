(** Stable JSON codecs for public files and audit events. *)

val ptime_to_string : Ptime.t -> string
val ptime_of_string : string -> (Ptime.t, string) result
val bar_to_yojson : Bar.t -> Yojson.Safe.t
val market_slice_to_yojson : Market_slice.t -> Yojson.Safe.t
val market_slice_to_yojson_v10 : Market_slice.t -> Yojson.Safe.t
val market_slice_to_yojson_v11 : Market_slice.t -> Yojson.Safe.t
val order_to_yojson : Order.t -> Yojson.Safe.t
val order_to_yojson_v8 : Order.t -> Yojson.Safe.t
val fill_to_yojson : Fill.t -> Yojson.Safe.t
val fill_to_yojson_v9 : Fill.t -> Yojson.Safe.t
val initial_portfolio_to_yojson : Initial_portfolio.t -> Yojson.Safe.t
val audit_to_yojson : Audit.t -> Yojson.Safe.t
val audit_to_string : Audit.t -> string
