(** Checked fixed-point values for executable state. Prices and money use six
    decimal places. *)

module Price : sig
  type t

  val scale : int64
  val of_micros : int64 -> (t, string) result
  val of_decimal_string : string -> (t, string) result
  val to_micros : t -> int64
  val to_decimal_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val is_multiple : t -> tick:t -> bool
  val pp : Format.formatter -> t -> unit
end

module Quantity : sig
  type t

  val zero : t
  val of_int64 : int64 -> (t, string) result
  val of_string : string -> (t, string) result
  val to_int64 : t -> int64
  val to_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val add : t -> t -> (t, string) result
  val subtract : t -> t -> (t, string) result
  val minimum : t -> t -> t
  val is_zero : t -> bool
  val is_multiple : t -> lot:t -> bool
  val round_down_to_multiple : t -> multiple:t -> (t, string) result
  val bps_floor : t -> bps:int -> (t, string) result
  val pp : Format.formatter -> t -> unit
end

module Weight : sig
  type t

  val scale : int64
  val zero : t
  val one : t
  val of_decimal_string : string -> (t, string) result
  val to_micros : t -> int64
  val to_decimal_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val add : t -> t -> (t, string) result
  val pp : Format.formatter -> t -> unit
end

module Money : sig
  type t

  val scale : int64
  val zero : t
  val of_micros : int64 -> t
  val of_decimal_string : string -> (t, string) result
  val to_micros : t -> int64
  val to_decimal_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val add : t -> t -> (t, string) result
  val subtract : t -> t -> (t, string) result
  val negate : t -> (t, string) result
  val notional : Price.t -> Quantity.t -> (t, string) result
  val fee : fixed:t -> bps:int -> notional:t -> (t, string) result

  val proportion_floor :
    t -> numerator:Quantity.t -> denominator:Quantity.t -> (t, string) result

  val pp : Format.formatter -> t -> unit
end
