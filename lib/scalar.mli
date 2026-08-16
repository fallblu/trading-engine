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

  val scale_ratio_exact :
    t -> numerator:int64 -> denominator:int64 -> (t, string) result

  val pp : Format.formatter -> t -> unit
end

module Quantity : sig
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
  val absolute : t -> (t, string) result
  val minimum : t -> t -> t
  val is_zero : t -> bool
  val is_positive : t -> bool
  val is_negative : t -> bool
  val is_nonnegative : t -> bool
  val is_multiple : t -> lot:t -> bool
  val round_toward_zero_to_multiple : t -> multiple:t -> (t, string) result
  val bps_floor : t -> bps:int -> (t, string) result

  val scale_ratio_exact :
    t -> numerator:int64 -> denominator:int64 -> (t, string) result

  val pp : Format.formatter -> t -> unit
end

module Weight : sig
  type t

  val scale : int64
  val of_micros : int64 -> t
  val zero : t
  val one : t
  val of_decimal_string : string -> (t, string) result
  val to_micros : t -> int64
  val to_decimal_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val add : t -> t -> (t, string) result
  val is_negative : t -> bool
  val absolute : t -> (t, string) result
  val pp : Format.formatter -> t -> unit
end

module Ratio : sig
  type t

  val scale : int64
  val one : t
  val of_decimal_string : string -> (t, string) result
  val to_micros : t -> int64
  val to_decimal_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
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
  val absolute : t -> (t, string) result
  val notional : Price.t -> Quantity.t -> (t, string) result
  val for_quantity : t -> Quantity.t -> (t, string) result
  val convert : t -> rate:Price.t -> (t, string) result
  val multiply_ratio : t -> Ratio.t -> (t, string) result
  val weight_toward_zero : t -> equity:t -> (Weight.t, string) result
  val fee : fixed:t -> bps:int -> notional:t -> (t, string) result

  val proportion_toward_zero :
    t -> numerator:Quantity.t -> denominator:Quantity.t -> (t, string) result

  val bps_ceil : t -> bps:int -> (t, string) result
  val pp : Format.formatter -> t -> unit
end
