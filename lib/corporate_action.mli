(** Split and cash-dividend events applied before matching their effective
    market slice. *)

type kind =
  | Split of { numerator : int64; denominator : int64 }
  | Cash_dividend of { amount_per_unit : Scalar.Money.t }
  | Distribution of {
      distribution_type : distribution_type;
      destination_instrument_id : Id.Instrument.t;
      numerator : int64;
      denominator : int64;
      basis_allocation_bps : int;
      fractional_policy : fractional_policy;
    }

and distribution_type = Stock_dividend | Rights | Spin_off

and fractional_policy =
  | Reject_fractional
  | Cash_in_lieu of { price : Scalar.Price.t; currency : string }

type t = private {
  id : Id.Corporate_action.t;
  instrument_id : Id.Instrument.t;
  kind : kind;
}

val split :
  id:Id.Corporate_action.t ->
  instrument_id:Id.Instrument.t ->
  numerator:int64 ->
  denominator:int64 ->
  (t, string) result

val cash_dividend :
  id:Id.Corporate_action.t ->
  instrument_id:Id.Instrument.t ->
  amount_per_unit:Scalar.Money.t ->
  (t, string) result

val distribution :
  id:Id.Corporate_action.t ->
  instrument_id:Id.Instrument.t ->
  distribution_type:distribution_type ->
  destination_instrument_id:Id.Instrument.t ->
  numerator:int64 ->
  denominator:int64 ->
  basis_allocation_bps:int ->
  fractional_policy:fractional_policy ->
  (t, string) result

val distribution_type_to_string : distribution_type -> string
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
