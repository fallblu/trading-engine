(** Split and cash-dividend events applied before matching their effective
    market slice. *)

type kind =
  | Split of { numerator : int64; denominator : int64 }
  | Cash_dividend of { amount_per_unit : Scalar.Money.t }

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

val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
