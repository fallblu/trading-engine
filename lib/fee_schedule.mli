(** Deterministic, composable execution-fee schedules. *)

type rounding =
  | Up
  | Down
  | Nearest
      (** Rounding is sign-symmetric: [Up] rounds away from zero, [Down] rounds
          toward zero, and [Nearest] rounds half away from zero. *)

type liquidity = Maker | Taker
type applicability = Any | Maker_only | Taker_only

type basis =
  | Fixed of Scalar.Money.t
  | Notional_bps of int
  | Per_unit of Scalar.Money.t

type component
type t

type calculated_component = private {
  name : string;
  kind : string;
  currency : string;
  amount : Scalar.Money.t;
  quote_amount : Scalar.Money.t;
}

val create_component :
  name:string ->
  currency:string ->
  basis:basis ->
  rounding:rounding ->
  applicability:applicability ->
  (component, string) result

val create :
  schedule_id:string ->
  instrument_id:Id.Instrument.t ->
  settlement_currency:string ->
  minimum:Scalar.Money.t option ->
  maximum:Scalar.Money.t option ->
  components:component list ->
  (t, string) result

val schedule_id : t -> string
val instrument_id : t -> Id.Instrument.t
val settlement_currency : t -> string
val minimum : t -> Scalar.Money.t option
val maximum : t -> Scalar.Money.t option
val components : t -> component list
val component_name : component -> string
val component_currency : component -> string
val component_basis : component -> basis
val component_rounding : component -> rounding
val component_applicability : component -> applicability
val rounding_to_string : rounding -> string
val rounding_of_string : string -> (rounding, string) result
val applicability_to_string : applicability -> string
val applicability_of_string : string -> (applicability, string) result
val basis_kind : basis -> string

val calculate :
  t ->
  quote_currency:string ->
  notional:Scalar.Money.t ->
  quantity:Scalar.Quantity.t ->
  liquidity:liquidity ->
  fx_rates:(string * Scalar.Price.t) list ->
  (calculated_component list * Scalar.Money.t, string) result
