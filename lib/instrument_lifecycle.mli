(** Deterministic mutable-listing state keyed by stable instrument identity. *)

type terminal_policy =
  | Hold
  | Cash_out of { price : Scalar.Price.t; currency : string }

type kind =
  | Halt of { reason : string }
  | Resume
  | Identifier_change of {
      symbol : string;
      provider : string;
      provider_instrument_id : string;
    }
  | Expiration of { terminal_policy : terminal_policy }
  | Delisting of { terminal_policy : terminal_policy; reason : string }

type event = private {
  id : Id.Corporate_action.t;
  instrument_id : Id.Instrument.t;
  kind : kind;
}

type status = Tradable | Halted | Expired | Delisted

type listing = private {
  instrument_id : Id.Instrument.t;
  symbol : string;
  provider_mappings : (string * string) list;
  status : status;
}

type t

val create_event :
  id:Id.Corporate_action.t ->
  instrument_id:Id.Instrument.t ->
  kind:kind ->
  (event, string) result

val compare_event : event -> event -> int
val create : Instrument.t list -> (t, string) result
val listing : t -> Id.Instrument.t -> listing option
val is_tradable : t -> Id.Instrument.t -> bool
val apply : t -> event -> (t, string) result
val status_to_string : status -> string
val kind_to_string : kind -> string
