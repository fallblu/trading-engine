type kind =
  | Split of { numerator : int64; denominator : int64 }
  | Cash_dividend of { amount_per_unit : Scalar.Money.t }

type t = {
  id : Id.Corporate_action.t;
  instrument_id : Id.Instrument.t;
  kind : kind;
}

let split ~id ~instrument_id ~numerator ~denominator =
  if Int64.compare numerator 0L <= 0 || Int64.compare denominator 0L <= 0 then
    Error "split numerator and denominator must be positive"
  else if Int64.equal numerator denominator then
    Error "split ratio must change the instrument units"
  else Ok { id; instrument_id; kind = Split { numerator; denominator } }

let cash_dividend ~id ~instrument_id ~amount_per_unit =
  if Scalar.Money.compare amount_per_unit Scalar.Money.zero <= 0 then
    Error "cash dividend amount per unit must be positive"
  else Ok { id; instrument_id; kind = Cash_dividend { amount_per_unit } }

let compare left right = Id.Corporate_action.compare left.id right.id

let pp formatter action =
  let kind =
    match action.kind with
    | Split { numerator; denominator } ->
        Printf.sprintf "split %Ld:%Ld" numerator denominator
    | Cash_dividend { amount_per_unit } ->
        "dividend " ^ Scalar.Money.to_decimal_string amount_per_unit
  in
  Format.fprintf formatter "%a %s %a" Id.Corporate_action.pp action.id kind
    Id.Instrument.pp action.instrument_id
