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

let valid_label value =
  String.length value > 0
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x21 && code <> 0x7f)
       value

let distribution ~id ~instrument_id ~distribution_type
    ~destination_instrument_id ~numerator ~denominator ~basis_allocation_bps
    ~fractional_policy =
  if Int64.compare numerator 0L <= 0 || Int64.compare denominator 0L <= 0 then
    Error "distribution numerator and denominator must be positive"
  else if basis_allocation_bps < 0 || basis_allocation_bps > 10_000 then
    Error "distribution basis allocation must be between 0 and 10000 bps"
  else if
    distribution_type = Stock_dividend
    && not (Id.Instrument.equal instrument_id destination_instrument_id)
  then Error "stock dividend destination must be its source instrument"
  else if distribution_type = Stock_dividend && basis_allocation_bps <> 0 then
    Error "stock dividend basis allocation must be zero"
  else if
    distribution_type = Stock_dividend
    && Int64.compare numerator (Int64.sub Int64.max_int denominator) > 0
  then Error "stock dividend total ratio overflows"
  else if
    distribution_type <> Stock_dividend
    && Id.Instrument.equal instrument_id destination_instrument_id
  then Error "rights and spin-off destinations must differ from their source"
  else
    match fractional_policy with
    | Cash_in_lieu { currency; _ } when not (valid_label currency) ->
        Error "cash-in-lieu currency must not be empty or contain whitespace"
    | Reject_fractional | Cash_in_lieu _ ->
        Ok
          {
            id;
            instrument_id;
            kind =
              Distribution
                {
                  distribution_type;
                  destination_instrument_id;
                  numerator;
                  denominator;
                  basis_allocation_bps;
                  fractional_policy;
                };
          }

let distribution_type_to_string = function
  | Stock_dividend -> "stock_dividend"
  | Rights -> "rights"
  | Spin_off -> "spin_off"

let compare left right = Id.Corporate_action.compare left.id right.id

let pp formatter action =
  let kind =
    match action.kind with
    | Split { numerator; denominator } ->
        Printf.sprintf "split %Ld:%Ld" numerator denominator
    | Cash_dividend { amount_per_unit } ->
        "dividend " ^ Scalar.Money.to_decimal_string amount_per_unit
    | Distribution { distribution_type; numerator; denominator; _ } ->
        Printf.sprintf "%s %Ld:%Ld"
          (distribution_type_to_string distribution_type)
          numerator denominator
  in
  Format.fprintf formatter "%a %s %a" Id.Corporate_action.pp action.id kind
    Id.Instrument.pp action.instrument_id
