type rounding = Up | Down | Nearest
type liquidity = Maker | Taker
type applicability = Any | Maker_only | Taker_only

type basis =
  | Fixed of Scalar.Money.t
  | Notional_bps of int
  | Per_unit of Scalar.Money.t

type component = {
  name : string;
  currency : string;
  basis : basis;
  rounding : rounding;
  applicability : applicability;
}

type t = {
  schedule_id : string;
  instrument_id : Id.Instrument.t;
  settlement_currency : string;
  minimum : Scalar.Money.t option;
  maximum : Scalar.Money.t option;
  components : component list;
}

type calculated_component = {
  name : string;
  kind : string;
  currency : string;
  amount : Scalar.Money.t;
  quote_amount : Scalar.Money.t;
}

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let valid_token value =
  String.length value > 0
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x21 && code <> 0x7f)
       value

let create_component ~name ~currency ~basis ~rounding ~applicability =
  if not (valid_token name) then
    Error "fee component name must not be empty or contain whitespace"
  else if not (valid_token currency) then
    Error "fee component currency must not be empty or contain whitespace"
  else
    match basis with
    | Notional_bps bps when bps < -10_000 || bps > 10_000 ->
        Error "fee component basis points must be between -10000 and 10000"
    | _ -> Ok { name; currency; basis; rounding; applicability }

let create ~schedule_id ~instrument_id ~settlement_currency ~minimum ~maximum
    ~(components : component list) =
  let nonnegative = function
    | None -> true
    | Some value -> Scalar.Money.compare value Scalar.Money.zero >= 0
  in
  let names =
    List.map (fun (component : component) -> component.name) components
  in
  if not (valid_token schedule_id) then
    Error "fee schedule ID must not be empty or contain whitespace"
  else if not (valid_token settlement_currency) then
    Error "fee settlement currency must not be empty or contain whitespace"
  else if components = [] then Error "fee schedule must contain a component"
  else if List.length names <> List.length (List.sort_uniq String.compare names)
  then Error "fee component names must be unique within a schedule"
  else if not (nonnegative minimum) then Error "fee minimum must be nonnegative"
  else if not (nonnegative maximum) then Error "fee maximum must be nonnegative"
  else
    match (minimum, maximum) with
    | Some lower, Some upper when Scalar.Money.compare lower upper > 0 ->
        Error "fee minimum must not exceed fee maximum"
    | _ ->
        Ok
          {
            schedule_id;
            instrument_id;
            settlement_currency;
            minimum;
            maximum;
            components;
          }

let schedule_id value = value.schedule_id
let instrument_id value = value.instrument_id
let settlement_currency value = value.settlement_currency
let minimum value = value.minimum
let maximum value = value.maximum
let components value = value.components
let component_name (value : component) = value.name
let component_currency (value : component) = value.currency
let component_basis (value : component) = value.basis
let component_rounding (value : component) = value.rounding
let component_applicability (value : component) = value.applicability

let rounding_to_string = function
  | Up -> "up"
  | Down -> "down"
  | Nearest -> "nearest"

let rounding_of_string = function
  | "up" -> Ok Up
  | "down" -> Ok Down
  | "nearest" -> Ok Nearest
  | value -> Error (Printf.sprintf "unsupported fee rounding %S" value)

let applicability_to_string = function
  | Any -> "any"
  | Maker_only -> "maker"
  | Taker_only -> "taker"

let applicability_of_string = function
  | "any" -> Ok Any
  | "maker" -> Ok Maker_only
  | "taker" -> Ok Taker_only
  | value -> Error (Printf.sprintf "unsupported fee applicability %S" value)

let basis_kind = function
  | Fixed _ -> "fixed"
  | Notional_bps _ -> "notional_bps"
  | Per_unit _ -> "per_unit"

let divide ~rounding numerator denominator =
  if Z.equal denominator Z.zero then
    Error "fee conversion rate must be positive"
  else
    let sign = Z.sign numerator in
    let absolute = Z.abs numerator in
    let quotient, remainder = Z.ediv_rem absolute denominator in
    let rounded =
      match rounding with
      | Down -> quotient
      | Up -> if Z.equal remainder Z.zero then quotient else Z.succ quotient
      | Nearest ->
          if Z.compare (Z.mul remainder (Z.of_int 2)) denominator >= 0 then
            Z.succ quotient
          else quotient
    in
    let signed = if sign < 0 then Z.neg rounded else rounded in
    if Z.fits_int64 signed then Ok (Scalar.Money.of_micros (Z.to_int64 signed))
    else Error "fee calculation overflow"

let rate currency fx_rates =
  match List.assoc_opt currency fx_rates with
  | Some value -> Ok value
  | None -> Error ("missing fee FX rate for currency " ^ currency)

let convert ~rounding ~fx_rates ~source_currency ~target_currency amount =
  if String.equal source_currency target_currency then Ok amount
  else
    let* source_rate = rate source_currency fx_rates in
    let* target_rate = rate target_currency fx_rates in
    divide ~rounding
      Z.(
        mul
          (of_int64 (Scalar.Money.to_micros amount))
          (of_int64 (Scalar.Price.to_micros source_rate)))
      (Z.of_int64 (Scalar.Price.to_micros target_rate))

let applies component liquidity =
  match (component.applicability, liquidity) with
  | Any, _ | Maker_only, Maker | Taker_only, Taker -> true
  | Maker_only, Taker | Taker_only, Maker -> false

let raw_amount component ~notional ~quantity ~quote_currency ~fx_rates =
  match component.basis with
  | Fixed value -> Ok value
  | Notional_bps bps ->
      let* native_notional =
        convert ~rounding:component.rounding ~fx_rates
          ~source_currency:quote_currency ~target_currency:component.currency
          notional
      in
      divide ~rounding:component.rounding
        Z.(mul (of_int64 (Scalar.Money.to_micros native_notional)) (of_int bps))
        (Z.of_int 10_000)
  | Per_unit value ->
      divide ~rounding:component.rounding
        Z.(
          mul
            (of_int64 (Scalar.Money.to_micros value))
            (of_int64 (Scalar.Quantity.to_micros quantity)))
        (Z.of_int64 Scalar.Quantity.scale)

let calculate schedule ~quote_currency ~notional ~quantity ~liquidity ~fx_rates
    =
  let calculate_component component =
    let* amount =
      raw_amount component ~notional ~quantity ~quote_currency ~fx_rates
    in
    let* quote_amount =
      convert ~rounding:component.rounding ~fx_rates
        ~source_currency:component.currency ~target_currency:quote_currency
        amount
    in
    Ok
      {
        name = component.name;
        kind = basis_kind component.basis;
        currency = component.currency;
        amount;
        quote_amount;
      }
  in
  let rec collect result = function
    | [] -> Ok (List.rev result)
    | component :: remaining when not (applies component liquidity) ->
        collect result remaining
    | component :: remaining ->
        let* calculated = calculate_component component in
        collect (calculated :: result) remaining
  in
  let* calculated = collect [] schedule.components in
  let* settlement_total =
    List.fold_left
      (fun result component ->
        let* total = result in
        let* amount =
          convert ~rounding:Nearest ~fx_rates
            ~source_currency:component.currency
            ~target_currency:schedule.settlement_currency component.amount
        in
        Scalar.Money.add total amount)
      (Ok Scalar.Money.zero) calculated
  in
  let bounded =
    let after_minimum =
      match schedule.minimum with
      | Some minimum when Scalar.Money.compare settlement_total minimum < 0 ->
          minimum
      | _ -> settlement_total
    in
    match schedule.maximum with
    | Some maximum when Scalar.Money.compare after_minimum maximum > 0 ->
        maximum
    | _ -> after_minimum
  in
  let* adjustment = Scalar.Money.subtract bounded settlement_total in
  let* calculated =
    if Scalar.Money.equal adjustment Scalar.Money.zero then Ok calculated
    else
      let* quote_amount =
        convert ~rounding:Nearest ~fx_rates
          ~source_currency:schedule.settlement_currency
          ~target_currency:quote_currency adjustment
      in
      let name, kind =
        if Scalar.Money.compare adjustment Scalar.Money.zero > 0 then
          ("minimum_adjustment", "minimum_adjustment")
        else ("maximum_adjustment", "maximum_adjustment")
      in
      Ok
        (calculated
        @ [
            {
              name;
              kind;
              currency = schedule.settlement_currency;
              amount = adjustment;
              quote_amount;
            };
          ])
  in
  let* total =
    List.fold_left
      (fun result component ->
        let* total = result in
        Scalar.Money.add total component.quote_amount)
      (Ok Scalar.Money.zero) calculated
  in
  Ok (calculated, total)
