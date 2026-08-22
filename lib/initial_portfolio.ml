type position = {
  instrument_id : Id.Instrument.t;
  quantity : Scalar.Quantity.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  dividend_pnl : Scalar.Money.t;
  execution_fees : Scalar.Money.t;
  borrow_fees : Scalar.Money.t;
}

type t = {
  base_currency : string;
  cash : (string * Scalar.Money.t) list;
  positions : position list;
  marks : (Id.Instrument.t * Scalar.Price.t) list;
  fx_rates : (string * Scalar.Price.t) list;
}

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let valid_currency value =
  String.length value > 0
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x21 && code <> 0x7f)
       value

let position ~instrument_id ~quantity ~cost_basis ~realized_pnl ~dividend_pnl
    ~execution_fees ~borrow_fees =
  if Scalar.Quantity.is_zero quantity then
    Error "initial position quantity must be nonzero"
  else if
    Scalar.Quantity.is_positive quantity
    <> (Scalar.Money.compare cost_basis Scalar.Money.zero > 0)
  then Error "initial position cost basis must have the same sign as quantity"
  else if Scalar.Money.compare execution_fees Scalar.Money.zero < 0 then
    Error "initial execution fees must be nonnegative"
  else if Scalar.Money.compare borrow_fees Scalar.Money.zero < 0 then
    Error "initial borrow fees must be nonnegative"
  else
    Ok
      {
        instrument_id;
        quantity;
        cost_basis;
        realized_pnl;
        dividend_pnl;
        execution_fees;
        borrow_fees;
      }

let unique compare values =
  List.length values = List.length (List.sort_uniq compare values)

let create ~base_currency ~cash ~positions ~marks ~fx_rates =
  if not (valid_currency base_currency) then
    Error "base currency must not be empty or contain whitespace"
  else if cash = [] then Error "initial cash must contain at least one currency"
  else if not (List.for_all (fun (currency, _) -> valid_currency currency) cash)
  then Error "cash currency must not be empty or contain whitespace"
  else if not (unique String.compare (List.map fst cash)) then
    Error "initial cash currencies must be unique"
  else if not (List.mem_assoc base_currency cash) then
    Error "initial cash must include the base currency"
  else
    let position_ids = List.map (fun value -> value.instrument_id) positions in
    let mark_ids = List.map fst marks in
    let fx_currencies = List.map fst fx_rates in
    if not (unique Id.Instrument.compare position_ids) then
      Error "initial position instrument IDs must be unique"
    else if not (unique Id.Instrument.compare mark_ids) then
      Error "initial mark instrument IDs must be unique"
    else if
      List.sort Id.Instrument.compare position_ids
      <> List.sort Id.Instrument.compare mark_ids
    then Error "initial marks must cover every initial position exactly once"
    else if not (unique String.compare fx_currencies) then
      Error "initial FX currencies must be unique"
    else if
      List.sort String.compare (List.map fst cash)
      <> List.sort String.compare fx_currencies
    then Error "initial FX rates must cover every cash currency exactly once"
    else
      match List.assoc_opt base_currency fx_rates with
      | None -> Error "initial FX rates must include the base currency"
      | Some rate ->
          let one = Scalar.Price.of_decimal_string "1" |> Result.get_ok in
          if Scalar.Price.compare rate one <> 0 then
            Error "initial base-currency FX rate must equal one"
          else
            Ok
              {
                base_currency;
                cash =
                  List.sort
                    (fun (left, _) (right, _) -> String.compare left right)
                    cash;
                positions =
                  List.sort
                    (fun left right ->
                      Id.Instrument.compare left.instrument_id
                        right.instrument_id)
                    positions;
                marks =
                  List.sort
                    (fun (left, _) (right, _) ->
                      Id.Instrument.compare left right)
                    marks;
                fx_rates =
                  List.sort
                    (fun (left, _) (right, _) -> String.compare left right)
                    fx_rates;
              }

let cash_only ~base_currency ~cash =
  let one = Scalar.Price.of_decimal_string "1" |> Result.get_ok in
  let fx_rates = List.map (fun (currency, _) -> (currency, one)) cash in
  create ~base_currency ~cash ~positions:[] ~marks:[] ~fx_rates
