type t = {
  id : Id.Instrument.t;
  symbol : string;
  quote_currency : string;
  tick_size : Scalar.Price.t;
  lot_size : Scalar.Quantity.t;
}

let validate_label ~name value =
  if String.length value = 0 then Error (name ^ " must not be empty")
  else if String.trim value <> value then
    Error (name ^ " must not have leading or trailing whitespace")
  else Ok ()

let create ~id ~symbol ~quote_currency ~tick_size ~lot_size =
  match validate_label ~name:"symbol" symbol with
  | Error _ as error -> error
  | Ok () -> (
      match validate_label ~name:"quote currency" quote_currency with
      | Error _ as error -> error
      | Ok () ->
          if Scalar.Quantity.is_zero lot_size then
            Error "lot size must be positive"
          else Ok { id; symbol; quote_currency; tick_size; lot_size })

let pp formatter instrument =
  Format.fprintf formatter "%s (%a)" instrument.symbol Id.Instrument.pp
    instrument.id
