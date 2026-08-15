type t = {
  id : Id.Instrument.t;
  symbol : string;
  quote_currency : string;
  tick_size : Scalar.Price.t;
  lot_size : Scalar.Quantity.t;
}

let validate_label ~name value =
  if String.length value = 0 then Error (name ^ " must not be empty")
  else if
    not
      (String.for_all
         (fun character ->
           let code = Char.code character in
           code >= 0x21 && code <> 0x7f)
         value)
  then Error (name ^ " must not contain whitespace or control characters")
  else Ok ()

let create ~id ~symbol ~quote_currency ~tick_size ~lot_size =
  match validate_label ~name:"symbol" symbol with
  | Error _ as error -> error
  | Ok () -> (
      match validate_label ~name:"quote currency" quote_currency with
      | Error _ as error -> error
      | Ok () ->
          if not (Scalar.Quantity.is_positive lot_size) then
            Error "lot size must be positive"
          else Ok { id; symbol; quote_currency; tick_size; lot_size })

let pp formatter instrument =
  Format.fprintf formatter "%s (%a)" instrument.symbol Id.Instrument.pp
    instrument.id
