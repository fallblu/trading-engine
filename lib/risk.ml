type t = {
  base_currency : string;
  instruments : Instrument.t Id.Instrument.Map.t;
  max_order_quantity : Scalar.Quantity.t;
  max_position : Scalar.Quantity.t;
}

let create ~base_currency ~instruments ~max_order_quantity ~max_position =
  if
    String.length base_currency = 0
    || String.trim base_currency <> base_currency
  then Error "base currency must be a nonempty trimmed string"
  else if
    List.exists
      (fun instrument ->
        not (String.equal instrument.Instrument.quote_currency base_currency))
      instruments
  then Error "all risk instruments must use the base currency"
  else if Scalar.Quantity.is_zero max_order_quantity then
    Error "maximum order quantity must be positive"
  else if Scalar.Quantity.is_zero max_position then
    Error "maximum position must be positive"
  else
    let add result instrument =
      match result with
      | Error _ as error -> error
      | Ok map ->
          if Id.Instrument.Map.mem instrument.Instrument.id map then
            Error "instrument IDs must be unique"
          else Ok (Id.Instrument.Map.add instrument.id instrument map)
    in
    match List.fold_left add (Ok Id.Instrument.Map.empty) instruments with
    | Error _ as error -> error
    | Ok instruments ->
        Ok { base_currency; instruments; max_order_quantity; max_position }

let base_currency state = state.base_currency

let instruments state =
  Id.Instrument.Map.bindings state.instruments |> List.map snd

let instrument state instrument_id =
  Id.Instrument.Map.find_opt instrument_id state.instruments

let sum_active_quantity orders side =
  let add result order =
    match result with
    | Error _ as error -> error
    | Ok quantity ->
        if order.Order.request.side = side then
          Scalar.Quantity.add quantity (Order.remaining_quantity order)
        else Ok quantity
  in
  List.fold_left add (Ok Scalar.Quantity.zero) orders

let check_alignment instrument request =
  if
    not
      (Scalar.Quantity.is_multiple request.Order.quantity
         ~lot:instrument.Instrument.lot_size)
  then Error "order quantity is not aligned to the instrument lot size"
  else
    match request.kind with
    | Order.Market -> Ok ()
    | Order.Limit price ->
        if Scalar.Price.is_multiple price ~tick:instrument.tick_size then Ok ()
        else Error "limit price is not aligned to the instrument tick size"

let check_buy state ~account ~oms request =
  let current = Account.position_quantity account request.Order.instrument_id in
  let active = Oms.active_for_instrument oms request.instrument_id in
  match sum_active_quantity active Order.Buy with
  | Error _ as error -> error
  | Ok pending -> (
      match Scalar.Quantity.add current pending with
      | Error _ as error -> error
      | Ok projected -> (
          match Scalar.Quantity.add projected request.quantity with
          | Error _ as error -> error
          | Ok projected ->
              if Scalar.Quantity.compare projected state.max_position > 0 then
                Error "order would exceed the maximum long position"
              else Ok ()))

let check_sell ~account ~oms request =
  let current = Account.position_quantity account request.Order.instrument_id in
  let active = Oms.active_for_instrument oms request.instrument_id in
  match sum_active_quantity active Order.Sell with
  | Error _ as error -> error
  | Ok pending -> (
      match Scalar.Quantity.subtract current pending with
      | Error _ -> Error "working sell orders already exceed the long position"
      | Ok available ->
          if Scalar.Quantity.compare request.quantity available > 0 then
            Error "sell order would exceed the available long position"
          else Ok ())

let check state ~account ~oms request =
  if Scalar.Quantity.compare request.Order.quantity state.max_order_quantity > 0
  then Error "order exceeds the maximum order quantity"
  else
    match instrument state request.instrument_id with
    | None -> Error "order refers to an unknown instrument"
    | Some instrument -> (
        match check_alignment instrument request with
        | Error _ as error -> error
        | Ok () -> (
            match request.side with
            | Order.Buy -> check_buy state ~account ~oms request
            | Order.Sell -> check_sell ~account ~oms request))
