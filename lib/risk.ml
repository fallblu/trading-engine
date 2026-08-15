type t = {
  base_currency : string;
  instruments : Instrument.t Id.Instrument.Map.t;
  max_order_quantity : Scalar.Quantity.t;
  max_long_position : Scalar.Quantity.t;
  max_short_position : Scalar.Quantity.t;
  max_gross_exposure : Scalar.Money.t;
  max_leverage : Scalar.Ratio.t;
  initial_margin_bps : int;
  maintenance_margin_bps : int;
  short_borrow_bps : int;
}

type margin_snapshot = {
  initial_requirement : Scalar.Money.t;
  maintenance_requirement : Scalar.Money.t;
  initial_excess : Scalar.Money.t;
  maintenance_excess : Scalar.Money.t;
  margin_call : bool;
}

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let valid_label value =
  String.length value > 0
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x21 && code <> 0x7f)
       value

let create ~base_currency ~instruments ~max_order_quantity ~max_long_position
    ~max_short_position ~max_gross_exposure ~max_leverage ~initial_margin_bps
    ~maintenance_margin_bps ~short_borrow_bps =
  if not (valid_label base_currency) then
    Error "base currency must not be empty or contain whitespace"
  else if not (Scalar.Quantity.is_positive max_order_quantity) then
    Error "maximum order quantity must be positive"
  else if not (Scalar.Quantity.is_positive max_long_position) then
    Error "maximum long position must be positive"
  else if not (Scalar.Quantity.is_positive max_short_position) then
    Error "maximum short position must be positive"
  else if Scalar.Money.compare max_gross_exposure Scalar.Money.zero <= 0 then
    Error "maximum gross exposure must be positive"
  else if initial_margin_bps <= 0 || initial_margin_bps > 10_000 then
    Error "initial margin basis points must be between 1 and 10000"
  else if maintenance_margin_bps <= 0 || maintenance_margin_bps > 10_000 then
    Error "maintenance margin basis points must be between 1 and 10000"
  else if initial_margin_bps < maintenance_margin_bps then
    Error "initial margin must not be below maintenance margin"
  else if short_borrow_bps < 0 || short_borrow_bps > 10_000 then
    Error "short borrow basis points must be between 0 and 10000"
  else if instruments = [] then Error "risk must define at least one instrument"
  else if
    List.exists
      (fun instrument ->
        Scalar.Quantity.compare max_order_quantity
          instrument.Instrument.lot_size
        < 0)
      instruments
  then Error "maximum order quantity must cover every instrument lot size"
  else if
    List.exists
      (fun instrument ->
        Scalar.Quantity.compare max_long_position instrument.Instrument.lot_size
        < 0)
      instruments
  then Error "maximum long position must cover every instrument lot size"
  else if
    List.exists
      (fun instrument ->
        Scalar.Quantity.compare max_short_position
          instrument.Instrument.lot_size
        < 0)
      instruments
  then Error "maximum short position must cover every instrument lot size"
  else
    let add result instrument =
      let* map = result in
      if Id.Instrument.Map.mem instrument.Instrument.id map then
        Error "instrument IDs must be unique"
      else Ok (Id.Instrument.Map.add instrument.id instrument map)
    in
    let* instruments =
      List.fold_left add (Ok Id.Instrument.Map.empty) instruments
    in
    Ok
      {
        base_currency;
        instruments;
        max_order_quantity;
        max_long_position;
        max_short_position;
        max_gross_exposure;
        max_leverage;
        initial_margin_bps;
        maintenance_margin_bps;
        short_borrow_bps;
      }

let base_currency state = state.base_currency

let instruments state =
  Id.Instrument.Map.bindings state.instruments |> List.map snd

let instrument state instrument_id =
  Id.Instrument.Map.find_opt instrument_id state.instruments

let max_order_quantity state = state.max_order_quantity
let max_long_position state = state.max_long_position
let max_short_position state = state.max_short_position
let max_gross_exposure state = state.max_gross_exposure
let max_leverage state = state.max_leverage
let initial_margin_bps state = state.initial_margin_bps
let maintenance_margin_bps state = state.maintenance_margin_bps
let short_borrow_bps state = state.short_borrow_bps

let margin_snapshot state valuation =
  let* initial_requirement =
    Scalar.Money.bps_ceil valuation.Account.gross_exposure
      ~bps:state.initial_margin_bps
  in
  let* maintenance_requirement =
    Scalar.Money.bps_ceil valuation.gross_exposure
      ~bps:state.maintenance_margin_bps
  in
  let* initial_excess =
    Scalar.Money.subtract valuation.equity initial_requirement
  in
  let* maintenance_excess =
    Scalar.Money.subtract valuation.equity maintenance_requirement
  in
  Ok
    {
      initial_requirement;
      maintenance_requirement;
      initial_excess;
      maintenance_excess;
      margin_call =
        Scalar.Money.compare maintenance_excess Scalar.Money.zero < 0;
    }

let check_initial_values state ~equity ~gross_exposure =
  if Scalar.Money.compare gross_exposure state.max_gross_exposure > 0 then
    Error "portfolio would exceed maximum gross exposure"
  else
    let* leveraged_equity =
      Scalar.Money.multiply_ratio equity state.max_leverage
    in
    if Scalar.Money.compare gross_exposure leveraged_equity > 0 then
      Error "portfolio would exceed maximum leverage"
    else
      let* initial_requirement =
        Scalar.Money.bps_ceil gross_exposure ~bps:state.initial_margin_bps
      in
      let* initial_excess = Scalar.Money.subtract equity initial_requirement in
      if Scalar.Money.compare initial_excess Scalar.Money.zero < 0 then
        Error "portfolio would violate initial margin"
      else Ok ()

let check_initial state valuation =
  check_initial_values state ~equity:valuation.Account.equity
    ~gross_exposure:valuation.gross_exposure

let check_post_fill state ~before ~after =
  if
    Scalar.Money.compare after.Account.gross_exposure
      before.Account.gross_exposure
    <= 0
  then Ok ()
  else check_initial state after

let check_position state quantity =
  if Scalar.Quantity.compare quantity state.max_long_position > 0 then
    Error "position would exceed the maximum long position"
  else
    let* minimum_short = Scalar.Quantity.negate state.max_short_position in
    if Scalar.Quantity.compare quantity minimum_short < 0 then
      Error "position would exceed the maximum short position"
    else Ok ()

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

let signed_order_quantity request =
  match request.Order.side with
  | Order.Buy -> Ok request.quantity
  | Order.Sell -> Scalar.Quantity.negate request.quantity

let working_position ~account ~oms instrument_id =
  let current = Account.position_quantity account instrument_id in
  let active = Oms.active_for_instrument oms instrument_id in
  List.fold_left
    (fun result order ->
      let* quantity = result in
      let remaining = Order.remaining_quantity order in
      let* delta =
        match order.Order.request.side with
        | Order.Buy -> Ok remaining
        | Order.Sell -> Scalar.Quantity.negate remaining
      in
      Scalar.Quantity.add quantity delta)
    (Ok current) active

let projected_position ~account ~oms request =
  let* pending = working_position ~account ~oms request.Order.instrument_id in
  let* projected =
    let* delta = signed_order_quantity request in
    Scalar.Quantity.add pending delta
  in
  if
    Scalar.Quantity.is_positive pending
    && Scalar.Quantity.is_negative projected
    || Scalar.Quantity.is_negative pending
       && Scalar.Quantity.is_positive projected
  then Error "one order must not cross a position through zero"
  else Ok projected

let projected_valuation_quantities state ~account ~oms request projected =
  Id.Instrument.Map.bindings state.instruments
  |> List.fold_left
       (fun result (instrument_id, _) ->
         let* values = result in
         if Id.Instrument.equal instrument_id request.Order.instrument_id then
           Ok ((instrument_id, projected) :: values)
         else
           let* quantity = working_position ~account ~oms instrument_id in
           Ok ((instrument_id, quantity) :: values))
       (Ok [])
  |> Result.map List.rev

let projected_gross_exposure state ~account ~oms ~marks ~fx_rates request =
  let* projected = projected_position ~account ~oms request in
  let* () = check_position state projected in
  let* quantities =
    projected_valuation_quantities state ~account ~oms request projected
  in
  let mark_map =
    List.fold_left
      (fun map (instrument_id, mark) ->
        Id.Instrument.Map.add instrument_id mark map)
      Id.Instrument.Map.empty marks
  in
  let module Currency_map = Map.Make (String) in
  let fx_map =
    List.fold_left
      (fun map (currency, rate) -> Currency_map.add currency rate map)
      Currency_map.empty fx_rates
  in
  let* gross_exposure =
    List.fold_left
      (fun result (instrument_id, quantity) ->
        let* gross = result in
        let* instrument =
          match instrument state instrument_id with
          | Some value -> Ok value
          | None -> Error "projected position has no configured instrument"
        in
        let* mark =
          match Id.Instrument.Map.find_opt instrument_id mark_map with
          | Some value -> Ok value
          | None -> Error "projected position has no current market price"
        in
        let* rate =
          match Currency_map.find_opt instrument.quote_currency fx_map with
          | Some value -> Ok value
          | None -> Error "projected position has no current FX rate"
        in
        let* value = Scalar.Money.notional mark quantity in
        let* value = Scalar.Money.absolute value in
        let* value = Scalar.Money.convert value ~rate in
        Scalar.Money.add gross value)
      (Ok Scalar.Money.zero) quantities
  in
  Ok gross_exposure

let check state ~account ~oms ~marks ~fx_rates request =
  if Scalar.Quantity.compare request.Order.quantity state.max_order_quantity > 0
  then Error "order exceeds the maximum order quantity"
  else
    match instrument state request.instrument_id with
    | None -> Error "order refers to an unknown instrument"
    | Some instrument ->
        let* () = check_alignment instrument request in
        let* pending =
          working_position ~account ~oms request.Order.instrument_id
        in
        let* projected = projected_position ~account ~oms request in
        let* pending_absolute = Scalar.Quantity.absolute pending in
        let* projected_absolute = Scalar.Quantity.absolute projected in
        if Scalar.Quantity.compare projected_absolute pending_absolute <= 0 then
          Ok ()
        else
          let* before =
            Account.value account ~instruments:(instruments state) ~marks
              ~fx_rates
          in
          let* projected_gross_exposure =
            projected_gross_exposure state ~account ~oms ~marks ~fx_rates
              request
          in
          check_initial_values state ~equity:before.equity
            ~gross_exposure:projected_gross_exposure
