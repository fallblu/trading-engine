module Currency_map = Map.Make (String)

type position = {
  quantity : Scalar.Quantity.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  dividend_pnl : Scalar.Money.t;
  execution_fees : Scalar.Money.t;
  borrow_fees : Scalar.Money.t;
}

type cash_attribution = {
  currency : string;
  amount : Scalar.Money.t;
  fx_rate : Scalar.Price.t;
  base_value : Scalar.Money.t;
}

type position_attribution = {
  instrument_id : Id.Instrument.t;
  quote_currency : string;
  quantity : Scalar.Quantity.t;
  mark : Scalar.Price.t;
  fx_rate : Scalar.Price.t;
  market_value : Scalar.Money.t;
  base_market_value : Scalar.Money.t;
  cost_basis : Scalar.Money.t;
  base_cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  base_realized_pnl : Scalar.Money.t;
  unrealized_pnl : Scalar.Money.t;
  base_unrealized_pnl : Scalar.Money.t;
  dividend_pnl : Scalar.Money.t;
  base_dividend_pnl : Scalar.Money.t;
  execution_fees : Scalar.Money.t;
  base_execution_fees : Scalar.Money.t;
  borrow_fees : Scalar.Money.t;
  base_borrow_fees : Scalar.Money.t;
  total_fees : Scalar.Money.t;
  base_total_fees : Scalar.Money.t;
}

type t = {
  base_currency : string;
  initial_cash : Scalar.Money.t Currency_map.t;
  cash : Scalar.Money.t Currency_map.t;
  positions : position Id.Instrument.Map.t;
}

type valuation = {
  base_currency : string;
  cash : Scalar.Money.t;
  net_market_value : Scalar.Money.t;
  long_market_value : Scalar.Money.t;
  short_market_value : Scalar.Money.t;
  gross_exposure : Scalar.Money.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  unrealized_pnl : Scalar.Money.t;
  equity : Scalar.Money.t;
  dividend_pnl : Scalar.Money.t;
  execution_fees : Scalar.Money.t;
  borrow_fees : Scalar.Money.t;
  total_fees : Scalar.Money.t;
  cash_balances : cash_attribution list;
  positions : position_attribution list;
}

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let empty_position =
  {
    quantity = Scalar.Quantity.zero;
    cost_basis = Scalar.Money.zero;
    realized_pnl = Scalar.Money.zero;
    dividend_pnl = Scalar.Money.zero;
    execution_fees = Scalar.Money.zero;
    borrow_fees = Scalar.Money.zero;
  }

let valid_currency value =
  String.length value > 0
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x21 && code <> 0x7f)
       value

let create ~base_currency ~initial_cash =
  if not (valid_currency base_currency) then
    Error "base currency must not be empty or contain whitespace"
  else
    let add result (currency, amount) =
      let* balances = result in
      if not (valid_currency currency) then
        Error "cash currency must not be empty or contain whitespace"
      else if Currency_map.mem currency balances then
        Error "initial cash currencies must be unique"
      else if Scalar.Money.compare amount Scalar.Money.zero < 0 then
        Error "initial cash balances must be nonnegative"
      else Ok (Currency_map.add currency amount balances)
    in
    let* balances = List.fold_left add (Ok Currency_map.empty) initial_cash in
    if not (Currency_map.mem base_currency balances) then
      Error "initial cash must include the base currency"
    else if Currency_map.is_empty balances then
      Error "initial cash must contain at least one currency"
    else
      Ok
        {
          base_currency;
          initial_cash = balances;
          cash = balances;
          positions = Id.Instrument.Map.empty;
        }

let of_initial_portfolio (initial : Initial_portfolio.t) =
  let cash =
    List.fold_left
      (fun balances (currency, amount) ->
        Currency_map.add currency amount balances)
      Currency_map.empty initial.cash
  in
  let positions =
    List.fold_left
      (fun positions (value : Initial_portfolio.position) ->
        Id.Instrument.Map.add value.instrument_id
          {
            quantity = value.quantity;
            cost_basis = value.cost_basis;
            realized_pnl = value.realized_pnl;
            dividend_pnl = value.dividend_pnl;
            execution_fees = value.execution_fees;
            borrow_fees = value.borrow_fees;
          }
          positions)
      Id.Instrument.Map.empty initial.positions
  in
  Ok
    {
      base_currency = initial.base_currency;
      initial_cash = cash;
      cash;
      positions;
    }

let base_currency (state : t) = state.base_currency
let initial_cash (state : t) = Currency_map.bindings state.initial_cash
let cash_balances (state : t) = Currency_map.bindings state.cash
let cash (state : t) currency = Currency_map.find_opt currency state.cash

let position (state : t) instrument_id =
  Option.value
    (Id.Instrument.Map.find_opt instrument_id state.positions)
    ~default:empty_position

let position_quantity (state : t) instrument_id =
  (position state instrument_id).quantity

let positions (state : t) = Id.Instrument.Map.bindings state.positions

let total_fees (position : position) =
  Scalar.Money.add position.execution_fees position.borrow_fees

let update_position (positions : position Id.Instrument.Map.t) instrument_id
    (value : position) =
  if
    Scalar.Quantity.is_zero value.quantity
    && Scalar.Money.equal value.cost_basis Scalar.Money.zero
    && Scalar.Money.equal value.realized_pnl Scalar.Money.zero
    && Scalar.Money.equal value.dividend_pnl Scalar.Money.zero
    && Scalar.Money.equal value.execution_fees Scalar.Money.zero
    && Scalar.Money.equal value.borrow_fees Scalar.Money.zero
  then Id.Instrument.Map.remove instrument_id positions
  else Id.Instrument.Map.add instrument_id value positions

let adjust_cash (state : t) currency delta =
  match Currency_map.find_opt currency state.cash with
  | None -> Error ("missing cash ledger for currency " ^ currency)
  | Some current ->
      let* amount = Scalar.Money.add current delta in
      Ok { state with cash = Currency_map.add currency amount state.cash }

let add_execution_fee (position : position) fee =
  let* execution_fees = Scalar.Money.add position.execution_fees fee in
  Ok { position with execution_fees }

let ensure_no_cross current delta =
  let* projected = Scalar.Quantity.add current delta in
  if
    Scalar.Quantity.is_positive current
    && Scalar.Quantity.is_negative projected
    || Scalar.Quantity.is_negative current
       && Scalar.Quantity.is_positive projected
  then Error "one fill must not cross a position through zero"
  else Ok projected

let apply_open_long (state : t) fill (current : position) projected =
  let* acquisition_cost = Scalar.Money.add fill.Fill.notional fill.fee in
  let* cash_delta = Scalar.Money.negate acquisition_cost in
  let* state = adjust_cash state fill.quote_currency cash_delta in
  let* cost_basis = Scalar.Money.add current.cost_basis acquisition_cost in
  let* updated =
    add_execution_fee { current with quantity = projected; cost_basis } fill.fee
  in
  Ok
    {
      state with
      positions = update_position state.positions fill.instrument_id updated;
    }

let apply_open_short (state : t) fill (current : position) projected =
  let* net_proceeds = Scalar.Money.subtract fill.Fill.notional fill.fee in
  let* state = adjust_cash state fill.quote_currency net_proceeds in
  let* basis_delta = Scalar.Money.negate net_proceeds in
  let* cost_basis = Scalar.Money.add current.cost_basis basis_delta in
  let* updated =
    add_execution_fee { current with quantity = projected; cost_basis } fill.fee
  in
  Ok
    {
      state with
      positions = update_position state.positions fill.instrument_id updated;
    }

let apply_close_long (state : t) fill (current : position) projected =
  let* removed_basis =
    if Scalar.Quantity.is_zero projected then Ok current.cost_basis
    else
      Scalar.Money.proportion_toward_zero current.cost_basis
        ~numerator:fill.Fill.quantity ~denominator:current.quantity
  in
  let* cost_basis = Scalar.Money.subtract current.cost_basis removed_basis in
  let* net_proceeds = Scalar.Money.subtract fill.notional fill.fee in
  let* state = adjust_cash state fill.quote_currency net_proceeds in
  let* realized_delta = Scalar.Money.subtract net_proceeds removed_basis in
  let* realized_pnl = Scalar.Money.add current.realized_pnl realized_delta in
  let* updated =
    add_execution_fee
      { current with quantity = projected; cost_basis; realized_pnl }
      fill.fee
  in
  Ok
    {
      state with
      positions = update_position state.positions fill.instrument_id updated;
    }

let apply_close_short (state : t) fill (current : position) projected =
  let* short_quantity = Scalar.Quantity.absolute current.quantity in
  let* removed_basis =
    if Scalar.Quantity.is_zero projected then Ok current.cost_basis
    else
      Scalar.Money.proportion_toward_zero current.cost_basis
        ~numerator:fill.Fill.quantity ~denominator:short_quantity
  in
  let* cost_basis = Scalar.Money.subtract current.cost_basis removed_basis in
  let* cover_cost = Scalar.Money.add fill.notional fill.fee in
  let* cash_delta = Scalar.Money.negate cover_cost in
  let* state = adjust_cash state fill.quote_currency cash_delta in
  let* negative_cover_cost = Scalar.Money.negate cover_cost in
  let* realized_delta =
    Scalar.Money.subtract negative_cover_cost removed_basis
  in
  let* realized_pnl = Scalar.Money.add current.realized_pnl realized_delta in
  let* updated =
    add_execution_fee
      { current with quantity = projected; cost_basis; realized_pnl }
      fill.fee
  in
  Ok
    {
      state with
      positions = update_position state.positions fill.instrument_id updated;
    }

let apply_fill (state : t) fill =
  let current = position state fill.Fill.instrument_id in
  match fill.side with
  | Order.Buy ->
      let* projected = ensure_no_cross current.quantity fill.quantity in
      if Scalar.Quantity.is_negative current.quantity then
        apply_close_short state fill current projected
      else apply_open_long state fill current projected
  | Order.Sell ->
      let* delta = Scalar.Quantity.negate fill.quantity in
      let* projected = ensure_no_cross current.quantity delta in
      if Scalar.Quantity.is_positive current.quantity then
        apply_close_long state fill current projected
      else apply_open_short state fill current projected

let apply_split (state : t) ~instrument_id ~numerator ~denominator =
  let current = position state instrument_id in
  if Scalar.Quantity.is_zero current.quantity then Ok state
  else
    let* quantity =
      Scalar.Quantity.scale_ratio_exact current.quantity ~numerator ~denominator
    in
    let positions =
      update_position state.positions instrument_id { current with quantity }
    in
    Ok { state with positions }

let apply_cash_dividend (state : t) ~instrument_id ~quote_currency
    ~amount_per_unit =
  let current = position state instrument_id in
  if Scalar.Quantity.is_zero current.quantity then Ok state
  else
    let* amount = Scalar.Money.for_quantity amount_per_unit current.quantity in
    let* state = adjust_cash state quote_currency amount in
    let* realized_pnl = Scalar.Money.add current.realized_pnl amount in
    let* dividend_pnl = Scalar.Money.add current.dividend_pnl amount in
    let updated = { current with realized_pnl; dividend_pnl } in
    Ok
      {
        state with
        positions = update_position state.positions instrument_id updated;
      }

let apply_borrow_fee (state : t) ~instrument_id ~quote_currency ~fee =
  let current = position state instrument_id in
  if not (Scalar.Quantity.is_negative current.quantity) then
    Error "borrow fees require an open short position"
  else if Scalar.Money.compare fee Scalar.Money.zero <= 0 then
    Error "borrow fee must be positive"
  else
    let* cash_delta = Scalar.Money.negate fee in
    let* state = adjust_cash state quote_currency cash_delta in
    let* realized_pnl = Scalar.Money.add current.realized_pnl cash_delta in
    let* borrow_fees = Scalar.Money.add current.borrow_fees fee in
    let updated = { current with realized_pnl; borrow_fees } in
    Ok
      {
        state with
        positions = update_position state.positions instrument_id updated;
      }

let value (state : t) ~instruments ~marks ~fx_rates =
  let canonical_flat_mark =
    Scalar.Price.of_micros Scalar.Price.scale |> Result.get_ok
  in
  let instrument_map =
    List.fold_left
      (fun map instrument ->
        Id.Instrument.Map.add instrument.Instrument.id instrument map)
      Id.Instrument.Map.empty instruments
  in
  let mark_map =
    List.fold_left
      (fun map (instrument_id, price) ->
        Id.Instrument.Map.add instrument_id price map)
      Id.Instrument.Map.empty marks
  in
  let fx_map =
    List.fold_left
      (fun map (currency, rate) -> Currency_map.add currency rate map)
      Currency_map.empty fx_rates
  in
  let fx currency =
    match Currency_map.find_opt currency fx_map with
    | Some value -> Ok value
    | None -> Error ("missing FX rate for currency " ^ currency)
  in
  let cash_attribution (currency, amount) =
    let* fx_rate = fx currency in
    let* base_value = Scalar.Money.convert amount ~rate:fx_rate in
    Ok { currency; amount; fx_rate; base_value }
  in
  let* cash_balances =
    Currency_map.bindings state.cash
    |> List.fold_left
         (fun result item ->
           let* values = result in
           let* value = cash_attribution item in
           Ok (value :: values))
         (Ok [])
    |> Result.map List.rev
  in
  let position_attribution instrument =
    let instrument_id = instrument.Instrument.id in
    let current = position state instrument_id in
    let* mark =
      match Id.Instrument.Map.find_opt instrument_id mark_map with
      | Some value -> Ok value
      | None when Scalar.Quantity.is_zero current.quantity ->
          Ok canonical_flat_mark
      | None ->
          Error
            (Format.asprintf "missing mark for instrument %a" Id.Instrument.pp
               instrument_id)
    in
    let* fx_rate = fx instrument.quote_currency in
    let* market_value = Scalar.Money.notional mark current.quantity in
    let* unrealized_pnl =
      Scalar.Money.subtract market_value current.cost_basis
    in
    let* total_fees = total_fees current in
    let convert value = Scalar.Money.convert value ~rate:fx_rate in
    let* base_market_value = convert market_value in
    let* base_cost_basis = convert current.cost_basis in
    let* base_realized_pnl = convert current.realized_pnl in
    let* base_unrealized_pnl = convert unrealized_pnl in
    let* base_dividend_pnl = convert current.dividend_pnl in
    let* base_execution_fees = convert current.execution_fees in
    let* base_borrow_fees = convert current.borrow_fees in
    let* base_total_fees = convert total_fees in
    Ok
      {
        instrument_id;
        quote_currency = instrument.quote_currency;
        quantity = current.quantity;
        mark;
        fx_rate;
        market_value;
        base_market_value;
        cost_basis = current.cost_basis;
        base_cost_basis;
        realized_pnl = current.realized_pnl;
        base_realized_pnl;
        unrealized_pnl;
        base_unrealized_pnl;
        dividend_pnl = current.dividend_pnl;
        base_dividend_pnl;
        execution_fees = current.execution_fees;
        base_execution_fees;
        borrow_fees = current.borrow_fees;
        base_borrow_fees;
        total_fees;
        base_total_fees;
      }
  in
  let unknown_mark =
    Id.Instrument.Map.bindings mark_map
    |> List.find_opt (fun (instrument_id, _) ->
        not (Id.Instrument.Map.mem instrument_id instrument_map))
  in
  let* () =
    match unknown_mark with
    | None -> Ok ()
    | Some _ -> Error "mark has no configured instrument"
  in
  let unknown_position =
    Id.Instrument.Map.bindings state.positions
    |> List.find_opt (fun (instrument_id, _) ->
        not (Id.Instrument.Map.mem instrument_id instrument_map))
  in
  let* () =
    match unknown_position with
    | None -> Ok ()
    | Some (instrument_id, _) ->
        Error
          (Format.asprintf "position has no configured instrument %a"
             Id.Instrument.pp instrument_id)
  in
  let missing_held_mark =
    Id.Instrument.Map.bindings state.positions
    |> List.find_opt (fun (instrument_id, (position : position)) ->
        (not (Scalar.Quantity.is_zero position.quantity))
        && not (Id.Instrument.Map.mem instrument_id mark_map))
  in
  let* () =
    match missing_held_mark with
    | None -> Ok ()
    | Some (instrument_id, _) ->
        Error
          (Format.asprintf "held position has no mark %a" Id.Instrument.pp
             instrument_id)
  in
  let attribution_ids =
    Id.Instrument.Map.merge
      (fun _ mark retained_position ->
        match (mark, retained_position) with None, None -> None | _ -> Some ())
      mark_map state.positions
  in
  let* positions =
    Id.Instrument.Map.bindings attribution_ids
    |> List.fold_left
         (fun result (instrument_id, ()) ->
           let* values = result in
           let* instrument =
             match Id.Instrument.Map.find_opt instrument_id instrument_map with
             | Some value -> Ok value
             | None -> Error "mark has no configured instrument"
           in
           let* value = position_attribution instrument in
           Ok (value :: values))
         (Ok [])
    |> Result.map List.rev
  in
  let add = Scalar.Money.add in
  let* cash =
    List.fold_left
      (fun result item ->
        let* total = result in
        add total item.base_value)
      (Ok Scalar.Money.zero) cash_balances
  in
  let accumulate result item =
    let* ( net_market_value,
           long_market_value,
           short_market_value,
           cost_basis,
           realized_pnl,
           unrealized_pnl,
           dividend_pnl,
           execution_fees,
           borrow_fees,
           total_fees ) =
      result
    in
    let* net_market_value = add net_market_value item.base_market_value in
    let* long_market_value, short_market_value =
      if Scalar.Money.compare item.base_market_value Scalar.Money.zero >= 0 then
        let* long_market_value = add long_market_value item.base_market_value in
        Ok (long_market_value, short_market_value)
      else
        let* magnitude = Scalar.Money.negate item.base_market_value in
        let* short_market_value = add short_market_value magnitude in
        Ok (long_market_value, short_market_value)
    in
    let* cost_basis = add cost_basis item.base_cost_basis in
    let* realized_pnl = add realized_pnl item.base_realized_pnl in
    let* unrealized_pnl = add unrealized_pnl item.base_unrealized_pnl in
    let* dividend_pnl = add dividend_pnl item.base_dividend_pnl in
    let* execution_fees = add execution_fees item.base_execution_fees in
    let* borrow_fees = add borrow_fees item.base_borrow_fees in
    let* total_fees = add total_fees item.base_total_fees in
    Ok
      ( net_market_value,
        long_market_value,
        short_market_value,
        cost_basis,
        realized_pnl,
        unrealized_pnl,
        dividend_pnl,
        execution_fees,
        borrow_fees,
        total_fees )
  in
  let* ( net_market_value,
         long_market_value,
         short_market_value,
         cost_basis,
         realized_pnl,
         unrealized_pnl,
         dividend_pnl,
         execution_fees,
         borrow_fees,
         total_fees ) =
    List.fold_left accumulate
      (Ok
         ( Scalar.Money.zero,
           Scalar.Money.zero,
           Scalar.Money.zero,
           Scalar.Money.zero,
           Scalar.Money.zero,
           Scalar.Money.zero,
           Scalar.Money.zero,
           Scalar.Money.zero,
           Scalar.Money.zero,
           Scalar.Money.zero ))
      positions
  in
  let* gross_exposure = add long_market_value short_market_value in
  let* equity = add cash net_market_value in
  Ok
    {
      base_currency = state.base_currency;
      cash;
      net_market_value;
      long_market_value;
      short_market_value;
      gross_exposure;
      cost_basis;
      realized_pnl;
      unrealized_pnl;
      equity;
      dividend_pnl;
      execution_fees;
      borrow_fees;
      total_fees;
      cash_balances;
      positions;
    }

let pp_valuation formatter valuation =
  Format.fprintf formatter
    "cash=%a equity=%a gross=%a realized=%a unrealized=%a fees=%a"
    Scalar.Money.pp valuation.cash Scalar.Money.pp valuation.equity
    Scalar.Money.pp valuation.gross_exposure Scalar.Money.pp
    valuation.realized_pnl Scalar.Money.pp valuation.unrealized_pnl
    Scalar.Money.pp valuation.total_fees
