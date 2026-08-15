type position = {
  quantity : Scalar.Quantity.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  total_fees : Scalar.Money.t;
}

type position_attribution = {
  instrument_id : Id.Instrument.t;
  quantity : Scalar.Quantity.t;
  mark : Scalar.Price.t;
  market_value : Scalar.Money.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  unrealized_pnl : Scalar.Money.t;
  total_fees : Scalar.Money.t;
}

type t = {
  initial_cash : Scalar.Money.t;
  cash : Scalar.Money.t;
  positions : position Id.Instrument.Map.t;
  realized_pnl : Scalar.Money.t;
  total_fees : Scalar.Money.t;
}

type valuation = {
  cash : Scalar.Money.t;
  market_value : Scalar.Money.t;
  cost_basis : Scalar.Money.t;
  realized_pnl : Scalar.Money.t;
  unrealized_pnl : Scalar.Money.t;
  equity : Scalar.Money.t;
  total_fees : Scalar.Money.t;
  positions : position_attribution list;
}

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let empty_position =
  {
    quantity = Scalar.Quantity.zero;
    cost_basis = Scalar.Money.zero;
    realized_pnl = Scalar.Money.zero;
    total_fees = Scalar.Money.zero;
  }

let create ~initial_cash =
  {
    initial_cash;
    cash = initial_cash;
    positions = Id.Instrument.Map.empty;
    realized_pnl = Scalar.Money.zero;
    total_fees = Scalar.Money.zero;
  }

let initial_cash (state : t) = state.initial_cash
let cash (state : t) = state.cash
let realized_pnl (state : t) = state.realized_pnl
let total_fees (state : t) = state.total_fees

let position (state : t) instrument_id =
  Option.value
    (Id.Instrument.Map.find_opt instrument_id state.positions)
    ~default:empty_position

let position_quantity (state : t) instrument_id =
  (position state instrument_id).quantity

let positions (state : t) = Id.Instrument.Map.bindings state.positions

let update_position (positions : position Id.Instrument.Map.t) instrument_id
    (value : position) =
  if
    Scalar.Quantity.is_zero value.quantity
    && Scalar.Money.equal value.cost_basis Scalar.Money.zero
    && Scalar.Money.equal value.realized_pnl Scalar.Money.zero
    && Scalar.Money.equal value.total_fees Scalar.Money.zero
  then Id.Instrument.Map.remove instrument_id positions
  else Id.Instrument.Map.add instrument_id value positions

let apply_fee (state : t) fee =
  match Scalar.Money.add state.total_fees fee with
  | Error _ as error -> error
  | Ok total_fees -> Ok { state with total_fees }

let apply_buy (state : t) (fill : Fill.t) (current : position) =
  let* quantity = Scalar.Quantity.add current.quantity fill.Fill.quantity in
  let* acquisition_cost = Scalar.Money.add fill.notional fill.fee in
  if Scalar.Money.compare acquisition_cost state.cash > 0 then
    Error "buy fill exceeds available cash"
  else
    let* cost_basis = Scalar.Money.add current.cost_basis acquisition_cost in
    let* position_fees = Scalar.Money.add current.total_fees fill.fee in
    let* cash = Scalar.Money.subtract state.cash acquisition_cost in
    let positions =
      update_position state.positions fill.instrument_id
        {
          quantity;
          cost_basis;
          realized_pnl = current.realized_pnl;
          total_fees = position_fees;
        }
    in
    apply_fee { state with cash; positions } fill.fee

let apply_sell (state : t) (fill : Fill.t) (current : position) =
  if Scalar.Quantity.compare fill.Fill.quantity current.quantity > 0 then
    Error "sell fill exceeds the long position"
  else
    let remaining =
      match Scalar.Quantity.subtract current.quantity fill.quantity with
      | Ok value -> value
      | Error message -> failwith message
    in
    let* removed_basis =
      if Scalar.Quantity.is_zero remaining then Ok current.cost_basis
      else
        Scalar.Money.proportion_floor current.cost_basis
          ~numerator:fill.quantity ~denominator:current.quantity
    in
    let* cost_basis = Scalar.Money.subtract current.cost_basis removed_basis in
    let* net_proceeds = Scalar.Money.subtract fill.notional fill.fee in
    let* cash = Scalar.Money.add state.cash net_proceeds in
    if Scalar.Money.compare cash Scalar.Money.zero < 0 then
      Error "sell fill fee exceeds available cash and proceeds"
    else
      let* realized_delta = Scalar.Money.subtract net_proceeds removed_basis in
      let* realized_pnl = Scalar.Money.add state.realized_pnl realized_delta in
      let* position_realized =
        Scalar.Money.add current.realized_pnl realized_delta
      in
      let* position_fees = Scalar.Money.add current.total_fees fill.fee in
      let positions =
        update_position state.positions fill.instrument_id
          {
            quantity = remaining;
            cost_basis;
            realized_pnl = position_realized;
            total_fees = position_fees;
          }
      in
      apply_fee { state with cash; positions; realized_pnl } fill.fee

let apply_fill (state : t) (fill : Fill.t) =
  let current = position state fill.Fill.instrument_id in
  match fill.side with
  | Order.Buy -> apply_buy state fill current
  | Order.Sell -> apply_sell state fill current

let value (state : t) ~marks =
  let marks =
    List.fold_left
      (fun map (instrument_id, price) ->
        Id.Instrument.Map.add instrument_id price map)
      Id.Instrument.Map.empty marks
  in
  let attribution instrument_id price =
    let position = position state instrument_id in
    let* market_value = Scalar.Money.notional price position.quantity in
    let* unrealized_pnl =
      Scalar.Money.subtract market_value position.cost_basis
    in
    Ok
      {
        instrument_id;
        quantity = position.quantity;
        mark = price;
        market_value;
        cost_basis = position.cost_basis;
        realized_pnl = position.realized_pnl;
        unrealized_pnl;
        total_fees = position.total_fees;
      }
  in
  let* positions =
    Id.Instrument.Map.bindings marks
    |> List.fold_left
         (fun result (instrument_id, price) ->
           let* positions = result in
           let* value = attribution instrument_id price in
           Ok (value :: positions))
         (Ok [])
    |> Result.map List.rev
  in
  let missing_mark =
    Id.Instrument.Map.bindings state.positions
    |> List.find_opt (fun (instrument_id, (position : position)) ->
        (not (Id.Instrument.Map.mem instrument_id marks))
        && not (Scalar.Quantity.is_zero position.quantity))
  in
  let* () =
    match missing_mark with
    | None -> Ok ()
    | Some (instrument_id, _) ->
        Error
          (Format.asprintf "missing mark for held instrument %a"
             Id.Instrument.pp instrument_id)
  in
  let accumulate result (position : position_attribution) =
    let* market_value, cost_basis, realized_pnl, total_fees = result in
    let* market_value = Scalar.Money.add market_value position.market_value in
    let* cost_basis = Scalar.Money.add cost_basis position.cost_basis in
    let* realized_pnl = Scalar.Money.add realized_pnl position.realized_pnl in
    let* total_fees = Scalar.Money.add total_fees position.total_fees in
    Ok (market_value, cost_basis, realized_pnl, total_fees)
  in
  let* market_value, cost_basis, realized_pnl, total_fees =
    List.fold_left accumulate
      (Ok
         ( Scalar.Money.zero,
           Scalar.Money.zero,
           Scalar.Money.zero,
           Scalar.Money.zero ))
      positions
  in
  if
    (not (Scalar.Money.equal realized_pnl state.realized_pnl))
    || not (Scalar.Money.equal total_fees state.total_fees)
  then Error "position attribution does not reconcile with account totals"
  else
    let* unrealized_pnl = Scalar.Money.subtract market_value cost_basis in
    let* equity = Scalar.Money.add state.cash market_value in
    Ok
      {
        cash = state.cash;
        market_value;
        cost_basis;
        realized_pnl = state.realized_pnl;
        unrealized_pnl;
        equity;
        total_fees = state.total_fees;
        positions;
      }

let pp_valuation formatter valuation =
  Format.fprintf formatter "cash=%a equity=%a realized=%a unrealized=%a fees=%a"
    Scalar.Money.pp valuation.cash Scalar.Money.pp valuation.equity
    Scalar.Money.pp valuation.realized_pnl Scalar.Money.pp
    valuation.unrealized_pnl Scalar.Money.pp valuation.total_fees
