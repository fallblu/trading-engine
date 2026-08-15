type position = { quantity : Scalar.Quantity.t; cost_basis : Scalar.Money.t }

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
}

let empty_position =
  { quantity = Scalar.Quantity.zero; cost_basis = Scalar.Money.zero }

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

let position state instrument_id =
  Option.value
    (Id.Instrument.Map.find_opt instrument_id state.positions)
    ~default:empty_position

let position_quantity state instrument_id =
  (position state instrument_id).quantity

let positions state = Id.Instrument.Map.bindings state.positions

let update_position positions instrument_id value =
  if Scalar.Quantity.is_zero value.quantity then
    Id.Instrument.Map.remove instrument_id positions
  else Id.Instrument.Map.add instrument_id value positions

let apply_fee (state : t) fee =
  match Scalar.Money.add state.total_fees fee with
  | Error _ as error -> error
  | Ok total_fees -> Ok { state with total_fees }

let apply_buy (state : t) (fill : Fill.t) (current : position) =
  match Scalar.Quantity.add current.quantity fill.Fill.quantity with
  | Error _ as error -> error
  | Ok quantity -> (
      match Scalar.Money.add fill.notional fill.fee with
      | Error _ as error -> error
      | Ok acquisition_cost -> (
          if Scalar.Money.compare acquisition_cost state.cash > 0 then
            Error "buy fill exceeds available cash"
          else
            match Scalar.Money.add current.cost_basis acquisition_cost with
            | Error _ as error -> error
            | Ok cost_basis -> (
                match Scalar.Money.subtract state.cash acquisition_cost with
                | Error _ as error -> error
                | Ok cash ->
                    let positions =
                      update_position state.positions fill.instrument_id
                        { quantity; cost_basis }
                    in
                    apply_fee { state with cash; positions } fill.fee)))

let apply_sell (state : t) (fill : Fill.t) (current : position) =
  if Scalar.Quantity.compare fill.Fill.quantity current.quantity > 0 then
    Error "sell fill exceeds the long position"
  else
    let remaining =
      match Scalar.Quantity.subtract current.quantity fill.quantity with
      | Ok value -> value
      | Error message -> failwith message
    in
    let removed_basis_result =
      if Scalar.Quantity.is_zero remaining then Ok current.cost_basis
      else
        Scalar.Money.proportion_floor current.cost_basis
          ~numerator:fill.quantity ~denominator:current.quantity
    in
    match removed_basis_result with
    | Error _ as error -> error
    | Ok removed_basis -> (
        match Scalar.Money.subtract current.cost_basis removed_basis with
        | Error _ as error -> error
        | Ok cost_basis -> (
            match Scalar.Money.subtract fill.notional fill.fee with
            | Error _ as error -> error
            | Ok net_proceeds -> (
                match Scalar.Money.add state.cash net_proceeds with
                | Error _ as error -> error
                | Ok cash when Scalar.Money.compare cash Scalar.Money.zero < 0
                  ->
                    Error "sell fill fee exceeds available cash and proceeds"
                | Ok cash -> (
                    match Scalar.Money.subtract net_proceeds removed_basis with
                    | Error _ as error -> error
                    | Ok realized_delta -> (
                        match
                          Scalar.Money.add state.realized_pnl realized_delta
                        with
                        | Error _ as error -> error
                        | Ok realized_pnl ->
                            let positions =
                              update_position state.positions fill.instrument_id
                                { quantity = remaining; cost_basis }
                            in
                            apply_fee
                              { state with cash; positions; realized_pnl }
                              fill.fee)))))

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
  let accumulate instrument_id position result =
    match result with
    | Error _ as error -> error
    | Ok (market_value, cost_basis) -> (
        match Id.Instrument.Map.find_opt instrument_id marks with
        | None ->
            Error
              (Format.asprintf "missing mark for held instrument %a"
                 Id.Instrument.pp instrument_id)
        | Some price -> (
            match Scalar.Money.notional price position.quantity with
            | Error _ as error -> error
            | Ok value -> (
                match Scalar.Money.add market_value value with
                | Error _ as error -> error
                | Ok market_value -> (
                    match Scalar.Money.add cost_basis position.cost_basis with
                    | Error _ as error -> error
                    | Ok cost_basis -> Ok (market_value, cost_basis)))))
  in
  match
    Id.Instrument.Map.fold accumulate state.positions
      (Ok (Scalar.Money.zero, Scalar.Money.zero))
  with
  | Error _ as error -> error
  | Ok (market_value, cost_basis) -> (
      match Scalar.Money.subtract market_value cost_basis with
      | Error _ as error -> error
      | Ok unrealized_pnl -> (
          match Scalar.Money.add state.cash market_value with
          | Error _ as error -> error
          | Ok equity ->
              Ok
                {
                  cash = state.cash;
                  market_value;
                  cost_basis;
                  realized_pnl = state.realized_pnl;
                  unrealized_pnl;
                  equity;
                  total_fees = state.total_fees;
                }))

let pp_valuation formatter valuation =
  Format.fprintf formatter "cash=%a equity=%a realized=%a unrealized=%a fees=%a"
    Scalar.Money.pp valuation.cash Scalar.Money.pp valuation.equity
    Scalar.Money.pp valuation.realized_pnl Scalar.Money.pp
    valuation.unrealized_pnl Scalar.Money.pp valuation.total_fees
