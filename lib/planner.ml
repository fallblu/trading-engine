type plan = {
  cancel_orders : Id.Order.t list;
  submit_order : Order.request option;
}

let target_position ~account ~oms ~instrument_id ~target =
  let active = Oms.active_for_instrument oms instrument_id in
  let direct =
    List.filter (fun order -> order.Order.request.origin = Order.Direct) active
  in
  if direct <> [] then
    Error
      "target position conflicts with an active direct order for the instrument"
  else
    let cancel_orders =
      active
      |> List.filter (fun order ->
          order.Order.request.origin = Order.Target_rebalance)
      |> List.map (fun order -> order.Order.id)
    in
    let current = Account.position_quantity account instrument_id in
    if Scalar.Quantity.equal current target then
      Ok { cancel_orders; submit_order = None }
    else
      let side, quantity_result =
        if Scalar.Quantity.compare target current > 0 then
          (Order.Buy, Scalar.Quantity.subtract target current)
        else (Order.Sell, Scalar.Quantity.subtract current target)
      in
      match quantity_result with
      | Error _ as error -> error
      | Ok quantity -> (
          match
            Order.request ~instrument_id ~side ~quantity ~kind:Order.Market
              ~origin:Order.Target_rebalance
          with
          | Error _ as error -> error
          | Ok request -> Ok { cancel_orders; submit_order = Some request })
