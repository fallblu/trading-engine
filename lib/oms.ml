type t = { orders : Order.t Id.Order.Map.t; fills : Fill.t Id.Fill.Map.t }
type fill_outcome = Applied of Order.t | Duplicate

let empty = { orders = Id.Order.Map.empty; fills = Id.Fill.Map.empty }
let find state order_id = Id.Order.Map.find_opt order_id state.orders
let orders state = Id.Order.Map.bindings state.orders |> List.map snd

let compare_active left right =
  let sequence =
    Int64.compare left.Order.created_sequence right.Order.created_sequence
  in
  if sequence <> 0 then sequence else Id.Order.compare left.id right.id

let active_orders state =
  List.filter Order.is_active (orders state) |> List.sort compare_active

let active_for_instrument state instrument_id =
  active_orders state
  |> List.filter (fun order ->
      Id.Instrument.equal order.Order.request.instrument_id instrument_id)

let ensure_new_id state order_id =
  if Id.Order.Map.mem order_id state.orders then Error "order ID already exists"
  else Ok ()

let insert state order =
  { state with orders = Id.Order.Map.add order.Order.id order state.orders }

let accept state ~id ~created_event_id ~accepted_sequence ~created_at
    ~eligible_after_slice_sequence request =
  match ensure_new_id state id with
  | Error _ as error -> error
  | Ok () -> (
      match
        Order.accept ~id ~created_event_id ~accepted_sequence ~created_at
          ~eligible_after_slice_sequence request
      with
      | Error _ as error -> error
      | Ok order -> Ok (insert state order, order))

let reject state ~id ~created_event_id ~rejected_sequence ~created_at
    ~eligible_after_slice_sequence request ~reason =
  match ensure_new_id state id with
  | Error _ as error -> error
  | Ok () -> (
      match
        Order.reject ~id ~created_event_id ~rejected_sequence ~created_at
          ~eligible_after_slice_sequence request ~reason
      with
      | Error _ as error -> error
      | Ok order -> Ok (insert state order, order))

let cancel state order_id =
  match find state order_id with
  | None -> Error "cannot cancel an unknown order"
  | Some order -> (
      match Order.cancel order with
      | Error _ as error -> error
      | Ok cancelled -> Ok (insert state cancelled, cancelled))

let adjust_for_split state ~instrument_id ~updated_event_ids ~numerator
    ~denominator =
  let active = active_for_instrument state instrument_id in
  if List.length active <> List.length updated_event_ids then
    Error "split adjustment event IDs must cover every active order"
  else
    let step result (order, updated_event_id) =
      match result with
      | Error _ as error -> error
      | Ok (state, adjusted) -> (
          match
            Order.adjust_for_split order ~updated_event_id ~numerator
              ~denominator
          with
          | Error _ as error -> error
          | Ok order -> Ok (insert state order, order :: adjusted))
    in
    List.combine active updated_event_ids
    |> List.fold_left step (Ok (state, []))
    |> Result.map (fun (state, adjusted) -> (state, List.rev adjusted))

let apply_fill state fill =
  match Id.Fill.Map.find_opt fill.Fill.id state.fills with
  | Some existing when Fill.equal existing fill -> Ok (state, Duplicate)
  | Some _ -> Error "fill ID conflicts with a different execution report"
  | None -> (
      match find state fill.order_id with
      | None -> Error "fill refers to an unknown order"
      | Some order -> (
          if
            not
              (Id.Instrument.equal order.request.instrument_id
                 fill.instrument_id)
          then Error "fill instrument differs from its order"
          else if order.request.side <> fill.side then
            Error "fill side differs from its order"
          else if Ptime.compare fill.executed_at order.created_at < 0 then
            Error "fill execution time predates its order"
          else
            match
              Order.apply_fill order ~quantity:fill.quantity
                ~notional:fill.notional
            with
            | Error _ as error -> error
            | Ok updated ->
                let state = insert state updated in
                let state =
                  {
                    state with
                    fills = Id.Fill.Map.add fill.id fill state.fills;
                  }
                in
                Ok (state, Applied updated)))
