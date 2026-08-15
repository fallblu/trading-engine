type side = Buy | Sell
type kind = Market | Limit of Scalar.Price.t
type origin = Direct | Target_rebalance | Margin_liquidation

type request = {
  instrument_id : Id.Instrument.t;
  side : side;
  quantity : Scalar.Quantity.t;
  kind : kind;
  origin : origin;
}

type status =
  | Working
  | Partially_filled
  | Filled
  | Cancelled
  | Rejected of string

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

type t = {
  id : Id.Order.t;
  request : request;
  created_event_id : Id.Event.t;
  updated_event_id : Id.Event.t;
  created_sequence : int64;
  created_at : Ptime.t;
  eligible_after_slice_sequence : int64;
  filled_quantity : Scalar.Quantity.t;
  filled_notional : Scalar.Money.t;
  status : status;
}

let request ~instrument_id ~side ~quantity ~kind ~origin =
  if not (Scalar.Quantity.is_positive quantity) then
    Error "order quantity must be positive"
  else Ok { instrument_id; side; quantity; kind; origin }

let make ~id ~created_event_id ~sequence ~created_at
    ~eligible_after_slice_sequence ~request ~status =
  if Int64.compare sequence 0L < 0 then
    Error "order sequence must be nonnegative"
  else if Int64.compare eligible_after_slice_sequence 0L < 0 then
    Error "order eligibility sequence must be nonnegative"
  else
    Ok
      {
        id;
        request;
        created_event_id;
        updated_event_id = created_event_id;
        created_sequence = sequence;
        created_at;
        eligible_after_slice_sequence;
        filled_quantity = Scalar.Quantity.zero;
        filled_notional = Scalar.Money.zero;
        status;
      }

let accept ~id ~created_event_id ~accepted_sequence ~created_at
    ~eligible_after_slice_sequence request =
  make ~id ~created_event_id ~sequence:accepted_sequence ~created_at
    ~eligible_after_slice_sequence ~request ~status:Working

let reject ~id ~created_event_id ~rejected_sequence ~created_at
    ~eligible_after_slice_sequence request ~reason =
  if String.length reason = 0 then
    Error "order rejection reason must not be empty"
  else
    make ~id ~created_event_id ~sequence:rejected_sequence ~created_at
      ~eligible_after_slice_sequence ~request ~status:(Rejected reason)

let remaining_quantity order =
  match
    Scalar.Quantity.subtract order.request.quantity order.filled_quantity
  with
  | Ok quantity -> quantity
  | Error _ ->
      failwith "validated order fill quantity exceeds its requested quantity"

let is_active order =
  match order.status with
  | Working | Partially_filled -> true
  | Filled | Cancelled | Rejected _ -> false

let is_terminal order = not (is_active order)

let is_market order =
  match order.request.kind with Market -> true | Limit _ -> false

let apply_fill order ~quantity ~notional =
  if not (is_active order) then Error "cannot fill a terminal order"
  else if not (Scalar.Quantity.is_positive quantity) then
    Error "fill quantity must be positive"
  else
    let remaining = remaining_quantity order in
    if Scalar.Quantity.compare quantity remaining > 0 then
      Error "fill quantity exceeds the order remainder"
    else
      match Scalar.Quantity.add order.filled_quantity quantity with
      | Error _ as error -> error
      | Ok filled_quantity -> (
          match Scalar.Money.add order.filled_notional notional with
          | Error _ as error -> error
          | Ok filled_notional ->
              let status =
                if Scalar.Quantity.equal filled_quantity order.request.quantity
                then Filled
                else Partially_filled
              in
              Ok { order with filled_quantity; filled_notional; status })

let cancel order =
  if is_active order then Ok { order with status = Cancelled }
  else Error "cannot cancel a terminal order"

let adjust_for_split order ~updated_event_id ~numerator ~denominator =
  if not (is_active order) then Error "cannot split-adjust a terminal order"
  else
    let adjust_quantity value =
      Scalar.Quantity.scale_ratio_exact value ~numerator ~denominator
    in
    let* quantity = adjust_quantity order.request.quantity in
    let* filled_quantity = adjust_quantity order.filled_quantity in
    let* kind =
      match order.request.kind with
      | Market -> Ok Market
      | Limit price ->
          Scalar.Price.scale_ratio_exact price ~numerator:denominator
            ~denominator:numerator
          |> Result.map (fun price -> Limit price)
    in
    if not (Scalar.Quantity.is_positive quantity) then
      Error "split-adjusted order quantity must be positive"
    else
      Ok
        {
          order with
          request = { order.request with quantity; kind };
          updated_event_id;
          filled_quantity;
        }

let side_to_string = function Buy -> "buy" | Sell -> "sell"

let kind_to_string = function
  | Market -> "market"
  | Limit price -> "limit@" ^ Scalar.Price.to_decimal_string price

let origin_to_string = function
  | Direct -> "direct"
  | Target_rebalance -> "target_rebalance"
  | Margin_liquidation -> "margin_liquidation"

let status_to_string = function
  | Working -> "working"
  | Partially_filled -> "partially_filled"
  | Filled -> "filled"
  | Cancelled -> "cancelled"
  | Rejected _ -> "rejected"

let pp formatter order =
  Format.fprintf formatter "%a %s %a %a %s" Id.Order.pp order.id
    (side_to_string order.request.side)
    Scalar.Quantity.pp order.request.quantity Id.Instrument.pp
    order.request.instrument_id
    (status_to_string order.status)
