type side = Buy | Sell
type kind = Market | Limit of Scalar.Price.t
type origin = Direct | Target_rebalance

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

type t = {
  id : Id.Order.t;
  request : request;
  created_sequence : int64;
  created_at : Ptime.t;
  eligible_after_bar_sequence : int64;
  filled_quantity : Scalar.Quantity.t;
  filled_notional : Scalar.Money.t;
  status : status;
}

let request ~instrument_id ~side ~quantity ~kind ~origin =
  if Scalar.Quantity.is_zero quantity then
    Error "order quantity must be positive"
  else Ok { instrument_id; side; quantity; kind; origin }

let make ~id ~sequence ~created_at ~eligible_after_bar_sequence ~request ~status
    =
  if Int64.compare sequence 0L < 0 then
    Error "order sequence must be nonnegative"
  else if Int64.compare eligible_after_bar_sequence 0L < 0 then
    Error "order eligibility sequence must be nonnegative"
  else
    Ok
      {
        id;
        request;
        created_sequence = sequence;
        created_at;
        eligible_after_bar_sequence;
        filled_quantity = Scalar.Quantity.zero;
        filled_notional = Scalar.Money.zero;
        status;
      }

let accept ~id ~accepted_sequence ~created_at ~eligible_after_bar_sequence
    request =
  make ~id ~sequence:accepted_sequence ~created_at ~eligible_after_bar_sequence
    ~request ~status:Working

let reject ~id ~rejected_sequence ~created_at ~eligible_after_bar_sequence
    request ~reason =
  if String.length reason = 0 then
    Error "order rejection reason must not be empty"
  else
    make ~id ~sequence:rejected_sequence ~created_at
      ~eligible_after_bar_sequence ~request ~status:(Rejected reason)

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
  else if Scalar.Quantity.is_zero quantity then
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

let side_to_string = function Buy -> "buy" | Sell -> "sell"

let kind_to_string = function
  | Market -> "market"
  | Limit price -> "limit@" ^ Scalar.Price.to_decimal_string price

let origin_to_string = function
  | Direct -> "direct"
  | Target_rebalance -> "target_rebalance"

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
