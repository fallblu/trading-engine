type side = Bid | Ask
type level = { price : Scalar.Price.t; quantity : Scalar.Quantity.t }

type kind =
  | Snapshot of { bids : level list; asks : level list }
  | Set of { side : side; price : Scalar.Price.t; quantity : Scalar.Quantity.t }
  | Delete of { side : side; price : Scalar.Price.t }
  | Trade of {
      price : Scalar.Price.t;
      quantity : Scalar.Quantity.t;
      aggressor_side : Market_event.aggressor_side;
    }

type t = {
  instrument_id : Id.Instrument.t;
  event_at : Ptime.t;
  available_at : Ptime.t;
  received_at : Ptime.t;
  ingest_sequence : int64;
  book_sequence : int64;
  kind : kind;
}

let level ~price ~quantity =
  if Scalar.Quantity.is_zero quantity then
    Error "order-book level quantity must be positive"
  else Ok { price; quantity }

let validate_common ~event_at ~available_at ~received_at ~ingest_sequence
    ~book_sequence =
  if Int64.compare ingest_sequence 0L <= 0 then
    Error "order-book ingest sequence must be positive"
  else if Int64.compare book_sequence 0L <= 0 then
    Error "order-book sequence must be positive"
  else if Ptime.compare available_at event_at < 0 then
    Error "order-book availability must not precede event time"
  else if Ptime.compare received_at available_at < 0 then
    Error "order-book receipt must not precede availability"
  else Ok ()

let ordered_levels side levels =
  let rec ordered = function
    | [] | [ _ ] -> true
    | left :: (right :: _ as remaining) ->
        let comparison = Scalar.Price.compare left.price right.price in
        (match side with Bid -> comparison > 0 | Ask -> comparison < 0)
        && ordered remaining
  in
  ordered levels

let snapshot ~instrument_id ~event_at ~available_at ~received_at
    ~ingest_sequence ~book_sequence ~bids ~asks =
  let ( let* ) = Result.bind in
  let* () =
    validate_common ~event_at ~available_at ~received_at ~ingest_sequence
      ~book_sequence
  in
  if bids = [] || asks = [] then
    Error "order-book snapshot must contain bid and ask depth"
  else if not (ordered_levels Bid bids && ordered_levels Ask asks) then
    Error "order-book snapshot levels must be unique and price ordered"
  else if Scalar.Price.compare (List.hd bids).price (List.hd asks).price > 0
  then Error "crossed order-book snapshot is invalid"
  else
    Ok
      {
        instrument_id;
        event_at;
        available_at;
        received_at;
        ingest_sequence;
        book_sequence;
        kind = Snapshot { bids; asks };
      }

let create_change kind ~instrument_id ~event_at ~available_at ~received_at
    ~ingest_sequence ~book_sequence =
  Result.map
    (fun () ->
      {
        instrument_id;
        event_at;
        available_at;
        received_at;
        ingest_sequence;
        book_sequence;
        kind;
      })
    (validate_common ~event_at ~available_at ~received_at ~ingest_sequence
       ~book_sequence)

let set ~instrument_id ~event_at ~available_at ~received_at ~ingest_sequence
    ~book_sequence ~side ~price ~quantity =
  if Scalar.Quantity.is_zero quantity then
    Error "order-book set quantity must be positive"
  else
    create_change
      (Set { side; price; quantity })
      ~instrument_id ~event_at ~available_at ~received_at ~ingest_sequence
      ~book_sequence

let delete ~instrument_id ~event_at ~available_at ~received_at ~ingest_sequence
    ~book_sequence ~side ~price =
  create_change
    (Delete { side; price })
    ~instrument_id ~event_at ~available_at ~received_at ~ingest_sequence
    ~book_sequence

let trade ~instrument_id ~event_at ~available_at ~received_at ~ingest_sequence
    ~book_sequence ~price ~quantity ~aggressor_side =
  if Scalar.Quantity.is_zero quantity then
    Error "order-book trade quantity must be positive"
  else
    create_change
      (Trade { price; quantity; aggressor_side })
      ~instrument_id ~event_at ~available_at ~received_at ~ingest_sequence
      ~book_sequence

let compare_replay_order left right =
  let availability = Ptime.compare left.available_at right.available_at in
  if availability <> 0 then availability
  else
    let receipt = Ptime.compare left.received_at right.received_at in
    if receipt <> 0 then receipt
    else Int64.compare left.ingest_sequence right.ingest_sequence

let side_to_string = function Bid -> "bid" | Ask -> "ask"

let side_of_string = function
  | "bid" -> Ok Bid
  | "ask" -> Ok Ask
  | _ -> Error "order-book side must be bid or ask"
