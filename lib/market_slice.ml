type t = {
  slice_sequence : int64;
  start_at : Ptime.t;
  end_at : Ptime.t;
  available_at : Ptime.t;
  received_at : Ptime.t;
  bars : Bar.t list;
}

let compare_bar left right =
  Id.Instrument.compare left.Bar.instrument_id right.Bar.instrument_id

let create ~slice_sequence ~start_at ~end_at ~available_at ~received_at ~bars =
  if Int64.compare slice_sequence 0L <= 0 then
    Error "market slice sequence must be positive"
  else if Ptime.compare start_at end_at >= 0 then
    Error "market slice start must precede its end"
  else if Ptime.compare available_at end_at < 0 then
    Error "market slice availability must not precede its end"
  else if Ptime.compare received_at available_at < 0 then
    Error "market slice receipt must not precede its availability"
  else if bars = [] then Error "market slice must contain at least one bar"
  else
    let bars = List.sort compare_bar bars in
    let rec unique = function
      | [] | [ _ ] -> true
      | left :: (right :: _ as remaining) ->
          (not (Id.Instrument.equal left.Bar.instrument_id right.instrument_id))
          && unique remaining
    in
    if not (unique bars) then
      Error "market slice must contain one bar per instrument"
    else
      Ok { slice_sequence; start_at; end_at; available_at; received_at; bars }

let bar state instrument_id =
  List.find_opt
    (fun bar -> Id.Instrument.equal bar.Bar.instrument_id instrument_id)
    state.bars

let compare_replay_order left right =
  let receipt = Ptime.compare left.received_at right.received_at in
  if receipt <> 0 then receipt
  else Int64.compare left.slice_sequence right.slice_sequence

let pp formatter state =
  Format.fprintf formatter "slice[%Ld] bars=%d" state.slice_sequence
    (List.length state.bars)
