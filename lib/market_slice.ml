type fx_mark = { currency : string; rate : Scalar.Price.t }

type t = {
  slice_sequence : int64;
  start_at : Ptime.t;
  end_at : Ptime.t;
  available_at : Ptime.t;
  received_at : Ptime.t;
  bars : Bar.t list;
  fx_rates : fx_mark list;
  corporate_actions : Corporate_action.t list;
  borrow_observations : Financing.borrow_observation list;
  cash_rate_observations : Financing.cash_rate_observation list;
}

let valid_currency value =
  String.length value > 0
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x21 && code <> 0x7f)
       value

let fx_mark ~currency ~rate =
  if not (valid_currency currency) then
    Error "FX currency must not be empty or contain whitespace"
  else Ok { currency; rate }

let compare_bar left right =
  Id.Instrument.compare left.Bar.instrument_id right.Bar.instrument_id

let create_v10 ~slice_sequence ~start_at ~end_at ~available_at ~received_at
    ~bars ~fx_rates ~corporate_actions ~borrow_observations
    ~cash_rate_observations =
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
    let fx_rates =
      List.sort
        (fun left right -> String.compare left.currency right.currency)
        fx_rates
    in
    let rec unique_fx = function
      | [] | [ _ ] -> true
      | left :: (right :: _ as remaining) ->
          (not (String.equal left.currency right.currency))
          && unique_fx remaining
    in
    let corporate_actions =
      List.sort Corporate_action.compare corporate_actions
    in
    let rec unique_actions = function
      | [] | [ _ ] -> true
      | left :: (right :: _ as remaining) ->
          (not (Id.Corporate_action.equal left.Corporate_action.id right.id))
          && unique_actions remaining
    in
    let borrow_observations =
      List.sort
        (fun (left : Financing.borrow_observation)
             (right : Financing.borrow_observation) ->
          Id.Instrument.compare left.Financing.instrument_id
            right.Financing.instrument_id)
        borrow_observations
    in
    let rec unique_borrow = function
      | [] | [ _ ] -> true
      | left :: (right :: _ as remaining) ->
          (not
             (Id.Instrument.equal left.Financing.instrument_id
                right.Financing.instrument_id))
          && unique_borrow remaining
    in
    let cash_rate_observations =
      List.sort
        (fun (left : Financing.cash_rate_observation)
             (right : Financing.cash_rate_observation) ->
          String.compare left.Financing.currency right.Financing.currency)
        cash_rate_observations
    in
    let rec unique_cash_rate = function
      | [] | [ _ ] -> true
      | left :: (right :: _ as remaining) ->
          (not (String.equal left.Financing.currency right.Financing.currency))
          && unique_cash_rate remaining
    in
    if not (unique bars) then
      Error "market slice must contain one bar per instrument"
    else if fx_rates = [] then Error "market slice must contain FX rates"
    else if not (unique_fx fx_rates) then
      Error "market slice must contain one FX rate per currency"
    else if not (unique_actions corporate_actions) then
      Error "market slice corporate action IDs must be unique"
    else if not (unique_borrow borrow_observations) then
      Error "market slice borrow observation instrument IDs must be unique"
    else if not (unique_cash_rate cash_rate_observations) then
      Error "market slice cash rate currencies must be unique"
    else
      Ok
        {
          slice_sequence;
          start_at;
          end_at;
          available_at;
          received_at;
          bars;
          fx_rates;
          corporate_actions;
          borrow_observations;
          cash_rate_observations;
        }

let create ~slice_sequence ~start_at ~end_at ~available_at ~received_at ~bars
    ~fx_rates ~corporate_actions =
  create_v10 ~slice_sequence ~start_at ~end_at ~available_at ~received_at ~bars
    ~fx_rates ~corporate_actions ~borrow_observations:[]
    ~cash_rate_observations:[]

let bar state instrument_id =
  List.find_opt
    (fun bar -> Id.Instrument.equal bar.Bar.instrument_id instrument_id)
    state.bars

let fx_rate state currency =
  List.find_opt (fun mark -> String.equal mark.currency currency) state.fx_rates
  |> Option.map (fun mark -> mark.rate)

let compare_replay_order left right =
  let receipt = Ptime.compare left.received_at right.received_at in
  if receipt <> 0 then receipt
  else Int64.compare left.slice_sequence right.slice_sequence

let pp formatter state =
  Format.fprintf formatter
    "slice[%Ld] bars=%d fx=%d actions=%d borrow=%d cash_rates=%d"
    state.slice_sequence (List.length state.bars)
    (List.length state.fx_rates)
    (List.length state.corporate_actions)
    (List.length state.borrow_observations)
    (List.length state.cash_rate_observations)
