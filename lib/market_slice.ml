type fx_mark = { currency : string; rate : Scalar.Price.t }

type t = {
  slice_sequence : int64;
  start_at : Ptime.t;
  end_at : Ptime.t;
  available_at : Ptime.t;
  received_at : Ptime.t;
  bars : Bar.t list;
  market_events : Market_event.t list;
  order_book_events : Order_book_event.t list;
  fx_rates : fx_mark list;
  corporate_actions : Corporate_action.t list;
  lifecycle_events : Instrument_lifecycle.event list;
  borrow_observations : Financing.borrow_observation list;
  cash_rate_observations : Financing.cash_rate_observation list;
  settlement_failures : Settlement.failure list;
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

let create_v15 ~slice_sequence ~start_at ~end_at ~available_at ~received_at
    ~bars ~fx_rates ~corporate_actions ~borrow_observations
    ~cash_rate_observations ~settlement_failures ~lifecycle_events
    ~market_events ~order_book_events =
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
    let lifecycle_events =
      List.sort Instrument_lifecycle.compare_event lifecycle_events
    in
    let rec unique_lifecycle = function
      | [] | [ _ ] -> true
      | left :: (right :: _ as remaining) ->
          (not
             (Id.Corporate_action.equal left.Instrument_lifecycle.id right.id))
          && unique_lifecycle remaining
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
    let settlement_failures =
      List.sort
        (fun (left : Settlement.failure) (right : Settlement.failure) ->
          String.compare left.instruction_id right.instruction_id)
        settlement_failures
    in
    let rec unique_failure = function
      | [] | [ _ ] -> true
      | left :: (right :: _ as remaining) ->
          (not
             (String.equal left.Settlement.instruction_id right.instruction_id))
          && unique_failure remaining
    in
    let rec ordered_events = function
      | [] | [ _ ] -> true
      | left :: (right :: _ as remaining) ->
          Market_event.compare_replay_order left right < 0
          && Int64.compare left.Market_event.ingest_sequence
               right.Market_event.ingest_sequence
             < 0
          && ordered_events remaining
    in
    let rec ordered_book_events = function
      | [] | [ _ ] -> true
      | left :: (right :: _ as remaining) ->
          Order_book_event.compare_replay_order left right < 0
          && Int64.compare left.Order_book_event.ingest_sequence
               right.Order_book_event.ingest_sequence
             < 0
          && ordered_book_events remaining
    in
    if not (unique bars) then
      Error "market slice must contain one bar per instrument"
    else if fx_rates = [] then Error "market slice must contain FX rates"
    else if not (unique_fx fx_rates) then
      Error "market slice must contain one FX rate per currency"
    else if not (unique_actions corporate_actions) then
      Error "market slice corporate action IDs must be unique"
    else if not (unique_lifecycle lifecycle_events) then
      Error "market slice lifecycle event IDs must be unique"
    else if not (unique_borrow borrow_observations) then
      Error "market slice borrow observation instrument IDs must be unique"
    else if not (unique_cash_rate cash_rate_observations) then
      Error "market slice cash rate currencies must be unique"
    else if not (unique_failure settlement_failures) then
      Error "market slice settlement failure instruction IDs must be unique"
    else if not (ordered_events market_events) then
      Error
        "market events must be strictly ordered by availability, receipt, and \
         ingest sequence"
    else if not (ordered_book_events order_book_events) then
      Error
        "order-book events must be strictly ordered by availability, receipt, \
         and ingest sequence"
    else
      Ok
        {
          slice_sequence;
          start_at;
          end_at;
          available_at;
          received_at;
          bars;
          market_events;
          order_book_events;
          fx_rates;
          corporate_actions;
          lifecycle_events;
          borrow_observations;
          cash_rate_observations;
          settlement_failures;
        }

let create_v14 ~slice_sequence ~start_at ~end_at ~available_at ~received_at
    ~bars ~fx_rates ~corporate_actions ~borrow_observations
    ~cash_rate_observations ~settlement_failures ~lifecycle_events
    ~market_events =
  create_v15 ~slice_sequence ~start_at ~end_at ~available_at ~received_at ~bars
    ~fx_rates ~corporate_actions ~borrow_observations ~cash_rate_observations
    ~settlement_failures ~lifecycle_events ~market_events ~order_book_events:[]

let create_v12 ~slice_sequence ~start_at ~end_at ~available_at ~received_at
    ~bars ~fx_rates ~corporate_actions ~borrow_observations
    ~cash_rate_observations ~settlement_failures ~lifecycle_events =
  create_v15 ~slice_sequence ~start_at ~end_at ~available_at ~received_at ~bars
    ~fx_rates ~corporate_actions ~borrow_observations ~cash_rate_observations
    ~settlement_failures ~lifecycle_events ~market_events:[]
    ~order_book_events:[]

let create_v13 = create_v12

let create_v11 ~slice_sequence ~start_at ~end_at ~available_at ~received_at
    ~bars ~fx_rates ~corporate_actions ~borrow_observations
    ~cash_rate_observations ~settlement_failures =
  create_v12 ~slice_sequence ~start_at ~end_at ~available_at ~received_at ~bars
    ~fx_rates ~corporate_actions ~borrow_observations ~cash_rate_observations
    ~settlement_failures ~lifecycle_events:[]

let create_v10 ~slice_sequence ~start_at ~end_at ~available_at ~received_at
    ~bars ~fx_rates ~corporate_actions ~borrow_observations
    ~cash_rate_observations =
  create_v11 ~slice_sequence ~start_at ~end_at ~available_at ~received_at ~bars
    ~fx_rates ~corporate_actions ~borrow_observations ~cash_rate_observations
    ~settlement_failures:[]

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
    "slice[%Ld] bars=%d events=%d book_events=%d fx=%d actions=%d lifecycle=%d \
     borrow=%d cash_rates=%d failures=%d"
    state.slice_sequence (List.length state.bars)
    (List.length state.market_events)
    (List.length state.order_book_events)
    (List.length state.fx_rates)
    (List.length state.corporate_actions)
    (List.length state.lifecycle_events)
    (List.length state.borrow_observations)
    (List.length state.cash_rate_observations)
    (List.length state.settlement_failures)
