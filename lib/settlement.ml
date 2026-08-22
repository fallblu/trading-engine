type cash_buying_power = Total_cash | Settled_cash
type position_availability = Total_positions | Settled_positions

type calendar = {
  calendar_id : string;
  version : string;
  business_dates : string list;
}

type rule = {
  instrument_id : Id.Instrument.t;
  calendar_id : string;
  lag_business_days : int;
}

type policy = {
  cash_buying_power : cash_buying_power;
  position_availability : position_availability;
  calendars : calendar list;
  rules : rule list;
}

type status =
  | Pending
  | Settled of Ptime.t
  | Failed of { failed_at : Ptime.t; reason : string }

type instruction = {
  instruction_id : string;
  fill_id : Id.Fill.t;
  instrument_id : Id.Instrument.t;
  currency : string;
  cash_movement : Scalar.Money.t;
  position_movement : Scalar.Quantity.t;
  trade_date : string;
  due_date : string;
  status : status;
}

type failure = { instruction_id : string; reason : string }

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let valid_token value =
  String.length value > 0
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x21 && code <> 0x7f)
       value

let valid_text value =
  String.length value > 0
  && String.equal value (String.trim value)
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x20 && code <> 0x7f)
       value

let valid_date value =
  String.length value = 10
  && value.[4] = '-'
  && value.[7] = '-'
  && Result.is_ok (Ptime.of_rfc3339 (value ^ "T00:00:00Z"))

let calendar ~calendar_id ~version ~business_dates =
  if not (valid_token calendar_id) then
    Error "settlement calendar_id must not be empty or contain whitespace"
  else if not (String.equal version "1") then
    Error (Printf.sprintf "unsupported settlement calendar version %S" version)
  else if business_dates = [] then
    Error "settlement calendar must define at least one business date"
  else if not (List.for_all valid_date business_dates) then
    Error "settlement business dates must use canonical YYYY-MM-DD dates"
  else if List.sort_uniq String.compare business_dates <> business_dates then
    Error "settlement business dates must be unique and increasing"
  else Ok { calendar_id; version; business_dates }

let rule ~instrument_id ~calendar_id ~lag_business_days =
  if not (valid_token calendar_id) then
    Error "settlement rule calendar_id must not be empty or contain whitespace"
  else if lag_business_days < 0 || lag_business_days > 30 then
    Error "settlement lag_business_days must be between zero and 30"
  else Ok { instrument_id; calendar_id; lag_business_days }

let policy ~cash_buying_power ~position_availability
    ~(calendars : calendar list) ~(rules : rule list) =
  if calendars = [] then Error "settlement policy must define calendars"
  else if rules = [] then Error "settlement policy must define instrument rules"
  else
    let calendar_ids =
      List.map (fun (value : calendar) -> value.calendar_id) calendars
    in
    let instruments =
      List.map (fun (value : rule) -> value.instrument_id) rules
    in
    if
      List.sort_uniq String.compare calendar_ids
      <> List.sort String.compare calendar_ids
    then Error "settlement calendar IDs must be unique"
    else if
      List.sort_uniq Id.Instrument.compare instruments
      <> List.sort Id.Instrument.compare instruments
    then Error "settlement rules must name unique instruments"
    else if
      not
        (List.for_all
           (fun (value : rule) -> List.mem value.calendar_id calendar_ids)
           rules)
    then Error "settlement rule refers to an unknown calendar"
    else Ok { cash_buying_power; position_availability; calendars; rules }

let date_of_timestamp timestamp =
  let year, month, day = Ptime.to_date timestamp in
  Printf.sprintf "%04d-%02d-%02d" year month day

let instruction policy (fill : Fill.t) =
  let* rule =
    match
      List.find_opt
        (fun (rule : rule) ->
          Id.Instrument.equal rule.instrument_id fill.instrument_id)
        policy.rules
    with
    | Some value -> Ok value
    | None -> Error "fill instrument has no settlement rule"
  in
  let* calendar =
    match
      List.find_opt
        (fun (calendar : calendar) ->
          String.equal calendar.calendar_id rule.calendar_id)
        policy.calendars
    with
    | Some value -> Ok value
    | None -> Error "settlement rule calendar is unavailable"
  in
  let trade_date = date_of_timestamp fill.executed_at in
  let* trade_index =
    let rec find index = function
      | [] -> Error "fill trade date is absent from its settlement calendar"
      | date :: remaining ->
          if String.equal date trade_date then Ok index
          else find (index + 1) remaining
    in
    find 0 calendar.business_dates
  in
  let* due_date =
    match
      List.nth_opt calendar.business_dates (trade_index + rule.lag_business_days)
    with
    | Some value -> Ok value
    | None ->
        Error "settlement calendar does not cover the instruction due date"
  in
  let* cash_movement, position_movement =
    match fill.side with
    | Order.Buy ->
        let* debit = Scalar.Money.add fill.notional fill.fee in
        let* cash = Scalar.Money.negate debit in
        Ok (cash, fill.quantity)
    | Order.Sell ->
        let* cash = Scalar.Money.subtract fill.notional fill.fee in
        let* position = Scalar.Quantity.negate fill.quantity in
        Ok (cash, position)
  in
  Ok
    {
      instruction_id = Id.Fill.to_string fill.id ^ "-settlement";
      fill_id = fill.id;
      instrument_id = fill.instrument_id;
      currency = fill.quote_currency;
      cash_movement;
      position_movement;
      trade_date;
      due_date;
      status = Pending;
    }

let failure ~instruction_id ~reason =
  if not (valid_token instruction_id) then
    Error
      "settlement failure instruction_id must not be empty or contain \
       whitespace"
  else if not (valid_text reason) then
    Error "settlement failure reason must be nonempty, trimmed text"
  else Ok { instruction_id; reason }

let settle instruction ~settled_at =
  match instruction.status with
  | Pending -> Ok { instruction with status = Settled settled_at }
  | Settled _ | Failed _ -> Error "settlement instruction is already terminal"

let fail instruction ~failed_at ~reason =
  match instruction.status with
  | Pending -> Ok { instruction with status = Failed { failed_at; reason } }
  | Settled _ | Failed _ -> Error "settlement instruction is already terminal"

let is_due instruction timestamp =
  String.compare (date_of_timestamp timestamp) instruction.due_date >= 0

let cash_buying_power_to_string = function
  | Total_cash -> "total_cash"
  | Settled_cash -> "settled_cash"

let position_availability_to_string = function
  | Total_positions -> "total_positions"
  | Settled_positions -> "settled_positions"

let status_to_string = function
  | Pending -> "pending"
  | Settled _ -> "settled"
  | Failed _ -> "failed"
