type day_count = Actual_365 | Actual_360
type compounding = Simple | Daily
type missing_data = Reject | Zero
type locate_policy = Reject_order | Clip_fill
type recall_policy = Reject_new_shorts | Close_out

type policy = {
  day_count : day_count;
  compounding : compounding;
  borrow_missing_data : missing_data;
  cash_missing_data : missing_data;
  locate_policy : locate_policy;
  recall_policy : recall_policy;
}

type borrow_observation = {
  instrument_id : Id.Instrument.t;
  effective_at : Ptime.t;
  available_quantity : Scalar.Quantity.t;
  annual_rate_bps : int;
  recalled : bool;
}

type cash_rate_observation = {
  currency : string;
  effective_at : Ptime.t;
  credit_rate_bps : int;
  debit_rate_bps : int;
}

let policy ~day_count ~compounding ~borrow_missing_data ~cash_missing_data
    ~locate_policy ~recall_policy =
  {
    day_count;
    compounding;
    borrow_missing_data;
    cash_missing_data;
    locate_policy;
    recall_policy;
  }

let legacy_policy =
  policy ~day_count:Actual_365 ~compounding:Simple ~borrow_missing_data:Zero
    ~cash_missing_data:Zero ~locate_policy:Clip_fill
    ~recall_policy:Reject_new_shorts

let valid_currency value =
  String.length value > 0
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x21 && code <> 0x7f)
       value

let valid_rate value = value >= -1_000_000 && value <= 1_000_000

let borrow_observation ~instrument_id ~effective_at ~available_quantity
    ~annual_rate_bps ~recalled =
  if not (valid_rate annual_rate_bps) then
    Error "borrow annual rate basis points must be between -1000000 and 1000000"
  else if recalled && not (Scalar.Quantity.is_zero available_quantity) then
    Error "recalled borrow availability must be zero"
  else
    Ok
      {
        instrument_id;
        effective_at;
        available_quantity;
        annual_rate_bps;
        recalled;
      }

let cash_rate_observation ~currency ~effective_at ~credit_rate_bps
    ~debit_rate_bps =
  if not (valid_currency currency) then
    Error "cash rate currency must not be empty or contain whitespace"
  else if not (valid_rate credit_rate_bps && valid_rate debit_rate_bps) then
    Error "cash annual rate basis points must be between -1000000 and 1000000"
  else Ok { currency; effective_at; credit_rate_bps; debit_rate_bps }

let picoseconds_per_day = Z.of_string "86400000000000000"

let span_picoseconds span =
  let days, picoseconds = Ptime.Span.to_d_ps span in
  Z.add (Z.mul (Z.of_int days) picoseconds_per_day) (Z.of_int64 picoseconds)

let round_ratio numerator denominator =
  let sign = Z.sign numerator in
  if sign = 0 then Z.zero
  else
    let magnitude = Z.abs numerator in
    let quotient, remainder = Z.ediv_rem magnitude denominator in
    let rounded =
      if Z.compare (Z.mul remainder (Z.of_int 2)) denominator >= 0 then
        Z.succ quotient
      else quotient
    in
    if sign < 0 then Z.neg rounded else rounded

let year_days = function Actual_365 -> 365 | Actual_360 -> 360

let simple_micros policy ~principal_micros ~annual_rate_bps duration_ps =
  let numerator =
    Z.mul (Z.mul principal_micros (Z.of_int annual_rate_bps)) duration_ps
  in
  let denominator =
    Z.mul
      (Z.mul (Z.of_int 10_000) (Z.of_int (year_days policy.day_count)))
      picoseconds_per_day
  in
  round_ratio numerator denominator

let accrue policy ~principal ~annual_rate_bps span =
  if not (valid_rate annual_rate_bps) then
    Error "annual rate basis points must be between -1000000 and 1000000"
  else
    let duration_ps = span_picoseconds span in
    if Z.sign duration_ps < 0 then
      Error "financing accrual span must be nonnegative"
    else
      let principal_micros = Z.of_int64 (Scalar.Money.to_micros principal) in
      let interest =
        match policy.compounding with
        | Simple ->
            simple_micros policy ~principal_micros ~annual_rate_bps duration_ps
        | Daily ->
            let whole_days, remainder =
              Z.ediv_rem duration_ps picoseconds_per_day
            in
            let rec compound remaining balance total =
              if Z.equal remaining Z.zero then (balance, total)
              else
                let amount =
                  simple_micros policy ~principal_micros:balance
                    ~annual_rate_bps picoseconds_per_day
                in
                compound (Z.pred remaining) (Z.add balance amount)
                  (Z.add total amount)
            in
            let balance, full_interest =
              compound whole_days principal_micros Z.zero
            in
            Z.add full_interest
              (simple_micros policy ~principal_micros:balance ~annual_rate_bps
                 remainder)
      in
      if Z.fits_int64 interest then
        Ok (Scalar.Money.of_micros (Z.to_int64 interest))
      else Error "financing accrual overflow"

let day_count_to_string = function
  | Actual_365 -> "actual_365"
  | Actual_360 -> "actual_360"

let compounding_to_string = function Simple -> "simple" | Daily -> "daily"
let missing_data_to_string = function Reject -> "reject" | Zero -> "zero"

let locate_policy_to_string = function
  | Reject_order -> "reject_order"
  | Clip_fill -> "clip_fill"

let recall_policy_to_string = function
  | Reject_new_shorts -> "reject_new_shorts"
  | Close_out -> "close_out"
