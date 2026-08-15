module Checked_int64 = struct
  let add left right =
    let result = Int64.add left right in
    if
      (Int64.compare right 0L > 0 && Int64.compare result left < 0)
      || (Int64.compare right 0L < 0 && Int64.compare result left > 0)
    then Error "int64 addition overflow"
    else Ok result

  let subtract left right =
    let result = Int64.sub left right in
    if
      (Int64.compare right 0L > 0 && Int64.compare result left > 0)
      || (Int64.compare right 0L < 0 && Int64.compare result left < 0)
    then Error "int64 subtraction overflow"
    else Ok result

  let multiply left right =
    if Int64.equal left 0L || Int64.equal right 0L then Ok 0L
    else if
      (Int64.equal left Int64.min_int && Int64.equal right (-1L))
      || (Int64.equal right Int64.min_int && Int64.equal left (-1L))
    then Error "int64 multiplication overflow"
    else
      let result = Int64.mul left right in
      if Int64.equal (Int64.div result right) left then Ok result
      else Error "int64 multiplication overflow"

  let negate value =
    if Int64.equal value Int64.min_int then Error "int64 negation overflow"
    else Ok (Int64.neg value)
end

let scale = 1_000_000L

let all_digits value =
  String.length value > 0
  && String.for_all
       (fun character -> character >= '0' && character <= '9')
       value

let pad_fraction value = value ^ String.make (6 - String.length value) '0'

let parse_scaled ~allow_negative value =
  let negative, unsigned =
    if String.length value > 0 && Char.equal value.[0] '-' then
      (true, String.sub value 1 (String.length value - 1))
    else (false, value)
  in
  if negative && not allow_negative then Error "value must be positive"
  else
    match String.split_on_char '.' unsigned with
    | [ whole ] -> (
        if not (all_digits whole) then Error "value must be a decimal number"
        else
          match Int64.of_string_opt whole with
          | None -> Error "decimal value is outside the supported range"
          | Some whole_value -> (
              match Checked_int64.multiply whole_value scale with
              | Error _ as error -> error
              | Ok scaled ->
                  if negative then Checked_int64.negate scaled else Ok scaled))
    | [ whole; fraction ] -> (
        if
          (not (all_digits whole))
          || (not (all_digits fraction))
          || String.length fraction > 6
        then Error "value must have at most six decimal places"
        else
          match
            ( Int64.of_string_opt whole,
              Int64.of_string_opt (pad_fraction fraction) )
          with
          | Some whole_value, Some fraction_value -> (
              match Checked_int64.multiply whole_value scale with
              | Error _ as error -> error
              | Ok scaled -> (
                  match Checked_int64.add scaled fraction_value with
                  | Error _ as error -> error
                  | Ok combined ->
                      if negative then Checked_int64.negate combined
                      else Ok combined))
          | _ -> Error "decimal value is outside the supported range")
    | _ -> Error "value must be a decimal number"

let trim_fraction value =
  let rec last_nonzero index =
    if index < 0 then -1
    else if Char.equal value.[index] '0' then last_nonzero (index - 1)
    else index
  in
  let last = last_nonzero (String.length value - 1) in
  if last < 0 then "" else String.sub value 0 (last + 1)

let scaled_to_string value =
  let whole = Int64.div value scale in
  let remainder = Int64.rem value scale in
  let fraction = Int64.abs remainder |> Int64.to_string in
  let fraction = String.make (6 - String.length fraction) '0' ^ fraction in
  let fraction = trim_fraction fraction in
  let sign =
    if Int64.compare value 0L < 0 && Int64.equal whole 0L then "-" else ""
  in
  if String.length fraction = 0 then Printf.sprintf "%s%Ld" sign whole
  else Printf.sprintf "%s%Ld.%s" sign whole fraction

module Price = struct
  type t = int64

  let scale = scale

  let of_micros value =
    if Int64.compare value 0L <= 0 then Error "price must be positive"
    else Ok value

  let of_decimal_string value =
    match parse_scaled ~allow_negative:false value with
    | Error _ as error -> error
    | Ok parsed when not (String.equal (scaled_to_string parsed) value) ->
        Error "price must use canonical decimal form"
    | Ok value -> of_micros value

  let to_micros value = value
  let to_decimal_string = scaled_to_string
  let compare = Int64.compare
  let equal = Int64.equal
  let is_multiple value ~tick = Int64.equal (Int64.rem value tick) 0L

  let pp formatter value =
    Format.pp_print_string formatter (to_decimal_string value)
end

module Quantity = struct
  type t = int64

  let zero = 0L

  let of_int64 value =
    if Int64.compare value 0L < 0 then Error "quantity must be nonnegative"
    else Ok value

  let of_string value =
    match Int64.of_string_opt value with
    | None -> Error "quantity must be a whole number"
    | Some parsed when not (String.equal (Int64.to_string parsed) value) ->
        Error "quantity must use canonical whole-number form"
    | Some value -> of_int64 value

  let to_int64 value = value
  let to_string = Int64.to_string
  let compare = Int64.compare
  let equal = Int64.equal

  let add left right =
    match Checked_int64.add left right with
    | Error _ as error -> error
    | Ok value -> of_int64 value

  let subtract left right =
    if Int64.compare right left > 0 then
      Error "quantity subtraction would be negative"
    else Ok (Int64.sub left right)

  let minimum left right = if compare left right <= 0 then left else right
  let is_zero value = Int64.equal value 0L
  let is_multiple value ~lot = Int64.equal (Int64.rem value lot) 0L

  let round_down_to_multiple value ~multiple =
    if Int64.equal multiple 0L then Error "quantity multiple must be positive"
    else Ok (Int64.sub value (Int64.rem value multiple))

  let bps_floor value ~bps =
    if bps < 0 || bps > 10_000 then
      Error "basis points must be between 0 and 10000"
    else
      let quotient = Int64.div value 10_000L in
      let remainder = Int64.rem value 10_000L in
      match Checked_int64.multiply quotient (Int64.of_int bps) with
      | Error _ as error -> error
      | Ok whole ->
          let partial =
            Int64.div (Int64.mul remainder (Int64.of_int bps)) 10_000L
          in
          Checked_int64.add whole partial

  let pp formatter value = Format.pp_print_string formatter (to_string value)
end

module Weight = struct
  type t = int64

  let scale = scale
  let zero = 0L
  let one = scale

  let of_decimal_string value =
    match parse_scaled ~allow_negative:false value with
    | Error _ as error -> error
    | Ok parsed when not (String.equal (scaled_to_string parsed) value) ->
        Error "weight must use canonical decimal form"
    | Ok value when Int64.compare value scale > 0 ->
        Error "weight must not exceed one"
    | Ok value -> Ok value

  let to_micros value = value
  let to_decimal_string = scaled_to_string
  let compare = Int64.compare
  let equal = Int64.equal
  let add = Checked_int64.add

  let pp formatter value =
    Format.pp_print_string formatter (to_decimal_string value)
end

module Money = struct
  type t = int64

  let scale = scale
  let zero = 0L
  let of_micros value = value

  let of_decimal_string value =
    match parse_scaled ~allow_negative:true value with
    | Error _ as error -> error
    | Ok parsed when not (String.equal (scaled_to_string parsed) value) ->
        Error "money must use canonical decimal form"
    | Ok value -> Ok value

  let to_micros value = value
  let to_decimal_string = scaled_to_string
  let compare = Int64.compare
  let equal = Int64.equal
  let add = Checked_int64.add
  let subtract = Checked_int64.subtract
  let negate = Checked_int64.negate

  let notional price quantity =
    Checked_int64.multiply (Price.to_micros price) (Quantity.to_int64 quantity)

  let fee ~fixed ~bps ~notional =
    if Int64.compare fixed 0L < 0 then Error "fixed fee must be nonnegative"
    else if Int64.compare notional 0L < 0 then
      Error "notional must be nonnegative"
    else if bps < 0 || bps > 10_000 then
      Error "fee basis points must be between 0 and 10000"
    else
      let basis = 10_000L in
      let bps_value = Int64.of_int bps in
      let quotient = Int64.div notional basis in
      let remainder = Int64.rem notional basis in
      match Checked_int64.multiply quotient bps_value with
      | Error _ as error -> error
      | Ok whole -> (
          let numerator = Int64.mul remainder bps_value in
          let partial =
            if Int64.equal numerator 0L then 0L
            else Int64.add (Int64.div (Int64.sub numerator 1L) basis) 1L
          in
          match Checked_int64.add whole partial with
          | Error _ as error -> error
          | Ok variable -> Checked_int64.add fixed variable)

  let proportion_floor value ~numerator ~denominator =
    let numerator = Quantity.to_int64 numerator in
    let denominator = Quantity.to_int64 denominator in
    if Int64.compare value 0L < 0 then
      Error "proportional value must be nonnegative"
    else if Int64.equal denominator 0L then
      Error "proportional denominator must be positive"
    else if Int64.compare numerator denominator > 0 then
      Error "proportional numerator must not exceed denominator"
    else
      let quotient = Int64.div value denominator in
      let remainder = Int64.rem value denominator in
      match Checked_int64.multiply quotient numerator with
      | Error _ as error -> error
      | Ok whole ->
          let partial =
            Z.(
              div
                (mul (of_int64 remainder) (of_int64 numerator))
                (of_int64 denominator))
          in
          if not (Z.fits_int64 partial) then
            Error "proportional calculation overflow"
          else Checked_int64.add whole (Z.to_int64 partial)

  let pp formatter value =
    Format.pp_print_string formatter (to_decimal_string value)
end
