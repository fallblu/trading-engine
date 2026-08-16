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

  let of_z value =
    if Z.fits_int64 value then Ok (Z.to_int64 value)
    else Error "fixed-point calculation overflow"
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

  let scale_ratio_exact value ~numerator ~denominator =
    if Int64.compare numerator 0L <= 0 || Int64.compare denominator 0L <= 0 then
      Error "price ratio terms must be positive"
    else
      let product = Z.mul (Z.of_int64 value) (Z.of_int64 numerator) in
      let denominator = Z.of_int64 denominator in
      if not (Z.equal (Z.rem product denominator) Z.zero) then
        Error "price ratio is not exactly representable"
      else
        match Checked_int64.of_z (Z.div product denominator) with
        | Error _ as error -> error
        | Ok adjusted -> of_micros adjusted

  let pp formatter value =
    Format.pp_print_string formatter (to_decimal_string value)
end

module Quantity = struct
  type t = int64

  let scale = scale
  let zero = 0L
  let of_micros value = value

  let of_decimal_string value =
    match parse_scaled ~allow_negative:true value with
    | Error _ as error -> error
    | Ok parsed when not (String.equal (scaled_to_string parsed) value) ->
        Error "quantity must use canonical decimal form"
    | Ok value -> Ok value

  let to_micros value = value
  let to_decimal_string = scaled_to_string
  let compare = Int64.compare
  let equal = Int64.equal
  let add = Checked_int64.add
  let subtract = Checked_int64.subtract
  let negate = Checked_int64.negate

  let absolute value =
    if Int64.equal value Int64.min_int then Error "quantity absolute overflow"
    else Ok (Int64.abs value)

  let minimum left right = if compare left right <= 0 then left else right
  let is_zero value = Int64.equal value 0L
  let is_positive value = Int64.compare value 0L > 0
  let is_negative value = Int64.compare value 0L < 0
  let is_nonnegative value = Int64.compare value 0L >= 0

  let is_multiple value ~lot =
    Int64.compare lot 0L > 0 && Int64.equal (Int64.rem value lot) 0L

  let round_toward_zero_to_multiple value ~multiple =
    if Int64.compare multiple 0L <= 0 then
      Error "quantity multiple must be positive"
    else Ok (Int64.sub value (Int64.rem value multiple))

  let bps_floor value ~bps =
    if Int64.compare value 0L < 0 then
      Error "basis-point quantity must be nonnegative"
    else if bps < 0 || bps > 10_000 then
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

  let scale_ratio_exact value ~numerator ~denominator =
    if Int64.compare numerator 0L <= 0 || Int64.compare denominator 0L <= 0 then
      Error "quantity ratio terms must be positive"
    else
      let product = Z.mul (Z.of_int64 value) (Z.of_int64 numerator) in
      let denominator = Z.of_int64 denominator in
      if not (Z.equal (Z.rem product denominator) Z.zero) then
        Error "quantity ratio is not exactly representable"
      else Checked_int64.of_z (Z.div product denominator)

  let pp formatter value =
    Format.pp_print_string formatter (to_decimal_string value)
end

module Weight = struct
  type t = int64

  let scale = scale
  let zero = 0L
  let one = scale
  let of_micros value = value

  let of_decimal_string value =
    match parse_scaled ~allow_negative:true value with
    | Error _ as error -> error
    | Ok parsed when not (String.equal (scaled_to_string parsed) value) ->
        Error "weight must use canonical decimal form"
    | Ok value -> Ok value

  let to_micros value = value
  let to_decimal_string = scaled_to_string
  let compare = Int64.compare
  let equal = Int64.equal
  let add = Checked_int64.add
  let is_negative value = Int64.compare value 0L < 0

  let absolute value =
    if Int64.equal value Int64.min_int then Error "weight absolute overflow"
    else Ok (Int64.abs value)

  let pp formatter value =
    Format.pp_print_string formatter (to_decimal_string value)
end

module Ratio = struct
  type t = int64

  let scale = scale
  let one = scale

  let of_decimal_string value =
    match parse_scaled ~allow_negative:false value with
    | Error _ as error -> error
    | Ok parsed when not (String.equal (scaled_to_string parsed) value) ->
        Error "ratio must use canonical decimal form"
    | Ok value when Int64.compare value 0L <= 0 ->
        Error "ratio must be positive"
    | Ok value -> Ok value

  let to_micros value = value
  let to_decimal_string = scaled_to_string
  let compare = Int64.compare
  let equal = Int64.equal

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

  let absolute value =
    if Int64.equal value Int64.min_int then Error "money absolute overflow"
    else Ok (Int64.abs value)

  let notional price quantity =
    Z.(
      div
        (mul
           (of_int64 (Price.to_micros price))
           (of_int64 (Quantity.to_micros quantity)))
        (of_int64 scale))
    |> Checked_int64.of_z

  let for_quantity amount quantity =
    Z.(
      div
        (mul (of_int64 amount) (of_int64 (Quantity.to_micros quantity)))
        (of_int64 scale))
    |> Checked_int64.of_z

  let convert value ~rate =
    Z.(
      div
        (mul (of_int64 value) (of_int64 (Price.to_micros rate)))
        (of_int64 scale))
    |> Checked_int64.of_z

  let multiply_ratio value ratio =
    Z.(
      div
        (mul (of_int64 value) (of_int64 (Ratio.to_micros ratio)))
        (of_int64 scale))
    |> Checked_int64.of_z

  let weight_toward_zero value ~equity =
    if Int64.compare equity 0L <= 0 then
      Error "portfolio equity must be positive"
    else
      Z.(div (mul (of_int64 value) (of_int64 Weight.scale)) (of_int64 equity))
      |> Checked_int64.of_z
      |> Result.map Weight.of_micros

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

  let proportion_toward_zero value ~numerator ~denominator =
    let numerator = Quantity.to_micros numerator in
    let denominator = Quantity.to_micros denominator in
    if Int64.compare numerator 0L < 0 then
      Error "proportional numerator must be nonnegative"
    else if Int64.compare denominator 0L <= 0 then
      Error "proportional denominator must be positive"
    else if Int64.compare numerator denominator > 0 then
      Error "proportional numerator must not exceed denominator"
    else
      Z.(div (mul (of_int64 value) (of_int64 numerator)) (of_int64 denominator))
      |> Checked_int64.of_z

  let bps_ceil value ~bps =
    if Int64.compare value 0L < 0 then
      Error "basis-point money must be nonnegative"
    else if bps < 0 then Error "basis points must be nonnegative"
    else
      let numerator = Z.mul (Z.of_int64 value) (Z.of_int bps) in
      let denominator = Z.of_int 10_000 in
      let result =
        if Z.equal numerator Z.zero then Z.zero
        else Z.div (Z.add numerator (Z.pred denominator)) denominator
      in
      Checked_int64.of_z result

  let pp formatter value =
    Format.pp_print_string formatter (to_decimal_string value)
end
