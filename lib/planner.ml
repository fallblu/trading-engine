let quantity_from_weight ~equity ~weight ~price ~lot_size =
  if Scalar.Money.compare equity Scalar.Money.zero < 0 then
    Error "portfolio equity must be nonnegative"
  else
    let numerator =
      Z.mul
        (Z.of_int64 (Scalar.Money.to_micros equity))
        (Z.of_int64 (Scalar.Weight.to_micros weight))
    in
    let denominator =
      Z.mul
        (Z.of_int64 Scalar.Weight.scale)
        (Z.of_int64 (Scalar.Price.to_micros price))
    in
    let quantity = Z.div numerator denominator in
    if not (Z.fits_int64 quantity) then Error "target quantity overflow"
    else
      match Scalar.Quantity.of_int64 (Z.to_int64 quantity) with
      | Error _ as error -> error
      | Ok quantity ->
          Scalar.Quantity.round_down_to_multiple quantity ~multiple:lot_size
