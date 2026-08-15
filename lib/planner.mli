(** Exact portfolio-target sizing helpers. *)

val quantity_from_weight :
  equity:Scalar.Money.t ->
  weight:Scalar.Weight.t ->
  price:Scalar.Price.t ->
  lot_size:Scalar.Quantity.t ->
  (Scalar.Quantity.t, string) result
