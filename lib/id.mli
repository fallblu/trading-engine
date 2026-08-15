(** Validated opaque identifiers used across engine boundaries. *)

module type S = sig
  type t

  val of_string : string -> (t, string) result
  val of_string_exn : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
end

module Run : S
module Instrument : S
module Order : S
module Fill : S
module Strategy : S
