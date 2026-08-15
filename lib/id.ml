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

module Make () = struct
  type t = string

  let valid_character character =
    let code = Char.code character in
    code >= 0x21 && code <> 0x7f

  let of_string value =
    if String.length value = 0 then Error "identifier must not be empty"
    else if String.trim value <> value then
      Error "identifier must not have leading or trailing whitespace"
    else if not (String.for_all valid_character value) then
      Error "identifier must not contain whitespace or control characters"
    else Ok value

  let of_string_exn value =
    match of_string value with
    | Ok identifier -> identifier
    | Error message -> invalid_arg message

  let to_string value = value
  let compare = String.compare
  let equal = String.equal
  let pp = Format.pp_print_string

  module Map = Map.Make (String)
  module Set = Set.Make (String)
end

module Run = Make ()
module Instrument = Make ()
module Order = Make ()
module Fill = Make ()
module Strategy = Make ()
module Event = Make ()
