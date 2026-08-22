(** Typed, dimensioned strategy observations. *)

type numeric
type value = Numeric of numeric | String of string | Boolean of bool
type aggregation = Last | Sum | Minimum | Maximum | Mean
type dimension = { key : string; value : string }

type t = private {
  name : string;
  value : value;
  unit_ : string option;
  dimensions : dimension list;
  aggregation : aggregation option;
}

val create :
  name:string ->
  value:value ->
  ?unit_:string ->
  ?dimensions:(string * string) list ->
  ?aggregation:aggregation ->
  unit ->
  (t, string) result

val aggregation_to_string : aggregation -> string
val aggregation_of_string : string -> (aggregation, string) result
val numeric_of_string : string -> (numeric, string) result
val numeric_to_string : numeric -> string
