type numeric = string
type value = Numeric of numeric | String of string | Boolean of bool
type aggregation = Last | Sum | Minimum | Maximum | Mean
type dimension = { key : string; value : string }

type t = {
  name : string;
  value : value;
  unit_ : string option;
  dimensions : dimension list;
  aggregation : aggregation option;
}

let max_name_bytes = Resource_limits.metric_name_bytes
let max_string_value_bytes = Resource_limits.metric_string_value_bytes
let max_unit_bytes = Resource_limits.metric_unit_bytes
let max_dimensions = Resource_limits.metric_dimensions
let max_dimension_key_bytes = Resource_limits.metric_dimension_key_bytes
let max_dimension_value_bytes = Resource_limits.metric_dimension_value_bytes

let valid_trimmed ~maximum value =
  String.length value > 0
  && String.length value <= maximum
  && String.equal value (String.trim value)

let numeric_to_string value = value

let numeric_of_string value =
  let length = String.length value in
  let start = if length > 0 && value.[0] = '-' then 1 else 0 in
  let decimal = ref None in
  let valid = ref (start < length) in
  for index = start to length - 1 do
    match value.[index] with
    | '0' .. '9' -> ()
    | '.' when !decimal = None && index > start && index + 1 < length ->
        decimal := Some index
    | _ -> valid := false
  done;
  let integer_end = Option.value !decimal ~default:length in
  if integer_end - start > 1 && value.[start] = '0' then valid := false;
  if Option.is_some !decimal && value.[length - 1] = '0' then valid := false;
  if String.equal value "-0" then valid := false;
  if !valid then Ok value
  else Error "metric numeric value must be a canonical decimal string"

let aggregation_to_string = function
  | Last -> "last"
  | Sum -> "sum"
  | Minimum -> "minimum"
  | Maximum -> "maximum"
  | Mean -> "mean"

let aggregation_of_string = function
  | "last" -> Ok Last
  | "sum" -> Ok Sum
  | "minimum" -> Ok Minimum
  | "maximum" -> Ok Maximum
  | "mean" -> Ok Mean
  | _ -> Error "metric aggregation must be last, sum, minimum, maximum, or mean"

let create ~name ~value ?unit_ ?(dimensions = []) ?aggregation () =
  if not (valid_trimmed ~maximum:max_name_bytes name) then
    Error "metric name must be a nonempty trimmed string of at most 128 bytes"
  else if
    match value with
    | String value -> String.length value > max_string_value_bytes
    | Numeric _ | Boolean _ -> false
  then Error "metric string value must contain at most 1024 bytes"
  else if
    match (value, aggregation) with
    | (String _ | Boolean _), Some (Sum | Minimum | Maximum | Mean) -> true
    | _ -> false
  then Error "non-numeric metrics only support last aggregation"
  else if
    match unit_ with
    | Some unit_ -> not (valid_trimmed ~maximum:max_unit_bytes unit_)
    | None -> false
  then Error "metric unit must be a nonempty trimmed string of at most 64 bytes"
  else if List.length dimensions > max_dimensions then
    Error "metric dimensions must contain at most 16 entries"
  else
    let dimensions =
      List.sort
        (fun (left, _) (right, _) -> String.compare left right)
        dimensions
    in
    let rec validate prior acc = function
      | [] -> Ok { name; value; unit_; dimensions = List.rev acc; aggregation }
      | (key, value) :: remaining ->
          if not (valid_trimmed ~maximum:max_dimension_key_bytes key) then
            Error
              "metric dimension key must be a nonempty trimmed string of at \
               most 64 bytes"
          else if String.length value > max_dimension_value_bytes then
            Error "metric dimension value must contain at most 128 bytes"
          else if Option.equal String.equal prior (Some key) then
            Error "metric dimension keys must be unique"
          else validate (Some key) ({ key; value } :: acc) remaining
    in
    validate None [] dimensions
