module type S = sig
  val name : string

  val start_slice :
    Execution.t ->
    instruments:Instrument.t list ->
    oms:Oms.t ->
    Market_slice.t ->
    (Execution.cursor, string) result
end

type t = (module S)

module Completed_bar_v1 = struct
  let name = "completed_bar_v1"
  let start_slice = Execution.start_slice
end

let of_module model = model
let name (module Model : S) = Model.name
let builtins : t list = [ (module Completed_bar_v1) ]
let supported = List.map name builtins

let find requested =
  match
    List.find_opt (fun model -> String.equal requested (name model)) builtins
  with
  | Some model -> Ok model
  | None -> Error (Printf.sprintf "unsupported execution model %S" requested)

let start_slice (module Model : S) = Model.start_slice
