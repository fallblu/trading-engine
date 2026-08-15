module Sequence_map = Map.Make (Int64)

type state = Strategy.intent list Sequence_map.t

let create schedule =
  let add result (sequence, intents) =
    match result with
    | Error _ as error -> error
    | Ok map ->
        if Int64.compare sequence 0L < 0 then
          Error "scheduled bar sequence must be nonnegative"
        else
          let existing =
            Option.value (Sequence_map.find_opt sequence map) ~default:[]
          in
          Ok (Sequence_map.add sequence (existing @ intents) map)
  in
  List.fold_left add (Ok Sequence_map.empty) schedule

let name = "scripted"

let on_event state _context event =
  match event with
  | Strategy.Bar_closed bar ->
      let intents =
        Option.value
          (Sequence_map.find_opt bar.Bar.source_sequence state)
          ~default:[]
      in
      (Sequence_map.remove bar.source_sequence state, intents)
  | Strategy.Fill_received _ | Strategy.Order_updated _
  | Strategy.Intent_rejected _ ->
      (state, [])
