type t = { artifact : Artifact_writer.t }

let audit_context event =
  let event_id = Id.Event.to_string event.Audit.event_id in
  let causation_ids = List.map Id.Event.to_string event.causation_ids in
  let order_id =
    match event.event with
    | Audit.Order_accepted order | Order_rejected order ->
        Some (Id.Order.to_string order.Order.id)
    | Order_cancelled { order; _ } -> Some (Id.Order.to_string order.id)
    | Fill_applied fill -> Some (Id.Order.to_string fill.Fill.order_id)
    | Margin_limited { order_id; _ } | Fill_clipped { order_id; _ } ->
        Some (Id.Order.to_string order_id)
    | _ -> None
  in
  (event_id, order_id, causation_ids)

let create ?effects final_path =
  Artifact_writer.create ?effects ~label:"journal" final_path
  |> Result.map (fun artifact -> { artifact })

let append journal event =
  let event_id, order_id, causation_ids = audit_context event in
  Artifact_writer.append journal.artifact (Codec.audit_to_string event ^ "\n")
  |> Result.map_error (Diagnostic.annotate ~event_id ?order_id ~causation_ids)

let close_preserving_partial journal =
  Artifact_writer.close_preserving_partial journal.artifact

let commit journal = Artifact_writer.commit [ journal.artifact ]
let artifact journal = journal.artifact
