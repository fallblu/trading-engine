type t = {
  final_path : string;
  partial_path : string;
  channel : out_channel;
  effects : Boundary_effects.t;
  mutable closed : bool;
}

let diagnostic ?event_id ?order_id ?causation_ids ~code message =
  Diagnostic.make ?event_id ?order_id ?causation_ids ~code
    ~phase:Diagnostic.Artifact message

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

let exception_message = function
  | Sys_error message -> message
  | Unix.Unix_error (code, operation, target) ->
      Printf.sprintf "%s(%s): %s" operation target (Unix.error_message code)
  | exception_ -> Printexc.to_string exception_

let create ?(effects = Boundary_effects.direct) final_path =
  let partial_path = final_path ^ ".partial" in
  if Sys.file_exists final_path then
    Error
      (diagnostic ~code:Diagnostic.Artifact_exists
         ("journal already exists: " ^ final_path))
  else if Sys.file_exists partial_path then
    Error
      (diagnostic ~code:Diagnostic.Artifact_exists
         ("partial journal already exists: " ^ partial_path))
  else
    try
      let channel =
        Boundary_effects.perform effects
          (Boundary_effects.Create_artifact partial_path) (fun () ->
            open_out_gen
              [ Open_wronly; Open_creat; Open_excl; Open_binary ]
              0o600 partial_path)
      in
      Ok { final_path; partial_path; channel; effects; closed = false }
    with exception_ ->
      Error
        (Diagnostic.of_exception ~code:Diagnostic.Artifact_io
           ~phase:Diagnostic.Artifact
           ~message:("could not create journal: " ^ exception_message exception_)
           exception_)

let append journal event =
  let event_id, order_id, causation_ids = audit_context event in
  if journal.closed then
    Error
      (diagnostic ~event_id ?order_id ~causation_ids
         ~code:Diagnostic.Artifact_state "cannot append to a closed journal")
  else
    try
      let contents = Codec.audit_to_string event ^ "\n" in
      Boundary_effects.perform journal.effects
        (Boundary_effects.Write_artifact { channel = journal.channel; contents })
        (fun () -> output_string journal.channel contents);
      Boundary_effects.perform journal.effects Boundary_effects.Flush_artifact
        (fun () -> flush journal.channel);
      Ok ()
    with exception_ ->
      Error
        (Diagnostic.of_exception ~code:Diagnostic.Artifact_io
           ~phase:Diagnostic.Artifact
           ~message:
             ("could not append journal " ^ journal.partial_path ^ ": "
             ^ exception_message exception_)
           exception_
        |> Diagnostic.annotate ~event_id ?order_id ~causation_ids)

let close_preserving_partial journal =
  if not journal.closed then (
    journal.closed <- true;
    close_out_noerr journal.channel)

let commit journal =
  if journal.closed then
    Error
      (diagnostic ~code:Diagnostic.Artifact_state
         "cannot commit a closed journal")
  else
    try
      Boundary_effects.perform journal.effects Boundary_effects.Flush_artifact
        (fun () -> flush journal.channel);
      Boundary_effects.perform journal.effects Boundary_effects.Close_artifact
        (fun () -> close_out journal.channel);
      journal.closed <- true;
      Boundary_effects.perform journal.effects
        (Boundary_effects.Publish_artifact
           {
             partial_path = journal.partial_path;
             final_path = journal.final_path;
           })
        (fun () -> Unix.link journal.partial_path journal.final_path);
      Boundary_effects.perform journal.effects
        (Boundary_effects.Cleanup_artifact journal.partial_path) (fun () ->
          Unix.unlink journal.partial_path);
      Ok ()
    with exception_ ->
      close_preserving_partial journal;
      Error
        (Diagnostic.of_exception ~code:Diagnostic.Artifact_io
           ~phase:Diagnostic.Artifact
           ~message:
             ("could not finalize journal: " ^ exception_message exception_)
           exception_)
