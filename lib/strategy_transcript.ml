type t = {
  final_path : string;
  partial_path : string;
  channel : out_channel;
  effects : Boundary_effects.t;
  mutable next_sequence : int64;
  mutable closed : bool;
}

let diagnostic ?sequence ~code message =
  Diagnostic.make ?sequence ~code ~phase:Diagnostic.Artifact message

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
         ("strategy transcript already exists: " ^ final_path))
  else if Sys.file_exists partial_path then
    Error
      (diagnostic ~code:Diagnostic.Artifact_exists
         ("partial strategy transcript already exists: " ^ partial_path))
  else
    try
      let channel =
        Boundary_effects.perform effects
          (Boundary_effects.Create_artifact partial_path) (fun () ->
            open_out_gen
              [ Open_wronly; Open_creat; Open_excl; Open_binary ]
              0o600 partial_path)
      in
      Ok
        {
          final_path;
          partial_path;
          channel;
          effects;
          next_sequence = 1L;
          closed = false;
        }
    with exception_ ->
      Error
        (Diagnostic.of_exception ~code:Diagnostic.Artifact_io
           ~phase:Diagnostic.Artifact
           ~message:
             ("could not create strategy transcript: "
             ^ exception_message exception_)
           exception_)

let append transcript ~direction message =
  if transcript.closed then
    Error
      (diagnostic ~sequence:transcript.next_sequence
         ~code:Diagnostic.Artifact_state
         "cannot append to a closed strategy transcript")
  else if Int64.equal transcript.next_sequence Int64.max_int then
    Error
      (diagnostic ~sequence:transcript.next_sequence
         ~code:Diagnostic.Artifact_state
         "strategy transcript sequence is exhausted")
  else
    try
      let record =
        Strategy_protocol.transcript_record
          ~transcript_sequence:transcript.next_sequence ~direction ~message
      in
      let contents = Strategy_protocol.message_to_string record ^ "\n" in
      Boundary_effects.perform transcript.effects
        (Boundary_effects.Write_artifact
           { channel = transcript.channel; contents })
        (fun () -> output_string transcript.channel contents);
      Boundary_effects.perform transcript.effects
        Boundary_effects.Flush_artifact (fun () -> flush transcript.channel);
      transcript.next_sequence <- Int64.succ transcript.next_sequence;
      Ok ()
    with exception_ ->
      Error
        (Diagnostic.of_exception ~sequence:transcript.next_sequence
           ~code:Diagnostic.Artifact_io ~phase:Diagnostic.Artifact
           ~message:
             ("could not append strategy transcript " ^ transcript.partial_path
            ^ ": "
             ^ exception_message exception_)
           exception_)

let close_preserving_partial transcript =
  if not transcript.closed then (
    transcript.closed <- true;
    close_out_noerr transcript.channel)

let commit transcript =
  if transcript.closed then
    Error
      (diagnostic ~sequence:transcript.next_sequence
         ~code:Diagnostic.Artifact_state
         "cannot commit a closed strategy transcript")
  else
    try
      Boundary_effects.perform transcript.effects
        Boundary_effects.Flush_artifact (fun () -> flush transcript.channel);
      Boundary_effects.perform transcript.effects
        Boundary_effects.Close_artifact (fun () -> close_out transcript.channel);
      transcript.closed <- true;
      Boundary_effects.perform transcript.effects
        (Boundary_effects.Publish_artifact
           {
             partial_path = transcript.partial_path;
             final_path = transcript.final_path;
           })
        (fun () -> Unix.link transcript.partial_path transcript.final_path);
      Boundary_effects.perform transcript.effects
        (Boundary_effects.Cleanup_artifact transcript.partial_path) (fun () ->
          Unix.unlink transcript.partial_path);
      Ok ()
    with exception_ ->
      close_preserving_partial transcript;
      Error
        (Diagnostic.of_exception ~code:Diagnostic.Artifact_io
           ~phase:Diagnostic.Artifact
           ~message:
             ("could not finalize strategy transcript: "
             ^ exception_message exception_)
           exception_)
