type t = { artifact : Artifact_writer.t; mutable next_sequence : int64 }

let diagnostic ?sequence ~code message =
  Diagnostic.make ?sequence ~code ~phase:Diagnostic.Artifact message

let create ?effects final_path =
  Artifact_writer.create ?effects ~label:"strategy transcript" final_path
  |> Result.map (fun artifact -> { artifact; next_sequence = 1L })

let append transcript ~direction message =
  if Int64.equal transcript.next_sequence Int64.max_int then
    Error
      (diagnostic ~sequence:transcript.next_sequence
         ~code:Diagnostic.Artifact_state
         "strategy transcript sequence is exhausted")
  else
    let record =
      Strategy_protocol.transcript_record
        ~transcript_sequence:transcript.next_sequence ~direction ~message
    in
    Artifact_writer.append transcript.artifact
      (Strategy_protocol.message_to_string record ^ "\n")
    |> Result.map (fun () ->
        transcript.next_sequence <- Int64.succ transcript.next_sequence)
    |> Result.map_error (Diagnostic.annotate ~sequence:transcript.next_sequence)

let close_preserving_partial transcript =
  Artifact_writer.close_preserving_partial transcript.artifact

let commit transcript = Artifact_writer.commit [ transcript.artifact ]
let artifact transcript = transcript.artifact
