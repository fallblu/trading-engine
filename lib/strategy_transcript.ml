type t = { artifact : Artifact_writer.t; mutable next_sequence : int64 }

let diagnostic_version = "1"
let max_rejection_prefix_bytes = 256

let diagnostic ?sequence ~code message =
  Diagnostic.make ?sequence ~code ~phase:Diagnostic.Artifact message

let create ?effects ?durability final_path =
  Artifact_writer.create ?effects ?durability ~label:"strategy transcript"
    final_path
  |> Result.map (fun artifact -> { artifact; next_sequence = 1L })

let append_record transcript record =
  if Int64.equal transcript.next_sequence Int64.max_int then
    Error
      (diagnostic ~sequence:transcript.next_sequence
         ~code:Diagnostic.Artifact_state
         "strategy transcript sequence is exhausted")
  else
    Artifact_writer.append transcript.artifact
      (Yojson.Safe.to_string record ^ "\n")
    |> Result.map (fun () ->
        transcript.next_sequence <- Int64.succ transcript.next_sequence)
    |> Result.map_error (Diagnostic.annotate ~sequence:transcript.next_sequence)

let append transcript ~direction message =
  Strategy_protocol.transcript_record
    ~transcript_sequence:transcript.next_sequence ~direction ~message
  |> append_record transcript

let hex_of_string value =
  let digits = "0123456789abcdef" in
  String.init
    (String.length value * 2)
    (fun index ->
      let byte = Char.code value.[index / 2] in
      if index mod 2 = 0 then digits.[byte lsr 4] else digits.[byte land 0xf])

let append_rejection transcript ~expected_sequence ~diagnostic:rejection
    ~raw_prefix ~observed_bytes ~truncated =
  let raw_prefix =
    if String.length raw_prefix <= max_rejection_prefix_bytes then raw_prefix
    else String.sub raw_prefix 0 max_rejection_prefix_bytes
  in
  `Assoc
    [
      ("strategy_diagnostic_version", `String diagnostic_version);
      ("transcript_sequence", `String (Int64.to_string transcript.next_sequence));
      ("record_type", `String "rejected_strategy_response");
      ("expected_strategy_sequence", `String (Int64.to_string expected_sequence));
      ("diagnostic", Diagnostic.to_yojson rejection);
      ( "evidence",
        `Assoc
          [
            ("encoding", `String "hex");
            ("prefix", `String (hex_of_string raw_prefix));
            ("observed_bytes", `Int observed_bytes);
            ("truncated", `Bool truncated);
          ] );
    ]
  |> append_record transcript

let close_preserving_partial transcript =
  Artifact_writer.close_preserving_partial transcript.artifact

let commit transcript = Artifact_writer.commit [ transcript.artifact ]
let artifact transcript = transcript.artifact
