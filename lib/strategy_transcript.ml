type t = {
  final_path : string;
  partial_path : string;
  channel : out_channel;
  mutable next_sequence : int64;
  mutable closed : bool;
}

let create final_path =
  let partial_path = final_path ^ ".partial" in
  if Sys.file_exists final_path then
    Error ("strategy transcript already exists: " ^ final_path)
  else if Sys.file_exists partial_path then
    Error ("partial strategy transcript already exists: " ^ partial_path)
  else
    try
      let channel =
        open_out_gen
          [ Open_wronly; Open_creat; Open_excl; Open_binary ]
          0o600 partial_path
      in
      Ok
        {
          final_path;
          partial_path;
          channel;
          next_sequence = 1L;
          closed = false;
        }
    with Sys_error message ->
      Error ("could not create strategy transcript: " ^ message)

let append transcript ~direction message =
  if transcript.closed then
    Error "cannot append to a closed strategy transcript"
  else if Int64.equal transcript.next_sequence Int64.max_int then
    Error "strategy transcript sequence is exhausted"
  else
    try
      let record =
        Strategy_protocol.transcript_record
          ~transcript_sequence:transcript.next_sequence ~direction ~message
      in
      output_string transcript.channel
        (Strategy_protocol.message_to_string record);
      output_char transcript.channel '\n';
      flush transcript.channel;
      transcript.next_sequence <- Int64.succ transcript.next_sequence;
      Ok ()
    with Sys_error message ->
      Error
        ("could not append strategy transcript " ^ transcript.partial_path
       ^ ": " ^ message)

let close_preserving_partial transcript =
  if not transcript.closed then (
    transcript.closed <- true;
    close_out_noerr transcript.channel)

let commit transcript =
  if transcript.closed then Error "cannot commit a closed strategy transcript"
  else
    try
      flush transcript.channel;
      close_out transcript.channel;
      transcript.closed <- true;
      Unix.link transcript.partial_path transcript.final_path;
      Unix.unlink transcript.partial_path;
      Ok ()
    with
    | Sys_error message ->
        close_preserving_partial transcript;
        Error ("could not finalize strategy transcript: " ^ message)
    | Unix.Unix_error (code, operation, target) ->
        close_preserving_partial transcript;
        Error
          (Printf.sprintf "could not finalize strategy transcript: %s(%s): %s"
             operation target (Unix.error_message code))
