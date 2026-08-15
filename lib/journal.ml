type t = {
  final_path : string;
  partial_path : string;
  channel : out_channel;
  mutable closed : bool;
}

let create final_path =
  let partial_path = final_path ^ ".partial" in
  if Sys.file_exists final_path then
    Error ("journal already exists: " ^ final_path)
  else if Sys.file_exists partial_path then
    Error ("partial journal already exists: " ^ partial_path)
  else
    try
      let channel =
        open_out_gen
          [ Open_wronly; Open_creat; Open_excl; Open_binary ]
          0o600 partial_path
      in
      Ok { final_path; partial_path; channel; closed = false }
    with Sys_error message -> Error ("could not create journal: " ^ message)

let append journal event =
  if journal.closed then Error "cannot append to a closed journal"
  else
    try
      output_string journal.channel (Codec.audit_to_string event);
      output_char journal.channel '\n';
      flush journal.channel;
      Ok ()
    with Sys_error message ->
      Error ("could not append journal " ^ journal.partial_path ^ ": " ^ message)

let close_preserving_partial journal =
  if not journal.closed then (
    journal.closed <- true;
    close_out_noerr journal.channel)

let commit journal =
  if journal.closed then Error "cannot commit a closed journal"
  else
    try
      flush journal.channel;
      close_out journal.channel;
      journal.closed <- true;
      Unix.link journal.partial_path journal.final_path;
      Unix.unlink journal.partial_path;
      Ok ()
    with
    | Sys_error message ->
        close_preserving_partial journal;
        Error ("could not finalize journal: " ^ message)
    | Unix.Unix_error (code, operation, target) ->
        close_preserving_partial journal;
        Error
          (Printf.sprintf "could not finalize journal: %s(%s): %s" operation
             target (Unix.error_message code))
