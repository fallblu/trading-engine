type t = { path : string; channel : out_channel; mutable closed : bool }

let create path =
  try
    let channel =
      open_out_gen
        [ Open_wronly; Open_creat; Open_excl; Open_binary ]
        0o600 path
    in
    Ok { path; channel; closed = false }
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
      Error ("could not append journal " ^ journal.path ^ ": " ^ message)

let close journal =
  if not journal.closed then (
    journal.closed <- true;
    close_out_noerr journal.channel)
