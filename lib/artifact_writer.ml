type open_state
type closed_state
type published_state
type renamed_state
type complete_state
type durability = Buffered | Durable

type 'state file = {
  label : string;
  final_path : string;
  partial_path : string;
  cleanup_path : string;
  channel : out_channel;
  effects : Boundary_effects.t;
  durability : durability;
}

type state =
  | Open of open_state file
  | Closed of closed_state file
  | Published of published_state file
  | Renamed of renamed_state file
  | Complete of complete_state file

type t = { mutable state : state }

let state_label = function
  | Open file -> file.label
  | Closed file -> file.label
  | Published file -> file.label
  | Renamed file -> file.label
  | Complete file -> file.label

let diagnostic ~code message =
  Diagnostic.make ~code ~phase:Diagnostic.Artifact message

let exception_message = function
  | Sys_error message -> message
  | Unix.Unix_error (code, operation, target) ->
      Printf.sprintf "%s(%s): %s" operation target (Unix.error_message code)
  | exception_ -> Printexc.to_string exception_

let exception_diagnostic ~label action exception_ =
  Diagnostic.of_exception ~code:Diagnostic.Artifact_io
    ~phase:Diagnostic.Artifact
    ~message:
      (Printf.sprintf "could not %s %s: %s" action label
         (exception_message exception_))
    exception_

let move_no_replace source_path target_path =
  Unix.link source_path target_path;
  try Unix.unlink source_path
  with exception_ ->
    (try Unix.unlink target_path with _ -> ());
    raise exception_

let transition file =
  {
    label = file.label;
    final_path = file.final_path;
    partial_path = file.partial_path;
    cleanup_path = file.cleanup_path;
    channel = file.channel;
    effects = file.effects;
    durability = file.durability;
  }

let create ?(effects = Boundary_effects.direct) ?(durability = Buffered) ~label
    final_path =
  let partial_path = final_path ^ ".partial" in
  let cleanup_path = partial_path ^ ".cleanup" in
  if Sys.file_exists final_path then
    Error
      (diagnostic ~code:Diagnostic.Artifact_exists
         (Printf.sprintf "%s already exists: %s" label final_path))
  else if Sys.file_exists partial_path then
    Error
      (diagnostic ~code:Diagnostic.Artifact_exists
         (Printf.sprintf "partial %s already exists: %s" label partial_path))
  else if Sys.file_exists cleanup_path then
    Error
      (diagnostic ~code:Diagnostic.Artifact_exists
         (Printf.sprintf "%s cleanup path already exists: %s" label cleanup_path))
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
          state =
            Open
              {
                label;
                final_path;
                partial_path;
                cleanup_path;
                channel;
                effects;
                durability;
              };
        }
    with exception_ -> Error (exception_diagnostic ~label "create" exception_)

let append artifact contents =
  match artifact.state with
  | Closed _ | Published _ | Renamed _ | Complete _ ->
      Error
        (diagnostic ~code:Diagnostic.Artifact_state
           ("cannot append to a closed " ^ state_label artifact.state))
  | Open file -> (
      if String.length contents > Resource_limits.artifact_record_bytes then
        Error
          (diagnostic ~code:Diagnostic.Resource_limit
             (Printf.sprintf "%s record is %d bytes; limit is %d bytes"
                file.label (String.length contents)
                Resource_limits.artifact_record_bytes))
      else
        try
          Boundary_effects.perform file.effects
            (Boundary_effects.Write_artifact
               { channel = file.channel; contents })
            (fun () -> output_string file.channel contents);
          Boundary_effects.perform file.effects Boundary_effects.Flush_artifact
            (fun () -> flush file.channel);
          Ok ()
        with exception_ ->
          Error (exception_diagnostic ~label:file.label "append" exception_))

let close_file artifact file =
  let closed = transition file in
  try
    Boundary_effects.perform file.effects Boundary_effects.Flush_artifact
      (fun () -> flush file.channel);
    (match file.durability with
    | Buffered -> ()
    | Durable ->
        Boundary_effects.perform file.effects
          (Boundary_effects.Sync_artifact file.channel) (fun () ->
            Unix.fsync (Unix.descr_of_out_channel file.channel)));
    Boundary_effects.perform file.effects Boundary_effects.Close_artifact
      (fun () -> close_out file.channel);
    artifact.state <- Closed closed;
    Ok ()
  with exception_ ->
    close_out_noerr file.channel;
    artifact.state <- Closed closed;
    Error (exception_diagnostic ~label:file.label "close" exception_)

let close_preserving_partial artifact =
  match artifact.state with
  | Open file ->
      close_out_noerr file.channel;
      artifact.state <- Closed (transition file)
  | Closed _ | Published _ | Renamed _ | Complete _ -> ()

let publish artifact =
  match artifact.state with
  | Closed file -> (
      try
        Boundary_effects.perform file.effects
          (Boundary_effects.Publish_artifact
             { partial_path = file.partial_path; final_path = file.final_path })
          (fun () -> Unix.link file.partial_path file.final_path);
        artifact.state <- Published (transition file);
        Ok ()
      with exception_ ->
        Error (exception_diagnostic ~label:file.label "publish" exception_))
  | Open _ | Published _ | Renamed _ | Complete _ ->
      Error
        (diagnostic ~code:Diagnostic.Artifact_state
           ("cannot publish " ^ state_label artifact.state
          ^ " from its current state"))

let rename_partial artifact =
  match artifact.state with
  | Published file -> (
      try
        Boundary_effects.perform file.effects
          (Boundary_effects.Rename_artifact
             {
               source_path = file.partial_path;
               target_path = file.cleanup_path;
             })
          (fun () -> move_no_replace file.partial_path file.cleanup_path);
        artifact.state <- Renamed (transition file);
        Ok ()
      with exception_ ->
        Error (exception_diagnostic ~label:file.label "rename" exception_))
  | Open _ | Closed _ | Renamed _ | Complete _ ->
      Error
        (diagnostic ~code:Diagnostic.Artifact_state
           ("cannot rename " ^ state_label artifact.state
          ^ " from its current state"))

let cleanup_partial artifact =
  match artifact.state with
  | Renamed file -> (
      try
        Boundary_effects.perform file.effects
          (Boundary_effects.Cleanup_artifact file.cleanup_path) (fun () ->
            Unix.unlink file.cleanup_path);
        artifact.state <- Complete (transition file);
        Ok ()
      with exception_ ->
        Error (exception_diagnostic ~label:file.label "clean up" exception_))
  | Open _ | Closed _ | Published _ | Complete _ ->
      Error
        (diagnostic ~code:Diagnostic.Artifact_state
           ("cannot clean up " ^ state_label artifact.state
          ^ " from its current state"))

let rollback artifact =
  match artifact.state with
  | Published file -> (
      try
        Boundary_effects.perform file.effects
          (Boundary_effects.Cleanup_artifact file.final_path) (fun () ->
            Unix.unlink file.final_path);
        artifact.state <- Closed (transition file);
        Ok ()
      with exception_ ->
        Error (exception_diagnostic ~label:file.label "roll back" exception_))
  | Open _ | Closed _ | Renamed _ | Complete _ -> Ok ()

let restore_partial artifact =
  match artifact.state with
  | Renamed file -> (
      try
        if not (Sys.file_exists file.partial_path) then
          Boundary_effects.perform file.effects
            (Boundary_effects.Rename_artifact
               {
                 source_path = file.cleanup_path;
                 target_path = file.partial_path;
               })
            (fun () -> move_no_replace file.cleanup_path file.partial_path);
        artifact.state <- Published (transition file);
        Ok ()
      with exception_ ->
        Error (exception_diagnostic ~label:file.label "restore" exception_))
  | Complete file -> (
      try
        if not (Sys.file_exists file.partial_path) then
          Boundary_effects.perform file.effects
            (Boundary_effects.Restore_artifact
               {
                 final_path = file.final_path;
                 partial_path = file.partial_path;
               })
            (fun () -> Unix.link file.final_path file.partial_path);
        artifact.state <- Published (transition file);
        Ok ()
      with exception_ ->
        Error (exception_diagnostic ~label:file.label "restore" exception_))
  | Open _ | Closed _ | Published _ -> Ok ()

let combine original = function
  | Ok () -> original
  | Error cleanup -> Diagnostic.combine original cleanup

let rollback_all artifacts original =
  List.fold_left
    (fun diagnostic artifact -> rollback artifact |> combine diagnostic)
    original artifacts

let restore_all artifacts original =
  List.fold_left
    (fun diagnostic artifact -> restore_partial artifact |> combine diagnostic)
    original artifacts

let directory_effects artifacts =
  let add directories artifact =
    let add_file file =
      match file.durability with
      | Buffered -> directories
      | Durable ->
          let directory = Filename.dirname file.final_path in
          if
            List.exists
              (fun (path, _) -> String.equal path directory)
              directories
          then directories
          else (directory, file.effects) :: directories
    in
    match artifact.state with
    | Open file -> add_file file
    | Closed file -> add_file file
    | Published file -> add_file file
    | Renamed file -> add_file file
    | Complete file -> add_file file
  in
  List.fold_left add [] artifacts |> List.rev

let sync_directory (directory, effects) =
  try
    Boundary_effects.perform effects (Boundary_effects.Sync_directory directory)
      (fun () ->
        let descriptor = Unix.openfile directory [ Unix.O_RDONLY ] 0 in
        Fun.protect
          ~finally:(fun () -> Unix.close descriptor)
          (fun () -> Unix.fsync descriptor));
    Ok ()
  with exception_ ->
    Error
      (exception_diagnostic
         ~label:("artifact directory " ^ directory)
         "synchronize" exception_)

let rec sync_directories = function
  | [] -> Ok ()
  | directory :: rest -> (
      match sync_directory directory with
      | Ok () -> sync_directories rest
      | Error _ as error -> error)

let add_cleanup diagnostic = function
  | Ok () -> diagnostic
  | Error cleanup -> Diagnostic.combine diagnostic cleanup

let rollback_after_failure artifacts directories diagnostic =
  let diagnostic = rollback_all artifacts diagnostic in
  sync_directories directories |> add_cleanup diagnostic

let restore_after_failure artifacts directories diagnostic =
  let diagnostic = restore_all artifacts diagnostic in
  sync_directories directories |> add_cleanup diagnostic

let rec close_all = function
  | [] -> Ok ()
  | artifact :: rest -> (
      match artifact.state with
      | Open file -> (
          match close_file artifact file with
          | Ok () -> close_all rest
          | Error diagnostic ->
              List.iter close_preserving_partial rest;
              Error diagnostic)
      | Closed _ | Published _ | Renamed _ | Complete _ ->
          List.iter close_preserving_partial rest;
          Error
            (diagnostic ~code:Diagnostic.Artifact_state
               (state_label artifact.state ^ " is not open for commit")))

let rec publish_all published = function
  | [] -> Ok ()
  | artifact :: rest -> (
      match publish artifact with
      | Ok () -> publish_all (artifact :: published) rest
      | Error diagnostic -> Error (rollback_all published diagnostic))

let cleanup_all artifacts =
  let rec loop = function
    | [] -> Ok ()
    | artifact :: rest -> (
        match cleanup_partial artifact with
        | Ok () -> loop rest
        | Error _ as error -> error)
  in
  loop artifacts

let rename_all artifacts =
  let rec loop = function
    | [] -> Ok ()
    | artifact :: rest -> (
        match rename_partial artifact with
        | Ok () -> loop rest
        | Error _ as error -> error)
  in
  loop artifacts

let commit artifacts =
  match artifacts with
  | [] ->
      Error
        (diagnostic ~code:Diagnostic.Artifact_state
           "artifact commit requires at least one writer")
  | _ -> (
      let directories = directory_effects artifacts in
      match close_all artifacts with
      | Error _ as error -> error
      | Ok () -> (
          match publish_all [] artifacts with
          | Error diagnostic ->
              Error (rollback_after_failure artifacts directories diagnostic)
          | Ok () -> (
              match sync_directories directories with
              | Error diagnostic ->
                  Error
                    (rollback_after_failure artifacts directories diagnostic)
              | Ok () -> (
                  match rename_all artifacts with
                  | Error diagnostic ->
                      Error
                        (restore_after_failure artifacts directories diagnostic)
                  | Ok () -> (
                      match cleanup_all artifacts with
                      | Error diagnostic ->
                          Error
                            (restore_after_failure artifacts directories
                               diagnostic)
                      | Ok () -> (
                          match sync_directories directories with
                          | Ok () -> Ok ()
                          | Error diagnostic ->
                              Error
                                (restore_after_failure artifacts directories
                                   diagnostic)))))))
