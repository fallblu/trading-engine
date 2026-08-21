(** Minimal effect dispatcher for deterministic boundary failure injection. *)

type stage =
  | Artifact_create
  | Artifact_write
  | Artifact_flush
  | Artifact_sync_file
  | Artifact_close
  | Artifact_publish
  | Artifact_rename
  | Artifact_cleanup
  | Artifact_restore
  | Artifact_sync_directory
  | Process_spawn
  | Process_exchange
  | Process_terminate
  | Process_reap

type operation =
  | Create_artifact of string
  | Write_artifact of { channel : out_channel; contents : string }
  | Flush_artifact
  | Sync_artifact of out_channel
  | Close_artifact
  | Publish_artifact of { partial_path : string; final_path : string }
  | Rename_artifact of { source_path : string; target_path : string }
  | Cleanup_artifact of string
  | Restore_artifact of { final_path : string; partial_path : string }
  | Sync_directory of string
  | Spawn_process
  | Exchange_process
  | Terminate_process
  | Reap_process

type t = { perform : 'a. operation -> (unit -> 'a) -> 'a }

val direct : t
val perform : t -> operation -> (unit -> 'a) -> 'a
val stage : operation -> stage
val stage_to_string : stage -> string
