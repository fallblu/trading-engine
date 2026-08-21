type stage =
  | Artifact_create
  | Artifact_write
  | Artifact_flush
  | Artifact_close
  | Artifact_publish
  | Artifact_cleanup
  | Artifact_restore
  | Process_spawn
  | Process_exchange
  | Process_terminate
  | Process_reap

type operation =
  | Create_artifact of string
  | Write_artifact of { channel : out_channel; contents : string }
  | Flush_artifact
  | Close_artifact
  | Publish_artifact of { partial_path : string; final_path : string }
  | Cleanup_artifact of string
  | Restore_artifact of { final_path : string; partial_path : string }
  | Spawn_process
  | Exchange_process
  | Terminate_process
  | Reap_process

type t = { perform : 'a. operation -> (unit -> 'a) -> 'a }

let direct = { perform = (fun _ run -> run ()) }

let perform (type result) effects operation (run : unit -> result) =
  effects.perform operation run

let stage = function
  | Create_artifact _ -> Artifact_create
  | Write_artifact _ -> Artifact_write
  | Flush_artifact -> Artifact_flush
  | Close_artifact -> Artifact_close
  | Publish_artifact _ -> Artifact_publish
  | Cleanup_artifact _ -> Artifact_cleanup
  | Restore_artifact _ -> Artifact_restore
  | Spawn_process -> Process_spawn
  | Exchange_process -> Process_exchange
  | Terminate_process -> Process_terminate
  | Reap_process -> Process_reap

let stage_to_string = function
  | Artifact_create -> "artifact create"
  | Artifact_write -> "artifact write"
  | Artifact_flush -> "artifact flush"
  | Artifact_close -> "artifact close"
  | Artifact_publish -> "artifact publish"
  | Artifact_cleanup -> "artifact cleanup"
  | Artifact_restore -> "artifact restore"
  | Process_spawn -> "process spawn"
  | Process_exchange -> "process exchange"
  | Process_terminate -> "process terminate"
  | Process_reap -> "process reap"
