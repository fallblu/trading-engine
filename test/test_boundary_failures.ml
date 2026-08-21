open Test_support
module T = Trading_engine

exception Injected_failure of string

let remove_if_exists path = if Sys.file_exists path then Sys.remove path

let with_absent_path suffix test =
  let path = Filename.temp_file "trading-engine-boundary" suffix in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      remove_if_exists path;
      remove_if_exists (path ^ ".partial"))
    (fun () -> test path)

let injected_effects target =
  let triggered = ref false in
  let perform : type result.
      T.Boundary_effects.operation -> (unit -> result) -> result =
   fun operation run ->
    if (not !triggered) && T.Boundary_effects.stage operation = target then (
      triggered := true;
      match operation with
      | T.Boundary_effects.Write_artifact { channel; contents } ->
          output_substring channel contents 0
            (Int.min 8 (String.length contents));
          raise (Injected_failure (T.Boundary_effects.stage_to_string target))
      | T.Boundary_effects.Publish_artifact { final_path; _ } ->
          Out_channel.with_open_bin final_path (fun channel ->
              output_string channel "rival\n");
          run ()
      | _ ->
          raise (Injected_failure (T.Boundary_effects.stage_to_string target)))
    else run ()
  in
  ({ T.Boundary_effects.perform }, triggered)

let audit =
  T.Audit.create ~contract_version:T.Contract.version ~engine_sequence:1L
    ~causation_ids:[]
    ~run_id:(run_id "boundary-failure")
    ~recorded_at:(timestamp "2026-01-02T21:00:02Z")
    (T.Audit.Run_started
       { scenario_sha256; execution_model = "completed_bar_v1" })

module type Artifact_writer = sig
  type t

  val create :
    effects:T.Boundary_effects.t -> string -> (t, T.Diagnostic.t) result

  val append : t -> (unit, T.Diagnostic.t) result
  val commit : t -> (unit, T.Diagnostic.t) result
  val close_preserving_partial : t -> unit
end

module Journal_writer = struct
  type t = T.Journal.t

  let create ~effects path = T.Journal.create ~effects path
  let append journal = T.Journal.append journal audit
  let commit = T.Journal.commit
  let close_preserving_partial = T.Journal.close_preserving_partial
end

module Transcript_writer = struct
  type t = T.Strategy_transcript.t

  let create ~effects path = T.Strategy_transcript.create ~effects path

  let append transcript =
    T.Strategy_transcript.append transcript
      ~direction:T.Strategy_protocol.Engine_to_strategy
      (T.Strategy_protocol.shutdown_message ~sequence:1L)

  let commit = T.Strategy_transcript.commit
  let close_preserving_partial = T.Strategy_transcript.close_preserving_partial
end

let artifact_stages =
  T.Boundary_effects.
    [
      Artifact_create;
      Artifact_write;
      Artifact_flush;
      Artifact_close;
      Artifact_publish;
      Artifact_cleanup;
    ]

let exercise_artifact_failure (type writer) writer_name
    (module Writer : Artifact_writer with type t = writer) stage =
  with_absent_path ".jsonl" @@ fun final_path ->
  let partial_path = final_path ^ ".partial" in
  let effects, triggered = injected_effects stage in
  let result =
    match Writer.create ~effects final_path with
    | Error _ as error -> error
    | Ok writer ->
        let result =
          match stage with
          | T.Boundary_effects.Artifact_write | Artifact_flush ->
              Writer.append writer
          | Artifact_close | Artifact_publish | Artifact_cleanup -> (
              match Writer.append writer with
              | Error _ as error -> error
              | Ok () -> Writer.commit writer)
          | Artifact_create -> Alcotest.fail "create failure was not injected"
          | Artifact_restore -> Alcotest.fail "expected a lifecycle failure"
          | Process_spawn | Process_exchange | Process_terminate | Process_reap
            ->
              Alcotest.fail "expected an artifact stage"
        in
        Writer.close_preserving_partial writer;
        result
  in
  let diagnostic = error result in
  Alcotest.(check bool) "fault triggered" true !triggered;
  Alcotest.(check string)
    "artifact diagnostic" "artifact.io"
    (T.Diagnostic.code_to_string diagnostic.code);
  let expected_final =
    stage = T.Boundary_effects.Artifact_publish
    || stage = T.Boundary_effects.Artifact_cleanup
  in
  let expected_partial = stage <> T.Boundary_effects.Artifact_create in
  Alcotest.(check bool)
    (writer_name ^ " final invariant")
    expected_final
    (Sys.file_exists final_path);
  Alcotest.(check bool)
    (writer_name ^ " partial invariant")
    expected_partial
    (Sys.file_exists partial_path);
  if stage = T.Boundary_effects.Artifact_write then
    Alcotest.(check int64)
      "short write retained" 8L
      (In_channel.with_open_bin partial_path In_channel.length);
  if stage = T.Boundary_effects.Artifact_publish then
    Alcotest.(check string)
      "publication rival preserved" "rival\n"
      (In_channel.with_open_bin final_path In_channel.input_all);
  if stage = T.Boundary_effects.Artifact_cleanup then
    Alcotest.(check string)
      "published and partial bytes agree"
      (In_channel.with_open_bin partial_path In_channel.input_all)
      (In_channel.with_open_bin final_path In_channel.input_all)

let artifact_cases writer_name writer =
  List.map
    (fun stage ->
      Alcotest.test_case
        (writer_name ^ " " ^ T.Boundary_effects.stage_to_string stage)
        `Quick
        (fun () -> exercise_artifact_failure writer_name writer stage))
    artifact_stages

let nth_failure target occurrence =
  let seen = ref 0 in
  let perform : type result.
      T.Boundary_effects.operation -> (unit -> result) -> result =
   fun operation run ->
    if T.Boundary_effects.stage operation = target then (
      seen := !seen + 1;
      if !seen = occurrence then
        raise (Injected_failure (T.Boundary_effects.stage_to_string target)));
    run ()
  in
  ({ T.Boundary_effects.perform }, seen)

let exercise_transaction_failure stage occurrence =
  with_absent_path ".journal.jsonl" @@ fun journal_path ->
  with_absent_path ".strategy.jsonl" @@ fun transcript_path ->
  let effects, seen = nth_failure stage occurrence in
  let journal =
    T.Artifact_writer.create ~effects ~label:"journal" journal_path |> ok
  in
  let transcript =
    T.Artifact_writer.create ~effects ~label:"strategy transcript"
      transcript_path
    |> ok
  in
  T.Artifact_writer.append journal "journal\n" |> ok;
  T.Artifact_writer.append transcript "transcript\n" |> ok;
  let diagnostic = T.Artifact_writer.commit [ journal; transcript ] |> error in
  Alcotest.(check int) "target occurrence reached" occurrence !seen;
  Alcotest.(check string)
    "artifact diagnostic" "artifact.io"
    (T.Diagnostic.code_to_string diagnostic.code);
  let finals_exist = stage = T.Boundary_effects.Artifact_cleanup in
  List.iter
    (fun path ->
      Alcotest.(check bool)
        "final-set invariant" finals_exist (Sys.file_exists path);
      Alcotest.(check bool)
        "partial-set invariant" true
        (Sys.file_exists (path ^ ".partial")))
    [ journal_path; transcript_path ];
  if finals_exist then
    List.iter
      (fun path ->
        Alcotest.(check string)
          "final and restored partial agree"
          (In_channel.with_open_bin path In_channel.input_all)
          (In_channel.with_open_bin (path ^ ".partial") In_channel.input_all))
      [ journal_path; transcript_path ]

let transaction_cases =
  List.concat_map
    (fun stage ->
      List.map
        (fun occurrence ->
          Alcotest.test_case
            (Printf.sprintf "transaction %s %d"
               (T.Boundary_effects.stage_to_string stage)
               occurrence)
            `Quick
            (fun () -> exercise_transaction_failure stage occurrence))
        [ 1; 2 ])
    T.Boundary_effects.[ Artifact_close; Artifact_publish; Artifact_cleanup ]

let initialization () =
  let instrument = instrument () in
  T.Strategy_protocol.
    {
      scenario_contract_version = T.Contract.version;
      scenario_sha256;
      metadata = `Assoc [ ("experiment", `String "boundary-failure") ];
      run_id = run_id "boundary-failure";
      base_currency = "USD";
      initial_cash = [ ("USD", money "10000") ];
      instruments = [ instrument ];
      risk = risk ~instruments:[ instrument ] ();
      execution_model = T.Execution_model.find "completed_bar_v1" |> ok;
      execution = execution ();
    }

let process_stages =
  T.Boundary_effects.
    [ Process_spawn; Process_exchange; Process_terminate; Process_reap ]

let exercise_process_failure stage =
  with_absent_path ".strategy.jsonl" @@ fun final_path ->
  let effects, triggered = injected_effects stage in
  let result =
    Eio_main.run @@ fun env ->
    T.Strategy_process.with_session ~effects ~env
      ~command:[ "./fake_strategy.py"; "success" ]
      ~timeout:1.0 ~transcript_path:final_path
      ~initialization:(initialization ()) (fun _ -> Ok ())
  in
  let diagnostic = error result in
  Alcotest.(check bool) "fault triggered" true !triggered;
  Alcotest.(check string)
    "process diagnostic" "strategy.process"
    (T.Diagnostic.code_to_string diagnostic.code);
  Alcotest.(check bool) "final absent" false (Sys.file_exists final_path);
  Alcotest.(check bool)
    "partial retained" true
    (Sys.file_exists (final_path ^ ".partial"))

let process_cases =
  List.map
    (fun stage ->
      Alcotest.test_case (T.Boundary_effects.stage_to_string stage) `Quick
        (fun () -> exercise_process_failure stage))
    process_stages

let tests =
  artifact_cases "journal" (module Journal_writer)
  @ artifact_cases "transcript" (module Transcript_writer)
  @ transaction_cases @ process_cases
