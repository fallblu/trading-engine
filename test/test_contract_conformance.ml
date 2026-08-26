open Test_support
module T = Trading_engine

type path_component = Field of string | Index of int

let field name = function
  | `Assoc fields -> List.assoc name fields
  | _ -> Alcotest.failf "%s must be a JSON object field" name

let string_field name json =
  match field name json with
  | `String value -> value
  | _ -> Alcotest.failf "%s must be a JSON string" name

let optional_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> Alcotest.failf "%s must be read from a JSON object" name

let list_field name json =
  match field name json with
  | `List values -> values
  | _ -> Alcotest.failf "%s must be a JSON array" name

let path_of_yojson = function
  | `List components ->
      List.map
        (function
          | `String name -> Field name
          | `Int index -> Index index
          | _ -> Alcotest.fail "mutation paths contain only fields and indexes")
        components
  | _ -> Alcotest.fail "mutation path must be a JSON array"

let rec find_path path json =
  match (path, json) with
  | [], value -> value
  | Field name :: remaining, `Assoc fields ->
      find_path remaining (List.assoc name fields)
  | Index index :: remaining, `List values ->
      find_path remaining (List.nth values index)
  | _ -> Alcotest.fail "mutation path does not select a value"

let rec set_path path replacement json =
  match (path, json) with
  | [], _ -> replacement
  | Field name :: remaining, `Assoc fields ->
      let found = ref false in
      let fields =
        List.map
          (fun (candidate, value) ->
            if String.equal candidate name then (
              found := true;
              (candidate, set_path remaining replacement value))
            else (candidate, value))
          fields
      in
      let fields =
        if !found then fields
        else
          match remaining with
          | [] -> fields @ [ (name, replacement) ]
          | _ -> Alcotest.fail "mutation cannot add a nested missing field"
      in
      `Assoc fields
  | Index index :: remaining, `List values ->
      `List
        (List.mapi
           (fun candidate value ->
             if candidate = index then set_path remaining replacement value
             else value)
           values)
  | _ -> Alcotest.fail "mutation path cannot be replaced"

let rec remove_path path json =
  match (path, json) with
  | [ Field name ], `Assoc fields ->
      `Assoc
        (List.filter
           (fun (candidate, _) -> not (String.equal name candidate))
           fields)
  | Field name :: remaining, `Assoc fields ->
      `Assoc
        (List.map
           (fun (candidate, value) ->
             if String.equal candidate name then
               (candidate, remove_path remaining value)
             else (candidate, value))
           fields)
  | Index index :: remaining, `List values ->
      `List
        (List.mapi
           (fun candidate value ->
             if candidate = index then remove_path remaining value else value)
           values)
  | _ -> Alcotest.fail "mutation path cannot be removed"

let apply_mutation document mutation =
  let operation = string_field "op" mutation in
  let path = field "path" mutation |> path_of_yojson in
  match operation with
  | "remove" -> remove_path path document
  | "add" | "replace" -> set_path path (field "value" mutation) document
  | "append_copy" -> (
      let index =
        match field "index" mutation with
        | `Int value -> value
        | _ -> Alcotest.fail "append_copy index must be an integer"
      in
      match find_path path document with
      | `List values ->
          set_path path (`List (values @ [ List.nth values index ])) document
      | _ -> Alcotest.fail "append_copy path must select an array")
  | value -> Alcotest.failf "unsupported mutation operation %s" value

let apply_mutations case document =
  List.fold_left apply_mutation document (list_field "mutations" case)

let contract_path relative = Filename.concat "../contracts" relative
let read_json relative = contract_path relative |> Yojson.Safe.from_file

let read_jsonl relative =
  In_channel.with_open_bin (contract_path relative) In_channel.input_lines
  |> List.filter (fun line -> not (String.equal line ""))
  |> List.map Yojson.Safe.from_string

let selected_record case records =
  match optional_field "record" case with
  | Some (`Int line_number) -> List.nth records (line_number - 1)
  | _ -> Alcotest.fail "differential case must select a source record"

let extracted case document =
  match optional_field "extract" case with
  | Some path -> find_path (path_of_yojson path) document
  | None -> document

let with_stream records function_ =
  let path = Filename.temp_file "trading-engine-conformance" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      Out_channel.with_open_bin path (fun channel ->
          List.iter
            (fun record ->
              Yojson.Safe.to_channel channel record;
              output_char channel '\n')
            records);
      function_ path)

let parse_stream path =
  In_channel.with_open_bin path (fun channel ->
      T.Scenario_stream.fold_channel
        ~max_record_bytes:T.Resource_limits.scenario_record_bytes channel
        ~init:(fun _ -> Ok ())
        ~step:(fun () _ -> Ok ())
        ~finish:(fun () ~slice_count:_ -> Ok ()))

let runtime_result case =
  let source = string_field "source" case in
  match string_field "kind" case with
  | "scenario" ->
      read_json source |> apply_mutations case |> T.Scenario.of_yojson
      |> Result.map (fun _ -> ())
  | "scenario_stream" ->
      let records = read_jsonl source in
      let records =
        match optional_field "record" case with
        | None -> records
        | Some (`Int line_number) ->
            List.mapi
              (fun index record ->
                if index = line_number - 1 then apply_mutations case record
                else record)
              records
        | _ -> Alcotest.fail "stream record must be an integer"
      in
      with_stream records parse_stream
  | "strategy_response" ->
      let response =
        read_jsonl source |> selected_record case |> extracted case
        |> apply_mutations case
      in
      let expected_sequence =
        string_field "expected_sequence" case |> Int64.of_string
      in
      T.Strategy_protocol.response_of_yojson ~expected_sequence response
      |> Result.map (fun _ -> ())
  | kind -> Alcotest.failf "unsupported differential runtime kind %s" kind

let check_case case =
  let name = string_field "name" case in
  let expected = string_field "runtime_expectation" case in
  let accepted = Result.is_ok (runtime_result case) in
  Alcotest.(check bool)
    (name ^ " runtime expectation")
    (String.equal expected "accept")
    accepted

let cases () =
  match read_json "conformance/cases.json" with
  | `Assoc fields -> (
      match List.assoc "cases" fields with
      | `List values -> values
      | _ -> Alcotest.fail "differential corpus cases must be an array")
  | _ -> Alcotest.fail "differential corpus must be an object"

let tests =
  cases ()
  |> List.map (fun case ->
      Alcotest.test_case (string_field "name" case) `Quick (fun () ->
          check_case case))
