let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let invalid ?json_path ?line ?sequence message =
  Diagnostic.make ?json_path ?line ?sequence
    ~code:Diagnostic.Scenario_stream_invalid ~phase:Diagnostic.Validation
    message

let object_fields ~name ~expected = function
  | `Assoc fields ->
      let names = List.map fst fields in
      let actual = List.sort_uniq String.compare names in
      let expected = List.sort_uniq String.compare expected in
      if List.length names <> List.length actual then
        Error (invalid (name ^ " has duplicate JSON fields"))
      else if actual = expected then Ok fields
      else
        let missing =
          List.filter (fun key -> not (List.mem key actual)) expected
        in
        let extra =
          List.filter (fun key -> not (List.mem key expected)) actual
        in
        Error
          (invalid
             (Printf.sprintf "%s fields differ: missing=[%s], extra=[%s]" name
                (String.concat "," missing)
                (String.concat "," extra)))
  | _ -> Error (invalid (name ^ " must be a JSON object"))

let field fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error (invalid ("missing JSON field: " ^ name))

let string ~name = function
  | `String value -> Ok value
  | _ -> Error (invalid (name ^ " must be a string"))

let int64_string ~name ~positive value =
  let* value = string ~name value in
  match Int64.of_string_opt value with
  | Some parsed
    when String.equal (Int64.to_string parsed) value
         && ((not positive) || Int64.compare parsed 0L > 0)
         && (positive || Int64.compare parsed 0L >= 0) ->
      Ok parsed
  | _ ->
      let requirement = if positive then "a positive" else "a nonnegative" in
      Error
        (invalid (name ^ " must be " ^ requirement ^ " canonical int64 string"))

let parse_json ~line_number line =
  if String.equal line "" then
    Error
      (invalid ~line:line_number ~json_path:"$"
         "scenario stream must not contain blank records")
  else
    try Ok (Yojson.Safe.from_string line)
    with Yojson.Json_error message as exception_ ->
      Error
        (Diagnostic.of_exception ~code:Diagnostic.Scenario_invalid_json
           ~phase:Diagnostic.Input ~line:line_number ~json_path:"$"
           ~message:
             (Printf.sprintf "invalid scenario stream JSON at line %d: %s"
                line_number message)
           exception_)

type envelope = {
  contract_version : string;
  record_type : string;
  payload : Yojson.Safe.t;
}

let parse_envelope ~line_number ~expected_sequence line =
  let result =
    let* json = parse_json ~line_number line in
    let name = Printf.sprintf "scenario stream record %d" line_number in
    let* fields =
      object_fields ~name
        ~expected:
          [ "contract_version"; "scenario_sequence"; "record_type"; "payload" ]
        json
    in
    let* contract_json = field fields "contract_version" in
    let* contract_version = string ~name:"contract_version" contract_json in
    if not (Contract.is_supported contract_version) then
      Error
        (Diagnostic.make ~code:Diagnostic.Scenario_unsupported_contract
           ~phase:Diagnostic.Validation ~line:line_number
           ~sequence:expected_sequence ~json_path:"$.contract_version"
           (Printf.sprintf
              "unsupported scenario contract_version %S (expected one of %s)"
              contract_version
              (String.concat ", " Contract.supported_versions)))
    else
      let* sequence_json = field fields "scenario_sequence" in
      let* scenario_sequence =
        int64_string ~name:"scenario_sequence" ~positive:true sequence_json
      in
      if not (Int64.equal scenario_sequence expected_sequence) then
        Error
          (invalid ~line:line_number ~sequence:scenario_sequence
             ~json_path:"$.scenario_sequence"
             "scenario_sequence must be contiguous and start at one")
      else
        let* type_json = field fields "record_type" in
        let* record_type = string ~name:"record_type" type_json in
        let* payload = field fields "payload" in
        Ok { contract_version; record_type; payload }
  in
  Result.map_error
    (Diagnostic.annotate ~line:line_number ~sequence:expected_sequence
       ~json_path:"$")
    result

let successor sequence =
  if Int64.equal sequence Int64.max_int then
    Error
      (invalid ~sequence ~json_path:"$.scenario_sequence"
         "scenario_sequence is exhausted")
  else Ok (Int64.succ sequence)

let footer_count payload =
  let* fields =
    object_fields ~name:"scenario stream end payload"
      ~expected:[ "slice_count" ] payload
  in
  let* count_json = field fields "slice_count" in
  int64_string ~name:"slice_count" ~positive:false count_json

let fold_channel channel ~init ~step ~finish =
  let line_number = ref 1 in
  match In_channel.input_line channel with
  | None ->
      Error
        (invalid ~line:1 ~sequence:1L
           "scenario stream must start with scenario_header")
  | Some line ->
      let* envelope =
        parse_envelope ~line_number:1 ~expected_sequence:1L line
      in
      if not (String.equal envelope.record_type "scenario_header") then
        Error
          (invalid ~line:1 ~sequence:1L ~json_path:"$.record_type"
             "scenario_header must be the first scenario stream record")
      else
        let* header =
          Scenario.stream_header_of_yojson
            ~contract_version:envelope.contract_version envelope.payload
          |> Result.map_error
               (Diagnostic.annotate ~line:1 ~sequence:1L ~json_path:"$.payload")
        in
        let* state = init header in
        let rec loop state previous slice_count expected_sequence =
          incr line_number;
          match In_channel.input_line channel with
          | None ->
              Error
                (invalid ~line:!line_number ~sequence:expected_sequence
                   "scenario_end must terminate the scenario stream")
          | Some line ->
              let* envelope =
                parse_envelope ~line_number:!line_number ~expected_sequence line
              in
              if
                not
                  (String.equal envelope.contract_version
                     header.contract_version)
              then
                Error
                  (invalid ~line:!line_number ~sequence:expected_sequence
                     ~json_path:"$.contract_version"
                     "scenario stream contract_version must remain constant")
              else if String.equal envelope.record_type "market_slice" then
                let* item =
                  Scenario.stream_item_of_yojson header ~previous
                    envelope.payload
                  |> Result.map_error
                       (Diagnostic.annotate ~line:!line_number
                          ~sequence:expected_sequence ~json_path:"$.payload")
                in
                let* state = step state item in
                let* expected_sequence = successor expected_sequence in
                if Int64.equal slice_count Int64.max_int then
                  Error
                    (invalid ~line:!line_number ~sequence:expected_sequence
                       "scenario slice count is exhausted")
                else
                  loop state (Some item) (Int64.succ slice_count)
                    expected_sequence
              else if String.equal envelope.record_type "scenario_end" then
                let* declared_count =
                  footer_count envelope.payload
                  |> Result.map_error
                       (Diagnostic.annotate ~line:!line_number
                          ~sequence:expected_sequence ~json_path:"$.payload")
                in
                if not (Int64.equal declared_count slice_count) then
                  Error
                    (invalid ~line:!line_number ~sequence:expected_sequence
                       ~json_path:"$.payload.slice_count"
                       "scenario_end slice_count differs from streamed market \
                        slices")
                else
                  match In_channel.input_line channel with
                  | Some _ ->
                      Error
                        (invalid ~line:(!line_number + 1)
                           "scenario_end must be the terminal scenario stream \
                            record")
                  | None -> finish state ~slice_count
              else
                Error
                  (invalid ~line:!line_number ~sequence:expected_sequence
                     ~json_path:"$.record_type"
                     ("unsupported scenario stream record_type: "
                    ^ envelope.record_type))
        in
        let* expected_sequence = successor 1L in
        loop state None 0L expected_sequence

let fold_file path ~init ~step ~finish =
  try
    In_channel.with_open_bin path (fun channel ->
        fold_channel channel ~init ~step ~finish)
  with Sys_error message as exception_ ->
    Error
      (Diagnostic.of_exception ~code:Diagnostic.Input_io ~phase:Diagnostic.Input
         ~message:("could not read scenario stream: " ^ message)
         exception_)
