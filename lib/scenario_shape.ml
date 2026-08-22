type error = { json_path : string; message : string }

type common = {
  metadata : Yojson.Safe.t;
  run_id : Yojson.Safe.t;
  base_currency : Yojson.Safe.t;
  initial_state : Yojson.Safe.t;
  instruments : Yojson.Safe.t;
  venue_calendars : Yojson.Safe.t option;
  risk : Yojson.Safe.t;
  execution : Yojson.Safe.t;
  max_internal_events : Yojson.Safe.t;
}

type batch = {
  contract_version : Yojson.Safe.t;
  common : common;
  schedule : Yojson.Safe.t;
  slices : Yojson.Safe.t;
}

type stream_item = { market_slice : Yojson.Safe.t; intents : Yojson.Safe.t }

let error ~json_path message = { json_path; message }

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let object_fields ~json_path ~name ~expected = function
  | `Assoc fields ->
      let names = List.map fst fields in
      let actual = List.sort_uniq String.compare names in
      let expected = List.sort_uniq String.compare expected in
      if List.length names <> List.length actual then
        let duplicates =
          List.filter
            (fun key -> List.length (List.filter (String.equal key) names) > 1)
            actual
        in
        Error
          (error ~json_path
             (Printf.sprintf "%s has duplicate JSON fields: [%s]" name
                (String.concat "," duplicates)))
      else if actual = expected then Ok fields
      else
        let missing =
          List.filter (fun key -> not (List.mem key actual)) expected
        in
        let extra =
          List.filter (fun key -> not (List.mem key expected)) actual
        in
        Error
          (error ~json_path
             (Printf.sprintf "%s fields differ: missing=[%s], extra=[%s]" name
                (String.concat "," missing)
                (String.concat "," extra)))
  | _ -> Error (error ~json_path (name ^ " must be a JSON object"))

let field ~root fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None ->
      Error
        (error ~json_path:(root ^ "." ^ name) ("missing JSON field: " ^ name))

let common ~root ~contract_version fields =
  let* metadata = field ~root fields "metadata" in
  let* run_id = field ~root fields "run_id" in
  let* base_currency = field ~root fields "base_currency" in
  let initial_field =
    if List.mem contract_version [ "7"; "6" ] then "initial_portfolio"
    else "initial_cash"
  in
  let* initial_state = field ~root fields initial_field in
  let* instruments = field ~root fields "instruments" in
  let venue_calendars =
    if List.mem contract_version [ "7"; "6"; "5" ] then
      List.assoc_opt "venue_calendars" fields
    else None
  in
  let* risk = field ~root fields "risk" in
  let* execution = field ~root fields "execution" in
  let* max_internal_events = field ~root fields "max_internal_events" in
  Ok
    {
      metadata;
      run_id;
      base_currency;
      initial_state;
      instruments;
      venue_calendars;
      risk;
      execution;
      max_internal_events;
    }

let batch json =
  let root = "$" in
  let* preliminary =
    match json with
    | `Assoc fields -> field ~root fields "contract_version"
    | _ -> Error (error ~json_path:root "scenario must be a JSON object")
  in
  let contract_version =
    match preliminary with `String value -> value | _ -> ""
  in
  let calendar_fields =
    if List.mem contract_version [ "7"; "6"; "5" ] then [ "venue_calendars" ]
    else []
  in
  let initial_field =
    if List.mem contract_version [ "7"; "6" ] then "initial_portfolio"
    else "initial_cash"
  in
  let* fields =
    object_fields ~json_path:root ~name:"scenario"
      ~expected:
        ([
           "contract_version";
           "metadata";
           "run_id";
           "base_currency";
           initial_field;
           "instruments";
           "risk";
           "execution";
           "max_internal_events";
           "schedule";
           "slices";
         ]
        @ calendar_fields)
      json
  in
  let* contract_version_json = field ~root fields "contract_version" in
  let* common = common ~root ~contract_version fields in
  let* schedule = field ~root fields "schedule" in
  let* slices = field ~root fields "slices" in
  Ok { contract_version = contract_version_json; common; schedule; slices }

let stream_header ~contract_version json =
  let root = "$.payload" in
  let calendar_fields =
    if List.mem contract_version [ "7"; "6"; "5" ] then [ "venue_calendars" ]
    else []
  in
  let initial_field =
    if List.mem contract_version [ "7"; "6" ] then "initial_portfolio"
    else "initial_cash"
  in
  let* fields =
    object_fields ~json_path:root ~name:"scenario stream header payload"
      ~expected:
        ([
           "metadata";
           "run_id";
           "base_currency";
           initial_field;
           "instruments";
           "risk";
           "execution";
           "max_internal_events";
         ]
        @ calendar_fields)
      json
  in
  common ~root ~contract_version fields

let stream_item json =
  let root = "$.payload" in
  let* fields =
    object_fields ~json_path:root ~name:"scenario stream slice payload"
      ~expected:[ "market_slice"; "intents" ]
      json
  in
  let* market_slice = field ~root fields "market_slice" in
  let* intents = field ~root fields "intents" in
  Ok { market_slice; intents }
