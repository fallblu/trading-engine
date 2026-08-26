module T = Trading_engine

type boundary =
  | Batch
  | Stream
  | Strategy
  | Timestamp
  | Decimal
  | Identifier
  | Json

type seed = { name : string; boundary : boundary; input : string }
type path_component = Field of string | Index of int

let boundary_name = function
  | Batch -> "batch"
  | Stream -> "stream"
  | Strategy -> "strategy"
  | Timestamp -> "timestamp"
  | Decimal -> "decimal"
  | Identifier -> "identifier"
  | Json -> "json"

let contracts =
  if Sys.file_exists "../contracts/conformance/cases.json" then "../contracts"
  else if Sys.file_exists "contracts/conformance/cases.json" then "contracts"
  else failwith "could not locate the contract corpus"

let contract_path relative = Filename.concat contracts relative

let field name = function
  | `Assoc fields -> List.assoc name fields
  | _ -> failwith (name ^ " must be read from an object")

let optional_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> failwith (name ^ " must be read from an object")

let string_field name json =
  match field name json with
  | `String value -> value
  | _ -> failwith (name ^ " must be a string")

let list_field name json =
  match field name json with
  | `List values -> values
  | _ -> failwith (name ^ " must be an array")

let path_of_yojson = function
  | `List components ->
      List.map
        (function
          | `String name -> Field name
          | `Int index -> Index index
          | _ -> failwith "mutation path contains an invalid component")
        components
  | _ -> failwith "mutation path must be an array"

let rec find_path path json =
  match (path, json) with
  | [], value -> value
  | Field name :: remaining, `Assoc fields ->
      find_path remaining (List.assoc name fields)
  | Index index :: remaining, `List values ->
      find_path remaining (List.nth values index)
  | _ -> failwith "mutation path does not select a value"

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
          | _ -> failwith "mutation cannot add a nested missing field"
      in
      `Assoc fields
  | Index index :: remaining, `List values ->
      `List
        (List.mapi
           (fun candidate value ->
             if candidate = index then set_path remaining replacement value
             else value)
           values)
  | _ -> failwith "mutation path cannot be replaced"

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
  | _ -> failwith "mutation path cannot be removed"

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
        | _ -> failwith "append_copy index must be an integer"
      in
      match find_path path document with
      | `List values ->
          set_path path (`List (values @ [ List.nth values index ])) document
      | _ -> failwith "append_copy target must be an array")
  | value -> failwith ("unsupported mutation operation " ^ value)

let apply_mutations case document =
  List.fold_left apply_mutation document (list_field "mutations" case)

let read_json relative = Yojson.Safe.from_file (contract_path relative)

let read_jsonl relative =
  In_channel.with_open_bin (contract_path relative) In_channel.input_lines
  |> List.filter (fun line -> not (String.equal line ""))
  |> List.map Yojson.Safe.from_string

let select_record case records =
  match optional_field "record" case with
  | Some (`Int line_number) -> List.nth records (line_number - 1)
  | _ -> failwith "conformance case must select a record"

let extract case document =
  match optional_field "extract" case with
  | Some path -> find_path (path_of_yojson path) document
  | None -> document

let materialize_case case =
  let name = "conformance/" ^ string_field "name" case in
  match optional_field "kind" case with
  | Some (`String "scenario") ->
      {
        name;
        boundary = Batch;
        input =
          read_json (string_field "source" case)
          |> apply_mutations case |> Yojson.Safe.to_string;
      }
  | Some (`String "scenario_stream") ->
      let records = read_jsonl (string_field "source" case) in
      let records =
        match optional_field "record" case with
        | None -> records
        | Some (`Int line_number) ->
            List.mapi
              (fun index record ->
                if index = line_number - 1 then apply_mutations case record
                else record)
              records
        | _ -> failwith "stream record must be an integer"
      in
      {
        name;
        boundary = Stream;
        input =
          ( records |> List.map Yojson.Safe.to_string |> String.concat "\n"
          |> fun document -> document ^ "\n" );
      }
  | Some (`String "strategy_response") ->
      {
        name;
        boundary = Strategy;
        input =
          read_jsonl (string_field "source" case)
          |> select_record case |> extract case |> apply_mutations case
          |> Yojson.Safe.to_string;
      }
  | Some (`String kind) -> failwith ("unsupported conformance kind " ^ kind)
  | Some _ -> failwith "conformance kind must be a string"
  | None ->
      let document =
        match optional_field "instance" case with
        | Some value -> value
        | None ->
            read_jsonl (string_field "source" case)
            |> select_record case |> extract case
      in
      {
        name;
        boundary =
          (if
             String.starts_with ~prefix:"strategy-message"
               (string_field "artifact" case)
           then Strategy
           else Json);
        input = apply_mutations case document |> Yojson.Safe.to_string;
      }

let rec fixture_files directory =
  Sys.readdir directory |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
      let path = Filename.concat directory name in
      if Sys.is_directory path then fixture_files path else [ path ])

let relative_to_contracts path =
  let prefix = contracts ^ Filename.dir_sep in
  String.sub path (String.length prefix)
    (String.length path - String.length prefix)

let canonical_seeds () =
  fixture_files contracts
  |> List.filter (fun path ->
      String.split_on_char '/' path |> List.mem "fixtures")
  |> List.concat_map (fun path ->
      let relative = relative_to_contracts path in
      let contents = In_channel.with_open_bin path In_channel.input_all in
      if String.ends_with ~suffix:".scenario.json" path then
        [ { name = relative; boundary = Batch; input = contents } ]
      else if String.ends_with ~suffix:".scenario.jsonl" path then
        [ { name = relative; boundary = Stream; input = contents } ]
      else if String.ends_with ~suffix:".strategy.jsonl" path then
        contents |> String.split_on_char '\n'
        |> List.filter (fun line -> not (String.equal line ""))
        |> List.mapi (fun index line ->
            let record = Yojson.Safe.from_string line in
            [
              {
                name = Printf.sprintf "%s:%d" relative (index + 1);
                boundary = Json;
                input = line;
              };
            ]
            @
            match optional_field "message" record with
            | Some message ->
                [
                  {
                    name = Printf.sprintf "%s:%d/message" relative (index + 1);
                    boundary = Strategy;
                    input = Yojson.Safe.to_string message;
                  };
                ]
            | None -> [])
        |> List.flatten
      else
        contents |> String.split_on_char '\n'
        |> List.filter (fun line -> not (String.equal line ""))
        |> List.mapi (fun index line ->
            {
              name = Printf.sprintf "%s:%d" relative (index + 1);
              boundary = Json;
              input = line;
            }))

let conformance_seeds () =
  match read_json "conformance/cases.json" with
  | `Assoc fields ->
      let cases name =
        match List.assoc name fields with
        | `List values -> values
        | _ -> failwith (name ^ " must be an array")
      in
      List.map materialize_case (cases "cases" @ cases "schema_only_cases")
  | _ -> failwith "conformance cases must be an object"

let primitive_seeds =
  [
    {
      name = "timestamp/canonical";
      boundary = Timestamp;
      input = "2026-01-02T14:30:00Z";
    };
    {
      name = "timestamp/offset";
      boundary = Timestamp;
      input = "2026-01-02T14:30:00-05:00";
    };
    {
      name = "timestamp/leap-second";
      boundary = Timestamp;
      input = "2026-01-02T14:30:60Z";
    };
    { name = "decimal/zero"; boundary = Decimal; input = "0" };
    { name = "decimal/signed"; boundary = Decimal; input = "-1.000001" };
    { name = "decimal/noncanonical"; boundary = Decimal; input = "01.0" };
    {
      name = "identifier/canonical";
      boundary = Identifier;
      input = "fuzz-id_01";
    };
    { name = "identifier/empty"; boundary = Identifier; input = "" };
    { name = "identifier/control"; boundary = Identifier; input = "bad\000id" };
  ]

let deep_json depth = String.make depth '[' ^ "null" ^ String.make depth ']'

let hostile_seeds () =
  let huge_json_string = "\"" ^ String.make 1_048_577 'x' ^ "\"" in
  let json_inputs =
    [
      ("malformed-utf8", String.make 1 (Char.chr 255));
      ("deep-nesting", deep_json 512);
      ("huge-token", huge_json_string);
      ( "duplicate-key",
        "{\"contract_version\":\"1\",\"contract_version\":\"1\"}" );
      ("truncation", "{\"contract_version\":");
    ]
  in
  List.concat_map
    (fun boundary ->
      List.map
        (fun (name, input) -> { name = "hostile/" ^ name; boundary; input })
        json_inputs)
    [ Batch; Stream; Strategy; Json ]
  @ [
      {
        name = "timestamp/huge";
        boundary = Timestamp;
        input = String.make 4096 '9';
      };
      {
        name = "decimal/huge";
        boundary = Decimal;
        input = String.make 4096 '9';
      };
      {
        name = "identifier/huge";
        boundary = Identifier;
        input = String.make 4096 'x';
      };
    ]

let with_stream document function_ =
  let path = Filename.temp_file "trading-engine-fuzz" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      Out_channel.with_open_bin path (fun channel ->
          output_string channel document);
      function_ path)

let expected_sequence document =
  try
    match Yojson.Safe.from_string document with
    | `Assoc fields -> (
        match List.assoc_opt "strategy_sequence" fields with
        | Some (`String value) ->
            Option.value (Int64.of_string_opt value) ~default:1L
        | _ -> 1L)
    | _ -> 1L
  with Yojson.Json_error _ -> 1L

let exercise boundary input =
  match boundary with
  | Batch -> ignore (T.Scenario.of_string input)
  | Stream ->
      with_stream input (fun path ->
          ignore
            (T.Scenario_stream.fold_file path
               ~init:(fun _ -> Ok ())
               ~step:(fun () _ -> Ok ())
               ~finish:(fun () ~slice_count:_ -> Ok ())))
  | Strategy ->
      ignore
        (T.Strategy_protocol.response_of_string
           ~expected_sequence:(expected_sequence input) input)
  | Timestamp -> ignore (T.Codec.ptime_of_string input)
  | Decimal ->
      ignore (T.Scalar.Price.of_decimal_string input);
      ignore (T.Scalar.Quantity.of_decimal_string input);
      ignore (T.Scalar.Money.of_decimal_string input);
      ignore (T.Scalar.Weight.of_decimal_string input);
      ignore (T.Scalar.Ratio.of_decimal_string input)
  | Identifier ->
      ignore (T.Id.Run.of_string input);
      ignore (T.Id.Instrument.of_string input);
      ignore (T.Id.Order.of_string input);
      ignore (T.Id.Fill.of_string input);
      ignore (T.Id.Strategy.of_string input);
      ignore (T.Id.Event.of_string input);
      ignore (T.Id.Corporate_action.of_string input)
  | Json -> (
      try ignore (Yojson.Safe.from_string input)
      with Yojson.Json_error _ -> ())

let hex_prefix input =
  let length = Int.min 128 (String.length input) in
  String.init (length * 2) (fun index ->
      let byte = Char.code input.[index / 2] in
      let nibble = if index mod 2 = 0 then byte lsr 4 else byte land 15 in
      "0123456789abcdef".[nibble])

let run seed =
  try exercise seed.boundary seed.input
  with exception_ ->
    Printf.eprintf
      "fuzz failure boundary=%s name=%s bytes=%d prefix=%s exception=%s\n%!"
      (boundary_name seed.boundary)
      seed.name (String.length seed.input) (hex_prefix seed.input)
      (Printexc.to_string exception_);
    exit 1

let mutate state input =
  let length = String.length input in
  match Random.State.int state 5 with
  | 0 ->
      if length = 0 then input
      else String.sub input 0 (Random.State.int state length)
  | 1 ->
      if length = 0 then String.make 1 (Char.chr (Random.State.int state 256))
      else
        let bytes = Bytes.of_string input in
        let index = Random.State.int state length in
        Bytes.set bytes index (Char.chr (Random.State.int state 256));
        Bytes.unsafe_to_string bytes
  | 2 ->
      let index = Random.State.int state (length + 1) in
      let byte = String.make 1 (Char.chr (Random.State.int state 256)) in
      String.sub input 0 index ^ byte ^ String.sub input index (length - index)
  | 3 ->
      if length = 0 then input
      else
        let start = Random.State.int state length in
        let count = 1 + Random.State.int state (length - start) in
        String.sub input 0 start
        ^ String.sub input (start + count) (length - start - count)
  | _ ->
      if length = 0 then input
      else
        let start = Random.State.int state length in
        let maximum = Int.min 64 (length - start) in
        let count = 1 + Random.State.int state maximum in
        let insertion = Random.State.int state (length + 1) in
        String.sub input 0 insertion
        ^ String.sub input start count
        ^ String.sub input insertion (length - insertion)

let () =
  let random_seed = ref 20_260_821 in
  let cases = ref 256 in
  Arg.parse
    [
      ("--seed", Arg.Set_int random_seed, "deterministic random seed");
      ("--cases", Arg.Set_int cases, "number of byte-mutation cases");
    ]
    (fun argument -> raise (Arg.Bad ("unexpected argument " ^ argument)))
    "fuzz_protocol [--seed INTEGER] [--cases INTEGER]";
  if !cases < 0 then raise (Arg.Bad "--cases must be nonnegative");
  let corpus = canonical_seeds () @ conformance_seeds () @ primitive_seeds in
  List.iter run corpus;
  List.iter run (hostile_seeds ());
  let state = Random.State.make [| !random_seed |] in
  for index = 1 to !cases do
    let source =
      List.nth corpus (Random.State.int state (List.length corpus))
    in
    run
      {
        source with
        name = Printf.sprintf "mutation/%d/%s" index source.name;
        input = mutate state source.input;
      }
  done;
  Printf.printf "fuzz seed=%d cases=%d corpus=%d status=ok\n" !random_seed
    !cases (List.length corpus)
