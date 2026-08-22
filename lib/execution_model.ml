module type S = sig
  val name : string

  val start_slice :
    Execution.t ->
    instruments:Instrument.t list ->
    oms:Oms.t ->
    Market_slice.t ->
    (Execution.cursor, string) result
end

type t = (module S)

type configuration_contract = {
  version : string;
  previous_versions : string list;
  scenario_contract_versions : string list;
  required_fields : string list;
  legacy_required_fields : string list;
  supported_order_types : string list;
  data_requirements : string list;
  limits : Yojson.Safe.t;
}

module Completed_bar_v1 = struct
  let name = "completed_bar_v1"
  let start_slice = Execution.start_slice
end

let of_module model = model
let name (module Model : S) = Model.name
let builtins : t list = [ (module Completed_bar_v1) ]
let supported = List.map name builtins

let completed_bar_v1_contract =
  {
    version = "2";
    previous_versions = [ "1" ];
    scenario_contract_versions = [ "10"; "9"; "8"; "7"; "6"; "5"; "4"; "3" ];
    required_fields = [ "version"; "participation_bps"; "fee_schedules" ];
    legacy_required_fields =
      [ "version"; "participation_bps"; "fixed_fee"; "fee_bps" ];
    supported_order_types = [ "market"; "limit"; "stop"; "stop_limit" ];
    data_requirements = [ "completed_ohlcv_bars" ];
    limits =
      `Assoc
        [
          ( "participation_bps",
            `Assoc [ ("minimum", `Int 0); ("maximum", `Int 10_000) ] );
          ("fee_bps", `Assoc [ ("minimum", `Int 0); ("maximum", `Int 10_000) ]);
          ( "fixed_fee",
            `Assoc [ ("minimum", `String "0"); ("unit", `String "money") ] );
        ];
  }

let configuration_contract model =
  match name model with
  | "completed_bar_v1" -> completed_bar_v1_contract
  | unsupported ->
      invalid_arg
        (Printf.sprintf "execution model %S has no configuration contract"
           unsupported)

let supports_configuration model version =
  let contract = configuration_contract model in
  String.equal contract.version version
  || List.mem version contract.previous_versions

let required_fields model version =
  let contract = configuration_contract model in
  if String.equal version contract.version then Ok contract.required_fields
  else if List.mem version contract.previous_versions then
    Ok contract.legacy_required_fields
  else
    Error
      (Printf.sprintf
         "unsupported execution configuration version %S for model %S" version
         (name model))

let supports_contract model version =
  List.mem version (configuration_contract model).scenario_contract_versions

let strings values = `List (List.map (fun value -> `String value) values)

let capabilities_to_yojson () =
  `List
    (List.map
       (fun model ->
         let contract = configuration_contract model in
         `Assoc
           [
             ("name", `String (name model));
             ( "configuration_versions",
               strings (contract.version :: contract.previous_versions) );
             ( "scenario_contract_versions",
               strings contract.scenario_contract_versions );
             ("required_fields", strings contract.required_fields);
             ( "configuration_required_fields",
               `Assoc
                 [
                   (contract.version, strings contract.required_fields);
                   ("1", strings contract.legacy_required_fields);
                 ] );
             ("supported_order_types", strings contract.supported_order_types);
             ("data_requirements", strings contract.data_requirements);
             ("limits", contract.limits);
           ])
       builtins)

let find requested =
  match
    List.find_opt (fun model -> String.equal requested (name model)) builtins
  with
  | Some model -> Ok model
  | None -> Error (Printf.sprintf "unsupported execution model %S" requested)

let start_slice (module Model : S) = Model.start_slice
