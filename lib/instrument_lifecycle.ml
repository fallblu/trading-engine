type terminal_policy =
  | Hold
  | Cash_out of { price : Scalar.Price.t; currency : string }

type kind =
  | Halt of { reason : string }
  | Resume
  | Identifier_change of {
      symbol : string;
      provider : string;
      provider_instrument_id : string;
    }
  | Expiration of { terminal_policy : terminal_policy }
  | Delisting of { terminal_policy : terminal_policy; reason : string }

type event = {
  id : Id.Corporate_action.t;
  instrument_id : Id.Instrument.t;
  kind : kind;
}

type status = Tradable | Halted | Expired | Delisted

type listing = {
  instrument_id : Id.Instrument.t;
  symbol : string;
  provider_mappings : (string * string) list;
  status : status;
}

type t = listing Id.Instrument.Map.t

let valid_label value =
  String.length value > 0
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x21 && code <> 0x7f)
       value

let validate_terminal_policy = function
  | Hold -> Ok ()
  | Cash_out { currency; _ } ->
      if valid_label currency then Ok ()
      else
        Error
          "terminal cash-out currency must not be empty or contain whitespace"

let create_event ~id ~instrument_id ~kind =
  match kind with
  | (Halt { reason } | Delisting { reason; _ }) when not (valid_label reason) ->
      Error "lifecycle reason must not be empty or contain whitespace"
  | Identifier_change { symbol; provider; provider_instrument_id }
    when not
           (valid_label symbol && valid_label provider
           && valid_label provider_instrument_id) ->
      Error "identifier-change values must not be empty or contain whitespace"
  | Expiration { terminal_policy } | Delisting { terminal_policy; _ } ->
      Result.map
        (fun () -> { id; instrument_id; kind })
        (validate_terminal_policy terminal_policy)
  | Halt _ | Resume | Identifier_change _ -> Ok { id; instrument_id; kind }

let compare_event left right = Id.Corporate_action.compare left.id right.id

let create instruments =
  List.fold_left
    (fun result (instrument : Instrument.t) ->
      Result.bind result (fun state ->
          if Id.Instrument.Map.mem instrument.id state then
            Error "lifecycle catalog instrument IDs must be unique"
          else
            Ok
              (Id.Instrument.Map.add instrument.id
                 {
                   instrument_id = instrument.id;
                   symbol = instrument.symbol;
                   provider_mappings = [];
                   status = Tradable;
                 }
                 state)))
    (Ok Id.Instrument.Map.empty) instruments

let listing state instrument_id = Id.Instrument.Map.find_opt instrument_id state

let is_tradable state instrument_id =
  match listing state instrument_id with
  | Some { status = Tradable; _ } -> true
  | Some _ | None -> false

let apply state (event : event) =
  match listing state event.instrument_id with
  | None -> Error "lifecycle event refers to an unknown instrument"
  | Some current ->
      let result =
        match (current.status, event.kind) with
        | (Expired | Delisted), _ ->
            Error "terminal instrument cannot accept another lifecycle event"
        | Tradable, Halt _ -> Ok { current with status = Halted }
        | Halted, Resume -> Ok { current with status = Tradable }
        | Halted, Halt _ -> Error "halted instrument cannot be halted again"
        | Tradable, Resume -> Error "tradable instrument cannot be resumed"
        | (Tradable | Halted), Identifier_change change ->
            let mappings =
              (change.provider, change.provider_instrument_id)
              :: List.remove_assoc change.provider current.provider_mappings
              |> List.sort (fun (left, _) (right, _) ->
                  String.compare left right)
            in
            Ok
              {
                current with
                symbol = change.symbol;
                provider_mappings = mappings;
              }
        | (Tradable | Halted), Expiration _ ->
            Ok { current with status = Expired }
        | (Tradable | Halted), Delisting _ ->
            Ok { current with status = Delisted }
      in
      Result.map
        (fun updated -> Id.Instrument.Map.add event.instrument_id updated state)
        result

let status_to_string = function
  | Tradable -> "tradable"
  | Halted -> "halted"
  | Expired -> "expired"
  | Delisted -> "delisted"

let kind_to_string = function
  | Halt _ -> "halt"
  | Resume -> "resume"
  | Identifier_change _ -> "identifier_change"
  | Expiration _ -> "expiration"
  | Delisting _ -> "delisting"
