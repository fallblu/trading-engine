type phase_kind =
  | Premarket
  | Opening_auction
  | Regular
  | Closing_auction
  | Postmarket

type phase = { kind : phase_kind; opens_at : Ptime.t; closes_at : Ptime.t }
type session_kind = Regular_session | Early_close | Holiday

type session = {
  session_date : string;
  kind : session_kind;
  phases : phase list;
}

type t = {
  id : Id.Venue_calendar.t;
  version : string;
  venue_id : Id.Venue.t;
  instrument_ids : Id.Instrument.Set.t;
  sessions : session list;
}

let ( let* ) result function_ =
  match result with Ok value -> function_ value | Error _ as error -> error

let phase_kind_of_string = function
  | "premarket" -> Ok Premarket
  | "opening_auction" -> Ok Opening_auction
  | "regular" -> Ok Regular
  | "closing_auction" -> Ok Closing_auction
  | "postmarket" -> Ok Postmarket
  | value -> Error (Printf.sprintf "unsupported venue phase %S" value)

let phase_kind_to_string = function
  | Premarket -> "premarket"
  | Opening_auction -> "opening_auction"
  | Regular -> "regular"
  | Closing_auction -> "closing_auction"
  | Postmarket -> "postmarket"

let session_kind_of_string = function
  | "regular" -> Ok Regular_session
  | "early_close" -> Ok Early_close
  | "holiday" -> Ok Holiday
  | value -> Error (Printf.sprintf "unsupported session policy %S" value)

let session_kind_to_string = function
  | Regular_session -> "regular"
  | Early_close -> "early_close"
  | Holiday -> "holiday"

let phase_rank = function
  | Premarket -> 0
  | Opening_auction -> 1
  | Regular -> 2
  | Closing_auction -> 3
  | Postmarket -> 4

let create_phase ~kind ~opens_at ~closes_at =
  if Ptime.compare opens_at closes_at >= 0 then
    Error "venue phase opens_at must precede closes_at"
  else Ok { kind; opens_at; closes_at }

let valid_session_date value =
  String.length value = 10
  && value.[4] = '-'
  && value.[7] = '-'
  &&
  match Ptime.of_rfc3339 (value ^ "T00:00:00Z") with
  | Ok _ -> true
  | Error _ -> false

let validate_phases phases =
  let rec loop previous_kind previous_close seen_regular = function
    | [] ->
        if seen_regular then Ok ()
        else Error "open session must define a regular phase"
    | (phase : phase) :: remaining ->
        if
          Option.exists
            (fun kind -> phase_rank phase.kind <= phase_rank kind)
            previous_kind
        then Error "venue phases must be unique and in market order"
        else if
          Option.exists
            (fun closes_at -> Ptime.compare phase.opens_at closes_at < 0)
            previous_close
        then Error "venue phases must not overlap"
        else
          loop (Some phase.kind) (Some phase.closes_at)
            (seen_regular || phase.kind = Regular)
            remaining
  in
  loop None None false phases

let create_session ~session_date ~kind ~phases =
  if not (valid_session_date session_date) then
    Error "session_date must be a canonical YYYY-MM-DD date"
  else
    match kind with
    | Holiday ->
        if phases = [] then Ok { session_date; kind; phases }
        else Error "holiday session policy must not define phases"
    | Regular_session | Early_close ->
        let* () = validate_phases phases in
        Ok { session_date; kind; phases }

let create ~id ~version ~venue_id ~instrument_ids ~sessions =
  if not (String.equal version "1") then
    Error (Printf.sprintf "unsupported venue calendar version %S" version)
  else if instrument_ids = [] then
    Error "venue calendar must reference at least one instrument"
  else if sessions = [] then
    Error "venue calendar must define at least one session policy"
  else
    let instrument_set = Id.Instrument.Set.of_list instrument_ids in
    if Id.Instrument.Set.cardinal instrument_set <> List.length instrument_ids
    then Error "venue calendar instrument IDs must be unique"
    else
      let dates = List.map (fun session -> session.session_date) sessions in
      if List.sort_uniq String.compare dates <> dates then
        Error "venue calendar sessions must have unique, increasing dates"
      else
        Ok { id; version; venue_id; instrument_ids = instrument_set; sessions }

let session_on calendar ~session_date =
  match
    List.find_opt
      (fun session -> String.equal session.session_date session_date)
      calendar.sessions
  with
  | Some session -> Ok session
  | None ->
      Error
        (Printf.sprintf
           "venue calendar %s version %s has no explicit policy for %s"
           (Id.Venue_calendar.to_string calendar.id)
           calendar.version session_date)
