(** Immutable venue-local session policies resolved outside the reducer. *)

type phase_kind =
  | Premarket
  | Opening_auction
  | Regular
  | Closing_auction
  | Postmarket

type phase = private {
  kind : phase_kind;
  opens_at : Ptime.t;
  closes_at : Ptime.t;
}

type session_kind = Regular_session | Early_close | Holiday

type session = private {
  session_date : string;
  kind : session_kind;
  phases : phase list;
}

type t = private {
  id : Id.Venue_calendar.t;
  version : string;
  venue_id : Id.Venue.t;
  instrument_ids : Id.Instrument.Set.t;
  sessions : session list;
}

val phase_kind_of_string : string -> (phase_kind, string) result
val phase_kind_to_string : phase_kind -> string
val session_kind_of_string : string -> (session_kind, string) result
val session_kind_to_string : session_kind -> string

val create_phase :
  kind:phase_kind ->
  opens_at:Ptime.t ->
  closes_at:Ptime.t ->
  (phase, string) result

val create_session :
  session_date:string ->
  kind:session_kind ->
  phases:phase list ->
  (session, string) result

val create :
  id:Id.Venue_calendar.t ->
  version:string ->
  venue_id:Id.Venue.t ->
  instrument_ids:Id.Instrument.t list ->
  sessions:session list ->
  (t, string) result

val session_on : t -> session_date:string -> (session, string) result
(** Return the explicit policy for [session_date]. Missing dates are errors and
    are never inferred from weekdays, holidays, or neighboring sessions. *)
