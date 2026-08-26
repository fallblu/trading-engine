open Test_support
module T = Trading_engine

let phase kind opens_at closes_at =
  T.Venue_calendar.create_phase ~kind ~opens_at:(timestamp opens_at)
    ~closes_at:(timestamp closes_at)
  |> ok

let explicit_session_policies () =
  let regular =
    T.Venue_calendar.create_session ~session_date:"2026-01-02"
      ~kind:T.Venue_calendar.Regular_session
      ~phases:
        [
          phase T.Venue_calendar.Premarket "2026-01-02T09:00:00Z"
            "2026-01-02T14:25:00Z";
          phase T.Venue_calendar.Opening_auction "2026-01-02T14:25:00Z"
            "2026-01-02T14:30:00Z";
          phase T.Venue_calendar.Regular "2026-01-02T14:30:00Z"
            "2026-01-02T20:55:00Z";
          phase T.Venue_calendar.Closing_auction "2026-01-02T20:55:00Z"
            "2026-01-02T21:00:00Z";
          phase T.Venue_calendar.Postmarket "2026-01-02T21:00:00Z"
            "2026-01-03T01:00:00Z";
        ]
    |> ok
  in
  let holiday =
    T.Venue_calendar.create_session ~session_date:"2026-01-03"
      ~kind:T.Venue_calendar.Holiday ~phases:[]
    |> ok
  in
  let early_close =
    T.Venue_calendar.create_session ~session_date:"2026-01-05"
      ~kind:T.Venue_calendar.Early_close
      ~phases:
        [
          phase T.Venue_calendar.Regular "2026-01-05T14:30:00Z"
            "2026-01-05T18:00:00Z";
        ]
    |> ok
  in
  let calendar =
    T.Venue_calendar.create
      ~id:(T.Id.Venue_calendar.of_string_exn "xnas-2026")
      ~version:"1"
      ~venue_id:(T.Id.Venue.of_string_exn "XNAS")
      ~instrument_ids:[ instrument_id "demo-equity-acme" ]
      ~sessions:[ regular; holiday; early_close ]
    |> ok
  in
  let selected =
    T.Venue_calendar.session_on calendar ~session_date:"2026-01-05" |> ok
  in
  Alcotest.(check string)
    "early-close policy" "early_close"
    (T.Venue_calendar.session_kind_to_string selected.kind);
  let missing =
    T.Venue_calendar.session_on calendar ~session_date:"2026-01-04" |> error
  in
  Alcotest.(check string)
    "missing policy is not inferred"
    "venue calendar xnas-2026 version 1 has no explicit policy for 2026-01-04"
    missing

let ambiguous_phase_policies_are_rejected () =
  let regular =
    phase T.Venue_calendar.Regular "2026-01-02T14:30:00Z" "2026-01-02T21:00:00Z"
  in
  let overlapping =
    phase T.Venue_calendar.Postmarket "2026-01-02T20:00:00Z"
      "2026-01-03T01:00:00Z"
  in
  Alcotest.(check bool)
    "overlap rejected" true
    (Result.is_error
       (T.Venue_calendar.create_session ~session_date:"2026-01-02"
          ~kind:T.Venue_calendar.Regular_session
          ~phases:[ regular; overlapping ]));
  Alcotest.(check bool)
    "missing regular phase rejected" true
    (Result.is_error
       (T.Venue_calendar.create_session ~session_date:"2026-01-02"
          ~kind:T.Venue_calendar.Regular_session
          ~phases:
            [
              phase T.Venue_calendar.Premarket "2026-01-02T09:00:00Z"
                "2026-01-02T14:00:00Z";
            ]));
  Alcotest.(check bool)
    "holiday phases rejected" true
    (Result.is_error
       (T.Venue_calendar.create_session ~session_date:"2026-01-02"
          ~kind:T.Venue_calendar.Holiday ~phases:[ regular ]))

let scenario_contract_requires_calendar_coverage () =
  let document =
    Yojson.Safe.from_file "../contracts/v1/fixtures/demo.scenario.json"
  in
  let scenario = T.Scenario.of_yojson document |> ok in
  Alcotest.(check int)
    "calendar retained" 1
    (List.length scenario.venue_calendars);
  let missing =
    match document with
    | `Assoc fields ->
        `Assoc
          (List.filter
             (fun (name, _) -> not (String.equal name "venue_calendars"))
             fields)
    | json -> json
  in
  let missing = T.Scenario.of_yojson missing |> error in
  Alcotest.(check (option string))
    "missing calendar path" (Some "$") missing.context.json_path;
  let wrong_member =
    match document with
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (name, value) ->
               if not (String.equal name "venue_calendars") then (name, value)
               else
                 let calendars =
                   match value with
                   | `List [ `Assoc calendar ] ->
                       `List
                         [
                           `Assoc
                             (List.map
                                (fun (field, value) ->
                                  if String.equal field "instrument_ids" then
                                    (field, `List [ `String "unknown" ])
                                  else (field, value))
                                calendar);
                         ]
                   | _ -> Alcotest.fail "fixture calendar shape changed"
                 in
                 (name, calendars))
             fields)
    | _ -> Alcotest.fail "fixture scenario must be an object"
  in
  let uncovered = T.Scenario.of_yojson wrong_member |> error in
  Alcotest.(check (option string))
    "coverage path" (Some "$.venue_calendars") uncovered.context.json_path

let tests =
  [
    Alcotest.test_case "explicit policies and missing dates" `Quick
      explicit_session_policies;
    Alcotest.test_case "ambiguous phases rejected" `Quick
      ambiguous_phase_policies_are_rejected;
    Alcotest.test_case "scenario calendar coverage" `Quick
      scenario_contract_requires_calendar_coverage;
  ]
