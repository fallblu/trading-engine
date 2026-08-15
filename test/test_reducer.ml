open Test_support
module T = Trading_engine
module Runner = T.Engine.Make (T.Scripted_strategy)

let runner ?(execution = execution ()) schedule =
  let strategy_state = T.Scripted_strategy.create schedule |> ok in
  let config = engine_config ~execution () in
  Runner.create ~run_id:(run_id "test-run") ~config
    ~initial_cash:(money "10000") ~strategy_state

let event_names events =
  List.map (fun event -> T.Audit.event_name event.T.Audit.event) events

let target quantity_value =
  T.Strategy.Target_position
    {
      instrument_id = instrument_id "test-equity";
      quantity = quantity quantity_value;
    }

let market_order_is_next_bar_ioc () =
  let state =
    runner
      ~execution:(execution ~participation_bps:5000 ())
      [ (1L, [ target "10" ]) ]
  in
  let state, first_events = Runner.process_bar state (bar 1L) |> ok in
  Alcotest.check quantity_testable "no same-bar position" T.Scalar.Quantity.zero
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check (list string))
    "first audit order"
    [ "bar_received"; "target_requested"; "order_accepted"; "valuation" ]
    (event_names first_events);
  let accepted = List.hd (T.Oms.active_orders (Runner.oms state)) in
  Alcotest.(check int64)
    "eligible after first bar" 1L accepted.eligible_after_bar_sequence;
  let state, second_events =
    Runner.process_bar state
      (bar ~open_price:"103" ~close_price:"107" ~volume:(Some "12") 2L)
    |> ok
  in
  Alcotest.check quantity_testable "six shares filled" (quantity "6")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check (list string))
    "fill then IOC cancellation"
    [ "bar_received"; "fill_applied"; "order_cancelled"; "valuation" ]
    (event_names second_events);
  let final_order = T.Oms.find (Runner.oms state) accepted.id |> Option.get in
  Alcotest.(check string)
    "partial market remainder cancelled" "cancelled"
    (T.Order.status_to_string final_order.status)

let partial_limit_persists () =
  let limit_request =
    request ~quantity_value:"10" ~kind:(T.Order.Limit (price "100")) ()
  in
  let state =
    runner
      ~execution:(execution ~participation_bps:10_000 ())
      [ (1L, [ T.Strategy.Submit_order limit_request ]) ]
  in
  let state, _ = Runner.process_bar state (bar 1L) |> ok in
  let state, _ =
    Runner.process_bar state
      (bar ~open_price:"105" ~low_price:"99" ~close_price:"101"
         ~volume:(Some "4") 2L)
    |> ok
  in
  let partial = List.hd (T.Oms.active_orders (Runner.oms state)) in
  Alcotest.check quantity_testable "four filled" (quantity "4")
    partial.filled_quantity;
  Alcotest.(check string)
    "limit remains active" "partially_filled"
    (T.Order.status_to_string partial.status);
  let state, _ =
    Runner.process_bar state
      (bar ~open_price:"99" ~low_price:"95" ~close_price:"102"
         ~volume:(Some "10") 3L)
    |> ok
  in
  Alcotest.check quantity_testable "ten total" (quantity "10")
    (T.Account.position_quantity (Runner.account state)
       (instrument_id "test-equity"));
  Alcotest.(check int)
    "no active remainder" 0
    (List.length (T.Oms.active_orders (Runner.oms state)))

let repeated_target_replaces_prior_planner_order () =
  let state = runner [ (1L, [ target "10"; target "5" ]) ] in
  let state, events = Runner.process_bar state (bar 1L) |> ok in
  Alcotest.(check (list string))
    "replacement audit"
    [
      "bar_received";
      "target_requested";
      "order_accepted";
      "target_requested";
      "order_cancelled";
      "order_accepted";
      "valuation";
    ]
    (event_names events);
  match T.Oms.orders (Runner.oms state) with
  | [ first; second ] ->
      Alcotest.(check string)
        "first cancelled" "cancelled"
        (T.Order.status_to_string first.status);
      Alcotest.check quantity_testable "replacement quantity" (quantity "5")
        second.request.quantity
  | _ -> Alcotest.fail "expected two target orders"

let target_conflicts_with_direct_order () =
  let direct =
    request ~quantity_value:"3" ~kind:(T.Order.Limit (price "90")) ()
  in
  let state = runner [ (1L, [ T.Strategy.Submit_order direct; target "5" ]) ] in
  let state, events = Runner.process_bar state (bar 1L) |> ok in
  Alcotest.(check bool)
    "target rejection audited" true
    (List.mem "intent_rejected" (event_names events));
  let active = T.Oms.active_orders (Runner.oms state) in
  Alcotest.(check int) "direct order remains" 1 (List.length active);
  Alcotest.(check string)
    "direct origin" "direct"
    (T.Order.origin_to_string (List.hd active).request.origin)

let unknown_zero_target_is_rejected () =
  let unknown_target =
    T.Strategy.Target_position
      {
        instrument_id = instrument_id "unknown-equity";
        quantity = quantity "0";
      }
  in
  let state = runner [ (1L, [ unknown_target ]) ] in
  let state, events = Runner.process_bar state (bar 1L) |> ok in
  Alcotest.(check (list string))
    "unknown no-op target is not silently accepted"
    [ "bar_received"; "target_requested"; "intent_rejected"; "valuation" ]
    (event_names events);
  Alcotest.(check int)
    "no order created" 0
    (List.length (T.Oms.orders (Runner.oms state)))

let external_ordering_is_validated () =
  let state = runner [] in
  let state, _ = Runner.process_bar state (bar 2L) |> ok in
  Alcotest.(check bool)
    "source sequence cannot repeat" true
    (Result.is_error (Runner.process_bar state (bar 2L)))

module Looping_strategy = struct
  type state = T.Order.request

  let name = "looping"

  let on_event request _context = function
    | T.Strategy.Bar_closed _ -> (request, [ T.Strategy.Submit_order request ])
    | T.Strategy.Order_updated _ ->
        (request, [ T.Strategy.Submit_order request ])
    | T.Strategy.Fill_received _ | T.Strategy.Intent_rejected _ -> (request, [])
end

module Looping_runner = T.Engine.Make (Looping_strategy)

let internal_feedback_is_capped () =
  let strategy_state = request ~quantity_value:"1" () in
  let config = engine_config ~max_internal_events:3 () in
  let state =
    Looping_runner.create ~run_id:(run_id "loop") ~config
      ~initial_cash:(money "10000") ~strategy_state
  in
  Alcotest.(check bool)
    "feedback loop rejected" true
    (Result.is_error (Looping_runner.process_bar state (bar 1L)))

let exact_internal_event_limit_succeeds () =
  let strategy_state = T.Scripted_strategy.create [] |> ok in
  let config = engine_config ~max_internal_events:1 () in
  let state =
    Runner.create ~run_id:(run_id "one-event") ~config
      ~initial_cash:(money "10000") ~strategy_state
  in
  Alcotest.(check bool)
    "one callback fits a limit of one" true
    (Result.is_ok (Runner.process_bar state (bar 1L)))

module Context_strategy = struct
  type state = {
    requests : T.Order.request list;
    observations : (int64 * int) list;
  }

  let name = "context-observer"

  let on_event state context = function
    | T.Strategy.Bar_closed bar when Int64.equal bar.T.Bar.source_sequence 1L ->
        ( state,
          List.map
            (fun request -> T.Strategy.Submit_order request)
            state.requests )
    | T.Strategy.Fill_received _ ->
        let position =
          T.Strategy.position context (instrument_id "test-equity")
          |> T.Scalar.Quantity.to_int64
        in
        let active = List.length (T.Strategy.working_orders context) in
        ( {
            state with
            observations = state.observations @ [ (position, active) ];
          },
          [] )
    | T.Strategy.Bar_closed _ | T.Strategy.Order_updated _
    | T.Strategy.Intent_rejected _ ->
        (state, [])
end

module Context_runner = T.Engine.Make (Context_strategy)

let notification_context_is_causal () =
  let first = request ~quantity_value:"2" () in
  let second = request ~quantity_value:"2" () in
  let strategy_state =
    Context_strategy.{ requests = [ first; second ]; observations = [] }
  in
  let state =
    Context_runner.create ~run_id:(run_id "contexts") ~config:(engine_config ())
      ~initial_cash:(money "10000") ~strategy_state
  in
  let state, _ = Context_runner.process_bar state (bar 1L) |> ok in
  let state, _ =
    Context_runner.process_bar state (bar ~volume:(Some "10") 2L) |> ok
  in
  let observations = (Context_runner.strategy_state state).observations in
  Alcotest.(check (list (pair int64 int)))
    "each fill sees its own post-event snapshot"
    [ (2L, 1); (4L, 0) ]
    observations

let tests =
  [
    Alcotest.test_case "market order is next-bar IOC" `Quick
      market_order_is_next_bar_ioc;
    Alcotest.test_case "partial limit persists" `Quick partial_limit_persists;
    Alcotest.test_case "target replacement" `Quick
      repeated_target_replaces_prior_planner_order;
    Alcotest.test_case "target/direct conflict" `Quick
      target_conflicts_with_direct_order;
    Alcotest.test_case "unknown zero target rejected" `Quick
      unknown_zero_target_is_rejected;
    Alcotest.test_case "external ordering validation" `Quick
      external_ordering_is_validated;
    Alcotest.test_case "internal feedback cap" `Quick
      internal_feedback_is_capped;
    Alcotest.test_case "exact internal event limit" `Quick
      exact_internal_event_limit_succeeds;
    Alcotest.test_case "causal notification contexts" `Quick
      notification_context_is_causal;
  ]
