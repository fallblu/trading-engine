module type CONTRACT = sig
  type state
  type reduction
  type market_slice
  type cursor
  type audit
  type context
  type event
  type intent

  module Validation : sig
    val run : state -> market_slice -> (unit, string) result
  end

  module Initialize : sig
    val run : state -> market_slice -> (reduction, string) result
  end

  module Actions : sig
    val run : market_slice -> reduction -> (reduction, string) result
  end

  module Borrow : sig
    val run : market_slice -> reduction -> (reduction, string) result
  end

  module Notifications : sig
    type request
    type outcome = Drained of reduction | Awaiting of request

    val run : reduction -> (outcome, string) result
    val has_pending : reduction -> bool
    val payload : request -> context * event
    val resume : request -> intent list -> reduction
  end

  module Matching : sig
    type outcome = Continue of reduction * cursor | Complete of reduction

    val start : market_slice -> reduction -> (cursor, string) result
    val run : market_slice -> cursor -> reduction -> (outcome, string) result
  end

  module Targets : sig
    val run : reduction -> (reduction, string) result
  end

  module Margin : sig
    val run : reduction -> (reduction, string) result
  end

  module Valuation : sig
    val run : reduction -> (state * audit list, string) result
  end
end

module Make (Contract : CONTRACT) = struct
  let ( let* ) result function_ =
    match result with Ok value -> function_ value | Error _ as error -> error

  type phase =
    | Matching of Contract.market_slice * Contract.cursor
    | Targets
    | Finish

  type progress =
    | Awaiting_strategy of Contract.Notifications.request * phase
    | Slice_completed of Contract.state * Contract.audit list

  let rec transition phase reduction =
    let* drained = Contract.Notifications.run reduction in
    match drained with
    | Contract.Notifications.Awaiting request ->
        Ok (Awaiting_strategy (request, phase))
    | Contract.Notifications.Drained reduction -> (
        match phase with
        | Matching (market_slice, cursor) -> (
            let* outcome =
              Contract.Matching.run market_slice cursor reduction
            in
            match outcome with
            | Contract.Matching.Continue (reduction, cursor) ->
                transition (Matching (market_slice, cursor)) reduction
            | Contract.Matching.Complete reduction ->
                transition Targets reduction)
        | Targets ->
            let* reduction = Contract.Targets.run reduction in
            transition Finish reduction
        | Finish ->
            let* reduction = Contract.Margin.run reduction in
            if Contract.Notifications.has_pending reduction then
              transition Finish reduction
            else
              let* state, audits = Contract.Valuation.run reduction in
              Ok (Slice_completed (state, audits)))

  let process_slice state market_slice =
    let* () = Contract.Validation.run state market_slice in
    let* reduction = Contract.Initialize.run state market_slice in
    let* reduction = Contract.Actions.run market_slice reduction in
    let* reduction = Contract.Borrow.run market_slice reduction in
    let* cursor = Contract.Matching.start market_slice reduction in
    transition (Matching (market_slice, cursor)) reduction

  let strategy_request = function
    | Awaiting_strategy (request, _) ->
        Some (Contract.Notifications.payload request)
    | Slice_completed _ -> None

  let resume progress intents =
    match progress with
    | Slice_completed _ ->
        Error "completed slice cannot accept strategy intents"
    | Awaiting_strategy (request, phase) ->
        transition phase (Contract.Notifications.resume request intents)

  let slice_result = function
    | Awaiting_strategy _ -> None
    | Slice_completed (state, audits) -> Some (state, audits)
end
