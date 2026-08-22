(** Pure reducer-phase sequencing behind one transition contract.

    The contract keeps domain state opaque. Every phase receives an immutable
    reduction and returns either a replacement reduction or a completed slice;
    only this module decides which phase runs next. *)

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
    (** Read-only ingress check. Success guarantees that later phases receive a
        new, ordered, catalog-complete slice. *)
  end

  module Initialize : sig
    val run : state -> market_slice -> (reduction, string) result
    (** Captures the slice snapshot and creates its reduction. The result owns
        one received-slice audit and an empty feedback queue. *)
  end

  module Actions : sig
    val run : market_slice -> reduction -> (reduction, string) result
    (** Applies the complete corporate-action batch in source order before any
        borrow accrual or matching. *)
  end

  module Borrow : sig
    val run : market_slice -> reduction -> (reduction, string) result
    (** Accrues deterministic short-borrow fees against the action-adjusted
        account before matching. *)
  end

  module Notifications : sig
    type request
    type outcome = Drained of reduction | Awaiting of request

    val run : reduction -> (outcome, string) result
    (** Drains accepted intents until empty or until a strategy callback must
        suspend the transition. No later phase runs with pending feedback. *)

    val has_pending : reduction -> bool
    val payload : request -> context * event
    val resume : request -> intent list -> reduction
  end

  module Matching : sig
    type outcome = Continue of reduction * cursor | Complete of reduction

    val start : market_slice -> reduction -> (cursor, string) result
    (** Fixes the eligible-order cursor after actions and borrow accrual. *)

    val run : market_slice -> cursor -> reduction -> (outcome, string) result
    (** Executes at most one cursor step. [Continue] retains the same slice;
        [Complete] has cancelled market remainders and queued the close
        callback. *)
  end

  module Targets : sig
    val run : reduction -> (reduction, string) result
    (** Reconciles persistent targets exactly once after the close callback. *)
  end

  module Margin : sig
    val run : reduction -> (reduction, string) result
    (** Assesses the post-target account. Generated order notifications are
        drained before this phase is reassessed. *)
  end

  module Valuation : sig
    val run : reduction -> (state * audit list, string) result
    (** Terminates the slice with exactly one valuation and returns audits in
        publication order. *)
  end
end

module Make (Contract : CONTRACT) : sig
  type progress

  val process_slice :
    Contract.state -> Contract.market_slice -> (progress, string) result

  val strategy_request : progress -> (Contract.context * Contract.event) option
  val resume : progress -> Contract.intent list -> (progress, string) result
  val slice_result : progress -> (Contract.state * Contract.audit list) option
end
