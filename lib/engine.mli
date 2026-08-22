(** Pure deterministic reducer for synchronized market slices. *)

type config

val config :
  contract_version:string ->
  risk:Risk.t ->
  execution_model:Execution_model.t ->
  execution:Execution.t ->
  max_internal_events:int ->
  (config, string) result

module Interactive : sig
  type t
  type progress

  val create :
    run_id:Id.Run.t ->
    scenario_sha256:string ->
    config:config ->
    initial_cash:(string * Scalar.Money.t) list ->
    (t, string) result

  val create_with_portfolio :
    run_id:Id.Run.t ->
    scenario_sha256:string ->
    config:config ->
    initial_portfolio:Initial_portfolio.t ->
    (t, string) result

  val account : t -> Account.t
  val oms : t -> Oms.t
  val latest_bar : t -> Id.Instrument.t -> Bar.t option
  val process_slice : t -> Market_slice.t -> (progress, string) result
  val strategy_request : progress -> (Strategy.context * Strategy.event) option
  val resume : progress -> Strategy.intent list -> (progress, string) result
  val slice_result : progress -> (t * Audit.t list) option
  val complete : t -> (t * Account.valuation * Audit.t list, string) result
end

module Make (Strategy_impl : Strategy.S) : sig
  type t

  val create :
    run_id:Id.Run.t ->
    scenario_sha256:string ->
    config:config ->
    initial_cash:(string * Scalar.Money.t) list ->
    strategy_state:Strategy_impl.state ->
    (t, string) result

  val create_with_portfolio :
    run_id:Id.Run.t ->
    scenario_sha256:string ->
    config:config ->
    initial_portfolio:Initial_portfolio.t ->
    strategy_state:Strategy_impl.state ->
    (t, string) result

  val account : t -> Account.t
  val oms : t -> Oms.t
  val latest_bar : t -> Id.Instrument.t -> Bar.t option
  val strategy_state : t -> Strategy_impl.state
  val with_strategy_state : t -> Strategy_impl.state -> t
  val process_slice : t -> Market_slice.t -> (t * Audit.t list, string) result
  val complete : t -> (t * Account.valuation * Audit.t list, string) result
end
