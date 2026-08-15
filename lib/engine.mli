(** Pure deterministic reducer for synchronized market slices. *)

type config

val config :
  risk:Risk.t ->
  execution_model:Execution_model.t ->
  execution:Execution.t ->
  max_internal_events:int ->
  (config, string) result

module Make (Strategy_impl : Strategy.S) : sig
  type t

  val create :
    run_id:Id.Run.t ->
    scenario_sha256:string ->
    config:config ->
    initial_cash:(string * Scalar.Money.t) list ->
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
