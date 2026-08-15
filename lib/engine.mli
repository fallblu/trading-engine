(** Pure orchestration of strategy, risk, OMS, execution, and accounting. *)

type config = private {
  risk : Risk.t;
  execution : Execution.t;
  max_internal_events : int;
}

val config :
  risk:Risk.t ->
  execution:Execution.t ->
  max_internal_events:int ->
  (config, string) result

module Make (Strategy_impl : Strategy.S) : sig
  type t

  val create :
    run_id:Id.Run.t ->
    config:config ->
    initial_cash:Scalar.Money.t ->
    strategy_state:Strategy_impl.state ->
    t

  val process_bar : t -> Bar.t -> (t * Audit.t list, string) result
  val complete : t -> (t * Account.valuation * Audit.t list, string) result
  val account : t -> Account.t
  val oms : t -> Oms.t
  val latest_bar : t -> Id.Instrument.t -> Bar.t option
  val strategy_state : t -> Strategy_impl.state
end
