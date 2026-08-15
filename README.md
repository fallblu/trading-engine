# Trading Engine

Trading Engine is a deterministic, event-driven OCaml execution-engine prototype. It runs a
typed strategy through pre-trade risk, order management, completed-bar execution, exact cash and
position accounting, valuation, and an append-only audit journal.

The prototype is replay-first. Its pure kernel and explicit source, strategy, execution, and
journal layers establish the boundaries needed for later paper and live adapters without putting
networking or wall-clock state inside the reducer.

```text
scenario bars and scheduled intents
                │
                ▼
      deterministic engine reducer
                │
     ┌──────────┼──────────┐
     ▼          ▼          ▼
 strategy  risk + OMS  bar simulator
     │          │          │
     └──────────┴──── fills┘
                │
                ▼
      accounting + valuation
                │
                ▼
        JSON Lines journal
```

## Implemented scope

- OCaml 5.5 and Dune 3.24 with a repository-local opam switch
- Opaque IDs and checked fixed-point prices, money, and quantities
- Completed OHLCV bars with source, availability, receipt, and engine ordering
- Pure strategy callbacks with causal, immutable context snapshots
- Direct market and limit orders
- Target-position planning with target-order replacement
- Long-only position and outstanding-sell risk
- Lot-size and tick-size validation
- Deterministic FIFO volume participation
- Partial fills, persistent GTC limits, and one-bar IOC markets
- Fixed and notional fees with explicit rounding
- Average-cost accounting, realized and unrealized P&L, and equity reconciliation
- Idempotent fills with conflicting duplicate detection
- Strict versioned scenario parsing and stable audit JSON
- Unit, scenario, golden-contract, and property-based tests

## Quick start

The project uses a local switch and does not modify the existing default switch:

```sh
cd ~/trading-engine
opam switch set .
opam install . --deps-only --with-test --locked
make check
```

The committed `trading_engine.opam.locked` captures the verified dependency set. Omit `--locked`
only when intentionally resolving a newer compatible set.

Run the included replay:

```sh
opam exec -- dune exec trading-engine -- \
  --input examples/demo.json \
  --journal demo.journal.jsonl
```

The journal path must not already exist. A successful run prints the order counts and reconciled
final valuation.

## Execution rules

An order emitted after bar sequence `n` cannot execute on bar `n`. It first becomes eligible on a
later bar.

- A market order fills at the next eligible bar's open. Volume can create a partial fill. Any
  remainder is cancelled after that bar.
- A buy limit fills at the better opening price when the open is at or below the limit. Otherwise,
  it fills at the limit when the low touches it.
- A sell limit uses the symmetric open/high rule.
- Limit remainders remain active.
- Eligible orders share volume capacity in accepted-order FIFO order.
- Participation capacity and fills round down to complete instrument lots.
- Executable OHLC prices must align with the instrument tick size.
- Every partial fill incurs the configured fixed and notional fees.

Completed bars do not reveal queue position or intrabar path. A touched limit fill is therefore an
explicit optimistic approximation, not a claim about exchange execution.

Read [Execution model](docs/execution-model.md) for the complete phase and accounting rules.

## Project boundaries

This version intentionally omits:

- Broker and streaming-market-data connectors
- External execution-report ingestion
- Exchange calendars and time-zone databases
- Cash buying-power, margin, leverage, and borrow models
- Short positions
- Multiple currencies and FX conversion
- Splits, dividends, and other corporate actions
- Durable snapshots and broker reconciliation
- Crash-safe or exactly-once journal guarantees
- Tick, trade, and order-book replay

Cash may become negative after a buy or market gap because this prototype has no buying-power or
margin policy. Positions cannot become negative. Add a broker-specific cash or margin model before
using the engine to authorize real orders.

The JSON Lines writer uses exclusive creation, appends in engine order, and flushes every event. It
does not call `fsync`, so it is an audit artifact rather than a production recovery log.

## Architecture and contracts

- [Architecture](docs/architecture.md)
- [Scenario version 1](docs/scenario-v1.md)
- [Execution model](docs/execution-model.md)
- [Persistra integration](docs/persistra.md)
- [Contributing](CONTRIBUTING.md)
