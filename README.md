# Trading Engine

Trading Engine is a deterministic, event-driven OCaml execution engine. It runs a typed strategy
through pre-trade risk, order management, synchronized completed-bar execution, exact accounting,
valuation, and a hash-bound JSON Lines audit journal.

The engine is replay-first. Its pure kernel and explicit source, strategy, execution, and journal
layers keep networking, files, and wall-clock state outside the reducer.

```text
scenario slices and scheduled intents
                  │
                  ▼
        deterministic reducer
                  │
       ┌──────────┼──────────┐
       ▼          ▼          ▼
   strategy  risk + OMS  slice simulator
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
- Opaque IDs and canonical checked fixed-point prices, weights, money, and quantities
- Synchronized market slices with one bar per configured instrument
- Separate market event, availability, receipt, slice, and engine ordering
- Pure strategy callbacks with causal, immutable context snapshots
- Portfolio weight and quantity targets covering the complete instrument catalog
- Current-equity weight sizing at synchronized closing marks with lot rounding
- Persistent target reconciliation through bounded market-order attempts
- Direct market and limit orders, cancellations, and metrics
- Long-only position, outstanding-sell, lot, tick, and size risk
- Deterministic sell-first matching, then FIFO within each side
- Shared per-instrument volume participation, partial fills, and GTC limits
- One-slice IOC market orders
- Cash buying power with whole-lot clipping and structured `cash_limited` records
- Fixed and notional fees with explicit rounding
- Average-cost accounting, realized and unrealized P&L, and equity reconciliation
- Strict scenario parsing, JSON Schema artifacts, and stable audit JSON
- Scenario SHA-256 binding in `run_started` and `run_completed`
- Exclusive partial journal creation and atomic no-replace finalization
- Unit, schema-conformance, scenario, golden-contract, and property tests

## Quick start

The project uses a local switch and does not modify the default switch. The complete check also
uses Python's `jsonschema` package to validate the committed scenario and journal fixtures.

```sh
cd ~/trading-engine
opam switch set .
opam install . --deps-only --with-test --locked
make check
```

Validate the included scenario with an in-memory replay:

```sh
opam exec -- dune exec trading-engine -- \
  --input contracts/v1/fixtures/demo.scenario.json \
  --validate-only
```

Run it and create a journal:

```sh
opam exec -- dune exec trading-engine -- \
  --input contracts/v1/fixtures/demo.scenario.json \
  --journal demo.journal.jsonl
```

Discover the executable version and machine-readable compatibility surface:

```sh
opam exec -- dune exec trading-engine -- --version
opam exec -- dune exec trading-engine -- --capabilities
```

Clients must confirm that both `scenario_contract_versions` and `journal_contract_versions`
contain the scenario's `contract_version` before starting a replay.

The final and `.partial` journal paths must not already exist. The CLI reads the scenario once,
hashes those exact bytes, parses the same bytes, and binds the hash into the journal. It writes to
the partial path and publishes the requested path only after `run_completed` is fully written and
the partial file is closed. An error preserves the partial artifact for diagnosis.

## Execution summary

An order emitted after slice `n` cannot execute on slice `n`. It first becomes eligible on a later
slice whose start is not earlier than its creation time.

- Market orders attempt the next eligible open and cancel any remainder after that slice.
- Persistent portfolio targets submit a new bounded attempt after each miss until reached or
  superseded.
- Limit orders use deterministic gap improvement and optimistic intrabar touch rules.
- Eligible sells consume each instrument's participation capacity before buys. FIFO creation
  order breaks ties within a side.
- Sell fills across the slice update cash before any buy affordability check.
- A buy proposal is clipped to the largest affordable whole-lot quantity at its actual fill price,
  including fees. Only the applied quantity consumes slice capacity, leaving clipped capacity for
  later eligible same-instrument buys. Cash never becomes negative.
- The engine emits exactly one valuation after each complete synchronized slice.

Read [Execution model](docs/execution-model.md) for the full phase, price, fee, cash, and accounting
rules.

## Project boundaries

The current scope omits:

- Broker and streaming-market-data connectors
- External execution-report ingestion
- Exchange calendars and time-zone databases
- Margin, leverage, borrow, and short-position models
- Multiple currencies and FX conversion
- Splits, dividends, and other corporate actions
- Durable reducer snapshots and broker reconciliation
- `fsync` and restart recovery for journals
- Tick, trade, and order-book replay

The journal writer flushes each record, creates its partial file exclusively, and finalizes with an
exclusive hard link. It does not call `fsync`, so the journal is an audit artifact rather than a
production recovery log.

## Architecture and contracts

- [Architecture](docs/architecture.md)
- [Scenario contract](docs/scenario.md)
- [Contract v1 and conformance fixtures](contracts/v1/README.md)
- [Scenario JSON Schema](contracts/v1/scenario.schema.json)
- [Journal record JSON Schema](contracts/v1/journal.schema.json)
- [Execution model](docs/execution-model.md)
- [Persistra integration](docs/persistra.md)
- [Contributing](CONTRIBUTING.md)
