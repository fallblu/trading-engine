# Trading Engine

Trading Engine is a deterministic, event-driven OCaml execution engine. It runs a typed strategy
through pre-trade risk, order management, synchronized completed-bar execution, exact accounting,
valuation, and a hash-bound JSON Lines audit journal.

The engine is replay-first. Its pure kernel and explicit source, strategy, execution, and journal
layers keep networking, files, and wall-clock state outside the reducer.

```text
scenario slices and scheduled or external intents
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
- Pure suspend/resume strategy requests with equivalent scripted and external reducers
- Portfolio weight and quantity targets covering the complete instrument catalog
- Current-equity weight sizing at synchronized closing marks with lot rounding
- Persistent target reconciliation through bounded market-order attempts
- Direct market and limit orders, cancellations, and metrics
- Signed long/short position, order, lot, tick, gross-exposure, leverage, and margin risk
- Deterministic liquidation-first matching, then sell-before-buy and FIFO priority
- Shared per-instrument volume participation, partial fills, and GTC limits
- One-slice IOC market orders
- Risk-aware fractional-lot clipping with structured `margin_limited` records
- Fixed and notional fees with explicit rounding
- Explicit multi-currency cash ledgers and complete per-slice FX marks in a base currency
- Split and cash-dividend processing before matching, including target and order adjustment
- Short borrow accrual, maintenance-margin calls, and deterministic liquidation orders
- Signed average-cost accounting, realized and unrealized P&L, and equity reconciliation
- Per-currency cash and per-instrument quantity, mark, value, basis, P&L, and fee attribution
- Deterministic event IDs, ordered causal references, and order-creation attribution
- Contract-selected compiled execution modules; v3 currently exposes `completed_bar_v1`
- Strict batch JSON and bounded-memory JSON Lines scenario parsing with JSON Schemas
- Versioned synchronous JSON Lines strategy processes with per-request timeouts and strict
  lifecycle supervision
- Complete bidirectional strategy transcripts with exclusive partial and no-replace publication
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
  --input contracts/v3/fixtures/demo.scenario.json \
  --validate-only
```

Run it and create a journal:

```sh
opam exec -- dune exec trading-engine -- \
  --input contracts/v3/fixtures/demo.scenario.json \
  --journal demo.journal.jsonl
```

For larger histories, validate and replay the equivalent stream one slice at a time:

```sh
opam exec -- dune exec trading-engine -- \
  --input contracts/v3/fixtures/demo.scenario.jsonl \
  --input-format jsonl \
  --journal demo.journal.jsonl
```

Run an external strategy against an empty-schedule scenario:

```sh
opam exec -- dune exec trading-engine -- \
  --input contracts/strategy/v1/fixtures/external.scenario.json \
  --journal external.journal.jsonl \
  --strategy-executable ./my-strategy \
  --strategy-arg=config.toml \
  --strategy-timeout 30 \
  --strategy-transcript external.strategy.jsonl
```

The engine launches the program directly without a shell. This supervises the child but does not
sandbox it; run strategy code with the same trust you give the invoking user. Protocol messages
own the child's standard input and output; strategy diagnostics belong on standard error. Only
one request is outstanding. Initialization must return `ready`, each event must return `intents`,
and shutdown must return `stopped`. Wrong versions or sequences, unknown or malformed fields,
oversized responses, EOF, timeout, extra output, and nonzero exit all fail the replay. The journal
and transcript retain partial artifacts for diagnosis.

Discover the executable version and machine-readable compatibility surface:

```sh
opam exec -- dune exec trading-engine -- --version
opam exec -- dune exec trading-engine -- --capabilities
```

Clients must confirm that both `scenario_contract_versions` and `journal_contract_versions`
contain the scenario's `contract_version` before starting a replay. External clients must also
require their version in `strategy_protocol_versions`.

The final and `.partial` journal paths must not already exist. Batch JSON hashes the same complete
document it parses. JSON Lines input is hashed and validated in a bounded-memory pass before the
journal is created, then replayed from the same open file and hashed again before publication. The
CLI binds that exact-byte hash into the journal. It writes to the partial path and publishes the
requested path only after `run_completed` is fully written and the partial file is closed. An
error preserves the partial artifact for diagnosis.

## Execution summary

An order emitted after slice `n` cannot execute on slice `n`. It first becomes eligible on a later
slice whose start is not earlier than its creation time.

- Market orders attempt the next eligible open and cancel any remainder after that slice.
- Persistent portfolio targets submit a new bounded attempt after each miss until reached or
  superseded.
- Limit orders use deterministic gap improvement and optimistic intrabar touch rules.
- Eligible liquidation orders consume capacity before other orders. Within each origin class,
  sells precede buys and FIFO creation order breaks ties within a side.
- Corporate actions are applied before matching. Splits adjust positions, persistent targets, and
  active orders; cash dividends credit longs and debit shorts in the quote-currency ledger.
- Borrow fees accrue on open shorts for the slice interval before matching.
- Proposed fills are clipped to the largest permitted fractional-lot quantity at the actual fill
  price. Increasing exposure must satisfy position, gross-exposure, leverage, and initial-margin
  limits; exposure-reducing fills remain available.
- A maintenance-margin breach cancels active orders, clears portfolio targets, and creates
  deterministic market orders that flatten positions in bounded lots across later slices.
- The engine emits exactly one valuation after each complete synchronized slice.

Read [Execution model](docs/execution-model.md) for the full phase, price, fee, cash, and accounting
rules.

## Project boundaries

The current scope omits:

- Broker and streaming-market-data connectors
- External execution-report ingestion
- Exchange calendars and time-zone databases
- Durable reducer snapshots and broker reconciliation
- `fsync` and restart recovery for journals
- Tick, trade, and order-book replay

The journal writer flushes each record, creates its partial file exclusively, and finalizes with an
exclusive hard link. It does not call `fsync`, so the journal is an audit artifact rather than a
production recovery log.

## Architecture and contracts

- [Architecture](docs/architecture.md)
- [Scenario contract](docs/scenario.md)
- [Current contract v3 and conformance fixtures](contracts/v3/README.md)
- [Frozen contract v2](contracts/v2/README.md)
- [Historical contract v1](contracts/v1/README.md)
- [Scenario JSON Schema](contracts/v3/scenario.schema.json)
- [Scenario stream record JSON Schema](contracts/v3/scenario-stream.schema.json)
- [Journal record JSON Schema](contracts/v3/journal.schema.json)
- [External strategy protocol v1](contracts/strategy/v1/README.md)
- [Strategy message JSON Schema](contracts/strategy/v1/message.schema.json)
- [Strategy transcript JSON Schema](contracts/strategy/v1/transcript.schema.json)
- [Execution model](docs/execution-model.md)
- [Persistra integration](docs/persistra.md)
- [Contributing](CONTRIBUTING.md)
