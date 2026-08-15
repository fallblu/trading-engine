# Architecture

The engine keeps deterministic transitions separate from effects. The library accepts validated
values and returns a new state plus ordered audit events. The CLI owns file access and the Eio
runtime shell.

## Layers

| Layer | Responsibility |
|---|---|
| `Id`, `Scalar` | Opaque identities and checked fixed-point values |
| `Instrument`, `Bar`, `Order`, `Fill` | Validated domain contracts |
| `Strategy` | Pure callback, immutable context, and typed intents |
| `Planner` | Target-position conversion and target-order replacement |
| `Risk` | Base-currency catalog, size, lot, tick, position, and working-sell checks |
| `Oms` | Order lifecycle, fill limits, idempotency, and deterministic ordering |
| `Execution` | Completed-bar matching, volume allocation, and fees |
| `Account` | Cash, positions, average cost, fees, P&L, and valuation |
| `Engine` | Sequencing and orchestration of the pure layers |
| `Scenario`, `Replay` | Strict input contract and deterministic batch runner |
| `Codec`, `Journal` | Stable audit JSON and append-only file output |

## Reducer phases

For each completed bar, the engine:

1. Validates the instrument, source sequence, receipt order, and per-instrument end time.
2. Emits `bar_received`.
3. Matches only orders accepted after an earlier bar.
4. Applies fills through the OMS and account.
5. Cancels remaining quantities for eligible market orders.
6. Stores the new close as the current mark.
7. Delivers captured fill and order-update callbacks to the strategy.
8. Delivers `Bar_closed` and processes the resulting intents in list order.
9. Emits a reconciled valuation.

The simulator computes one deterministic venue batch from the OMS state at the start of the bar.
Each callback receives a context captured immediately after its own transition. Strategy actions
run after already queued venue notifications, so they cannot retroactively change fills inferred
inside the completed bar.

The engine caps internal callbacks and intents per external bar. The cap turns a feedback loop into
a deterministic error.

## Time and order

The bar contract keeps four dimensions visible:

- `end_at` is the market event time represented by the completed bar.
- `available_at` is the earliest time a strategy could use it.
- `received_at` is when this engine run observed it.
- `source_sequence` provides stable source order.

`engine_sequence` orders every external and derived audit event. The replay clock uses
`received_at`. It never substitutes file retrieval time for market availability.

The version 1 source sequence is global within a scenario. Receipt time cannot move backward.
Each instrument's bar end must increase.

## Determinism

Determinism depends on:

- Immutable state transitions
- Ordered OCaml maps instead of hash-table iteration
- Global source sequences
- FIFO order sorting by creation sequence and order ID
- Fixed-point arithmetic with checked `int64` storage and wide proportional intermediates
- Explicit fee, participation, and fill rules
- Stable generated order and fill IDs derived from the run ID
- Stable JSON field and event order

Running the same scenario with the same run ID produces byte-identical audit lines.

## Invariants

The implementation and tests enforce:

- Filled quantity stays between zero and requested quantity.
- Terminal orders cannot accept new fills or cancellations.
- Equal duplicate fill reports are idempotent; conflicting reuse of a fill ID fails.
- A position quantity never becomes negative.
- Cost basis is nonnegative and becomes zero when a position closes.
- Cash equals initial cash minus buys and fees plus sells net of fees.
- Total fees equal the sum of fill fees.
- Equity equals cash plus marked position value.
- Equity change equals realized plus unrealized P&L when there are no external cash flows.
- Fills respect order size, bar capacity, lot size, and tick size.
- Every configured instrument uses the engine's single base currency.

## Path to paper and live operation

This prototype separates the simulator, OMS, account, strategy, and runtime, but the engine still
invokes the completed-bar simulator directly. A paper/live milestone should add a public venue
command and execution-report boundary before writing a broker adapter:

```text
engine command ──► venue adapter ──► broker
      ▲                                │
      └──────── execution report ◄─────┘
```

That milestone must add pending broker states, external report sequencing, durable command
outboxes, restart snapshots, and broker reconciliation. It should preserve the current domain and
accounting reducers rather than add network calls to them.
