# Architecture

The engine keeps deterministic transitions separate from effects. The library accepts validated
values and returns a new state plus ordered audit events. The CLI owns input reads, hashing,
journal files, and the runtime shell.

## Layers

| Layer | Responsibility |
|---|---|
| `Id`, `Scalar` | Opaque identities and checked fixed-point values |
| `Instrument`, `Bar`, `Market_slice`, `Order`, `Fill` | Validated domain contracts |
| `Strategy` | Pure callback, immutable context, and typed intents |
| `Planner` | Exact weight-to-quantity conversion |
| `Risk` | Catalog, size, lot, tick, position, and working-sell checks |
| `Oms` | Order lifecycle, fill limits, idempotency, and deterministic ordering |
| `Execution` | Synchronized-slice matching, capacity allocation, and fees |
| `Account` | Cash, positions, average cost, fees, P&L, and valuation |
| `Engine` | Sequencing, portfolio reconciliation, and pure orchestration |
| `Scenario`, `Scenario_stream`, `Replay` | Strict batch and bounded-memory input runners |
| `Sha256`, `Codec`, `Journal` | Input identity, stable audit JSON, and file publication |

## Reducer phases

For each synchronized market slice, the engine:

1. Validates catalog coverage, slice order, receipt order, and market time.
2. Emits `run_started` once and then `market_slice_received`.
3. Builds one matching batch from orders that became eligible after an earlier slice.
4. Offers each instrument's remaining capacity to sells first, then buys, using FIFO within each
   side.
5. Applies sell fills before evaluating buy affordability.
6. Clips buy fills to affordable lots, consumes only applied capacity, and emits `cash_limited`
   when clipping occurs.
7. Cancels eligible market-order remainders.
8. Stores every synchronized close as the current mark.
9. Delivers captured fill, order, and `Market_slice_closed` callbacks.
10. Applies scheduled intents and reconciles the persistent portfolio target once.
11. Emits one valuation for the complete slice.

Target-generated market orders are limited to `max_order_quantity`, rounded down to a lot, and
retried after later slices. A new portfolio target atomically supersedes the prior desired
quantities and cancels any working target orders. Direct limits remain GTC.

After the final slice, the runner emits `run_completed` with the scenario hash, final valuation,
and order-status counts. The reducer rejects later slices and duplicate completion.

## Time and order

Each market slice exposes:

- `end_at`: the market event time represented by the completed bars
- `available_at`: the earliest time a strategy could use the slice
- `received_at`: when this run observed the slice
- `slice_sequence`: stable, positive source order

All configured instruments share these values within a slice. `engine_sequence` orders every
external and derived audit event. The replay clock uses `received_at`; each order records that time
as `created_at`. A slice starting before an order was created cannot execute that order.

Schedule sequences and slice sequences are positive and strictly increasing. Every schedule entry
anchors to an existing slice. A scheduled order-changing intent must be received no later than the
next slice start.

## Determinism

Determinism depends on:

- Immutable state transitions
- Ordered maps instead of hash-table iteration
- Complete synchronized slices and stable slice sequences
- Sell-first matching and FIFO order sorting by creation sequence and ID
- Canonical exact strings and checked fixed-point arithmetic
- Explicit fee, participation, fill, sizing, and affordability rules
- Stable generated order and fill IDs derived from the run ID
- Stable JSON field and event order
- The exact scenario-byte SHA-256 in both terminal audit records

Running the same scenario bytes produces byte-identical audit lines.

The JSON Lines runner hashes and validates the complete stream before journal creation. It then
replays one slice-plus-intents record at a time and does not accumulate market slices, schedule
maps, or audit events. A required terminal record distinguishes completion from truncation.

## Invariants

The implementation and tests enforce:

- Every slice contains exactly one bar for every configured instrument.
- Filled quantity stays between zero and requested quantity.
- Terminal orders cannot accept new fills or cancellations.
- Equal duplicate fill reports are idempotent; conflicting ID reuse fails.
- Position quantity, cost basis, cash, market value, equity, and total fees stay nonnegative.
- Cash equals initial cash minus buys and fees plus sells net of fees.
- Equity equals cash plus marked position value.
- Equity change equals realized plus unrealized P&L without external cash flows.
- Fills respect order size, capacity, lot size, tick size, and causal time.
- Every configured instrument uses the single base currency.

## Path to paper and live operation

A paper or live milestone should add a public venue-command and execution-report boundary before a
broker adapter:

```text
engine command ──► venue adapter ──► broker
      ▲                                │
      └──────── execution report ◄─────┘
```

That work must add pending broker states, external report sequencing, a durable command outbox,
restart snapshots, and broker reconciliation without adding effects to the reducer.
