# Architecture

The engine keeps deterministic transitions separate from effects. The library accepts validated
values and returns a new state plus ordered audit events. The CLI owns input reads, hashing,
journal files, and the runtime shell.

## Layers

| Layer | Responsibility |
|---|---|
| `Id`, `Scalar` | Opaque identities and checked fixed-point values |
| `Instrument`, `Bar`, `Market_slice`, `Corporate_action`, `Order`, `Fill` | Validated domain contracts |
| `Strategy` | Pure callback, immutable context, and typed intents |
| `Planner` | Exact weight-to-quantity conversion |
| `Risk` | Catalog, size, alignment, signed position, exposure, leverage, and margin checks |
| `Oms` | Order lifecycle, fill limits, idempotency, and deterministic ordering |
| `Execution`, `Execution_model` | Pluggable synchronized-slice matching, capacity allocation, and fees |
| `Account` | Currency ledgers, signed positions, attribution, average cost, fees, P&L, and valuation |
| `Engine` | Sequencing, portfolio reconciliation, and pure suspend/resume orchestration |
| `Scenario_shape` | Exact batch, stream-header, and stream-item JSON fields |
| `Scenario` | Domain construction shared by batch and stream inputs |
| `Scenario_validation` | Shared cross-field and cross-record scenario invariants |
| `Scenario_stream`, `Replay` | Bounded-memory stream adaptation and scripted runners |
| `Strategy_protocol`, `Strategy_process`, `External_replay` | Versioned child supervision and external runners |
| `Sha256`, `Codec`, `Diagnostic`, `Artifact_writer`, `Journal`, `Strategy_transcript` | Input identity, stable diagnostics and audit JSON, and file publication |

Boundary failures use the versioned [diagnostic contract](diagnostics.md). Pure domain constructors
and reducer internals keep plain errors inside the deterministic boundary; replay adapters attach
stable codes, phases, source locations, event causality, and sanitized exception details before
returning an error to callers.

Batch and stream headers use the same domain construction and static semantic checks. Stream
items reuse the batch slice, intent, timeline, and catalog validators against the prior item;
they do not construct temporary batch documents. Shape, construction, and semantic errors retain
their precise JSON path, while the stream adapter adds the record line and sequence.

The journal and strategy transcript share one typed-state artifact lifecycle for exclusive staging,
append, close, no-replace publication, and cleanup. Artifact writers and the process supervisor route
their minimal operating-system operations through one boundary dispatcher. Production executes
those effects directly. Failure-path tests replace one operation at a time, including partial
writes, without introducing files, pipes, processes, or fault state into the reducer.

## Reducer phases

For each synchronized market slice, the engine:

1. Validates catalog coverage, slice order, receipt order, and market time.
2. Stores the synchronized closes and complete FX vector, emits `run_started` once, and then emits
   `market_slice_received`.
3. Applies splits and dividends, adjusting signed positions, persistent targets, and active orders.
4. Accrues borrow fees on open shorts for the slice interval.
5. Fixes the priority sequence of orders that became eligible after an earlier slice.
6. Offers each instrument's remaining capacity to liquidation orders first, then applies
   sell-before-buy and FIFO priority within each origin class.
7. Clips proposals to the largest lot-aligned quantity allowed by position, exposure, leverage,
   and initial-margin risk; only applied quantity consumes capacity.
8. Pauses matching to deliver each fill and order request, then applies the response before the
   next callback or eligible order.
9. Cancels eligible market-order remainders and delivers their order updates.
10. Delivers `market_slice_closed` with the same clock, bars, and FX marks as the fill callbacks.
11. Reconciles the persistent portfolio target once after the closing callback.
12. Assesses maintenance margin, cancelling active orders and creating bounded liquidation orders
    when breached.
13. Delivers resulting order updates and drains strategy feedback.
14. Emits one base-currency valuation with complete cash, position, fee, and margin attribution.

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
external and derived audit event. Each record's deterministic `event_id` combines `run_id` with
that sequence, while sorted `causation_ids` point only to earlier records in the same run. Orders
retain the event that created them. The replay clock uses `received_at`; each order records that
time as `created_at`. A slice starting before an order was created cannot execute that order.

Schedule sequences and slice sequences are positive and strictly increasing. Every schedule entry
anchors to an existing slice. A scheduled order-changing intent must be received no later than the
next slice start.

Batch validation indexes each slice together with its successor in an ordered map. Building that
index costs `O(s log s)` for `s` slices, and each of the `m` scheduled sequence lookups costs
`O(log s)`. Validation does not rescan the slice list for each schedule entry.

## Determinism

Determinism depends on:

- Immutable state transitions
- Ordered maps instead of hash-table iteration
- Complete synchronized slices and stable slice sequences
- Liquidation-first, sell-before-buy, and FIFO order sorting
- Canonical exact strings and checked fixed-point arithmetic
- Explicit fee, participation, fill, sizing, and margin-risk rules
- Explicit corporate actions, FX marks, borrow rates, and margin policy
- Stable generated order and fill IDs derived from the run ID
- Stable event IDs and canonical causal-reference ordering
- Stable JSON field and event order
- The exact scenario-byte SHA-256 in both terminal audit records

Running the same scenario bytes produces byte-identical audit lines.

`Engine.Interactive` stops at each strategy request and exposes the immutable context and event.
Its `resume` transition accepts typed intents and continues the same pure reducer. The scripted
runner invokes an in-process callback at that boundary. The external runner serializes it through
protocol v3. Reducer state never contains a process, clock, pipe, timeout, or file handle.

Each strategy callback carries an account valuation built at that reducer boundary. All callbacks
for a slice use its receipt time, completed bars, and FX vector. A callback response is reduced
before any later callback or eligible order, so the next context exposes its effects.
Positive-equity accounts expose realized portfolio weights; zero- and negative-equity accounts
explicitly omit weights.

Reducer feedback uses an immutable two-list queue. Adding generated notifications to the tail and
removing the next item are amortized constant-time operations. Prepending one callback's response
costs only the size of that response. Queue representation changes do not affect processing order
or the exact `max_internal_events` count.

The JSON Lines runner hashes and validates the complete stream before journal creation. It then
replays one slice-plus-intents record at a time and does not accumulate market slices, schedule
maps, or audit events. A required terminal record distinguishes completion from truncation.

External replay requires an empty batch schedule or empty streamed intent batches. The effectful
supervisor launches an explicit argument vector, permits one request at a time, enforces a
per-exchange timeout and 1 MiB message limit, then requires a clean child exit with no extra
standard output. A response may contain at most 4,096 intents. It records every accepted request
and response in sequence. Failures preserve
the transcript and journal partials. After both writers close, publication links every final path
without replacement before moving any partial path to a reserved cleanup name. Only after every
move succeeds does the transaction unlink those cleanup names. A close or link failure rolls back
final links created by the transaction. A move or cleanup failure keeps the complete final set and
restores every partial name.

Buffered publication flushes every record. Durable publication also synchronizes each staged file
before close, synchronizes each containing directory after all final links exist, removes partial
links, and synchronizes the directories again. Unsupported file or directory synchronization is an
artifact failure. The failure path rolls back an unpublished final set or restores partial names
beside an already complete final set.

## Invariants

The implementation and tests enforce:

- Every slice contains exactly one bar for every configured instrument.
- Filled quantity stays between zero and requested quantity.
- Terminal orders cannot accept new fills or cancellations.
- Equal duplicate fill reports are idempotent; conflicting ID reuse fails.
- Order and fill quantities are positive, while positions, targets, cash, basis, P&L, and equity
  may be signed.
- A fill can close a position but cannot cross it through zero.
- Every slice contains exactly one FX mark for every scenario currency, with base FX equal to one.
- Each currency ledger equals its initial balance plus native fills, dividends, and borrow fees.
- Equity equals cash plus marked position value.
- Native and base position values, basis, realized P&L, unrealized P&L, dividends, and fees sum
  exactly to each valuation's account aggregates.
- Fills respect order size, capacity, lot size, tick size, and causal time.
- Exposure-increasing fills respect long/short position, gross-exposure, leverage, and initial
  margin limits; maintenance breaches produce deterministic cancel-and-liquidate transitions.
- Corporate action IDs are unique, actions precede matching, and split-adjusted state remains
  exactly aligned to configured lots and ticks.

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
