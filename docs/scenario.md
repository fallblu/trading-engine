# Scenario contract

A replay scenario uses either one strict JSON object or a strict JSON Lines stream. Exact prices,
weights, quantities, money, and sequences are canonical JSON strings. Counts and basis points are
JSON integers. Unknown, missing, duplicate, noncanonical, and non-finite values fail parsing.

Use [the v3 demo](../contracts/v3/fixtures/demo.scenario.json) as the canonical complete example.
The [scenario JSON Schema](../contracts/v3/scenario.schema.json) provides structural validation.
The engine parser also enforces cross-field and cross-record invariants.

```sh
trading-engine --input scenario.json --validate-only
trading-engine --input scenario.jsonl --input-format jsonl --validate-only
```

## JSON Lines stream

Use the stream for histories that should not be materialized inside the engine. The first record
is `scenario_header` and carries the static top-level fields. Each `market_slice` record carries
one complete slice and the intents evaluated after that slice. The final `scenario_end` record
declares the number of slices. It is required even for an empty stream, so a truncated valid
prefix cannot be mistaken for a complete scenario.

Every record has exactly `contract_version`, `scenario_sequence`, `record_type`, and `payload`.
The contract version is repeated, and `scenario_sequence` is contiguous from one. Intents are
adjacent to their decision slice rather than stored in a future-looking global schedule. Before
replay, the reader checks each intent-bearing slice against the next slice's start time while
retaining only those two records.

The [stream record JSON Schema](../contracts/v3/scenario-stream.schema.json) validates each line,
and [the v3 stream fixture](../contracts/v3/fixtures/demo.scenario.jsonl) is the canonical example.
The engine validates the entire stream before creating a journal. It then replays one record at a
time without retaining prior slices, scheduled batches, or audit events. Reducer state still
retains current account, order, target, and latest-bar state required by execution semantics.

## Top-level fields

| Field | Meaning |
|---|---|
| `contract_version` | Required string identifying this file contract; v3 is `"3"` |
| `metadata` | Required arbitrary JSON object preserved for provenance and ignored by execution |
| `run_id` | Stable identity used in generated IDs |
| `base_currency` | Reporting currency used for aggregate risk and valuation |
| `initial_cash` | One explicit nonnegative balance for every scenario currency |
| `instruments` | Approved executable-instrument catalog |
| `risk` | Signed position, exposure, leverage, margin, and borrow policy |
| `execution` | Capacity and fee configuration |
| `max_internal_events` | Positive reducer feedback cap, at most `4611686018427387903` |
| `schedule` | Intents emitted after named slices |
| `slices` | Complete synchronized market observations |

Metadata may contain nested JSON values. Duplicate object keys and non-finite numbers are rejected
at any depth. Metadata is retained on `Scenario.t` but never affects execution.

An external strategy replay requires `schedule: []`. The JSON Lines form likewise requires every
slice record's `intents` array to be empty. This keeps one authoritative decision source: either
the scenario contract or the separate strategy protocol, never both.

## Instruments, risk, and execution

Each instrument contains `instrument_id`, `symbol`, `quote_currency`, `tick_size`, and `lot_size`.
Identifiers and labels are nonempty and contain no whitespace or control characters. Tick and lot
sizes are positive exact values with at most six decimal places. Quote currencies may differ from
`base_currency`; `initial_cash` contains every distinct quote currency plus the base currency
exactly once.

Risk contains positive `max_order_quantity`, `max_long_position`, `max_short_position`,
`max_gross_exposure`, and `max_leverage` values, initial and maintenance margin basis points, and
annualized `short_borrow_bps`. Initial margin cannot be below maintenance margin, and each
quantity limit must cover at least one lot for every instrument. Orders that increase gross
exposure must satisfy every applicable limit; exposure-reducing orders remain admissible.

Execution contains:

- `model`, the compiled execution module selected by contract name; v3 supports
  `completed_bar_v1`
- `participation_bps`, from 0 through 10,000
- `fixed_fee`, a nonnegative money string
- `fee_bps`, from 0 through 10,000

## Schedule and intents

Schedule entries are positive, strictly increasing, and anchored to existing slices:

```json
{
  "after_slice_sequence": "1",
  "intents": [
    {
      "type": "target_weights",
      "targets": [
        { "instrument_id": "asset-a", "weight": "0.6" },
        { "instrument_id": "asset-b", "weight": "0.3" }
      ]
    }
  ]
}
```

Supported intents are:

- `target_weights` with a `targets` array of `instrument_id` and `weight`
- `target_quantities` with a `targets` array of `instrument_id` and `quantity`
- `submit_order` with instrument, side, quantity, kind, and nullable limit price
- `cancel_order` with a deterministic `order_id`
- `emit_metric` with string `name` and `value`

Both target forms contain every configured instrument exactly once. Weights and quantities are
signed. Gross absolute weight must not exceed `max_leverage`; quantity targets align to their
instrument lots and stay within the long and short position limits. A rebalance that crosses from
long to short, or short to long, first flattens the existing position and continues toward the
target on a later attempt.

A market submission uses `"order_kind": "market"` and `"limit_price": null`. A limit submission
uses `"order_kind": "limit"` and a canonical positive price. Static order size, lot, and tick
checks run during parsing; position and outstanding-order checks run in the reducer.

## Market slices

Each slice has common timing, one bar per configured instrument, a complete set of currency-to-base
FX marks, and zero or more corporate actions:

Timestamps use `YYYY-MM-DD[Tt]HH:MM:SS`, optional one-to-six fractional-second digits, and either
`Z`/`z` or a colonized numeric offset such as `-05:00`. Seconds range from `00` through `59`.
Audit timestamps use the same boundary.

```json
{
  "slice_sequence": "1",
  "start_at": "2026-01-02T14:30:00Z",
  "end_at": "2026-01-02T21:00:00Z",
  "available_at": "2026-01-02T21:00:01Z",
  "received_at": "2026-01-02T21:00:02Z",
  "bars": [
    {
      "instrument_id": "asset-a",
      "open": "100",
      "high": "105",
      "low": "99",
      "close": "104",
      "volume": "100"
    }
  ],
  "fx_rates": [
    { "currency": "USD", "rate": "1" }
  ],
  "corporate_actions": []
}
```

Use `null` volume when unavailable; it means unlimited simulation capacity, not zero. Sequences
are positive and strictly increasing. End times strictly increase and receipt time never moves
backward. Start precedes end, availability does not precede end, and receipt does not precede
availability. OHLC values satisfy their usual range relationships. Volume may be fractional but
must align to the instrument lot. Each slice supplies exactly one positive FX rate for every
scenario currency, and the base-currency rate is exactly one.

Supported corporate actions are exact-ratio `split` and per-unit `cash_dividend` records. Action
IDs are unique across the scenario. Actions are applied in canonical ID order before borrow fees
and matching. A split rescales the position, persistent target, and active orders while preserving
basis; a dividend changes the quote-currency cash ledger and realized dividend P&L, crediting a
long and debiting a short.

For causal next-open execution, an order-changing schedule entry's anchor `received_at` is no later
than the next slice `start_at`.

## Audit journal

The [journal JSON Schema](../contracts/v3/journal.schema.json) validates each JSON Lines record.
Every record contains `contract_version`, `engine_sequence`, deterministic `event_id`, ordered
`causation_ids`, `run_id`, `recorded_at`, `event_type`, and an event-specific `payload`. Causal
references are unique prior event IDs from the same run. The version is repeated on every record
so a journal remains self-describing when it is streamed or split.

The first record is `run_started` with `scenario_sha256` and the selected execution model. The CLI
hashes the exact batch document or stream bytes it parses.
`market_slice_received` contains the complete normalized slice. Portfolio requests record their
basis, original weight when applicable, computed quantity, and sizing reference price. Orders use
`eligible_after_slice_sequence`; fills use `slice_sequence`. `margin_limited` records a proposed
fill and the greatest lot-aligned quantity permitted by position, exposure, leverage, and initial
margin policy. Each order snapshot retains both creation and latest-update event IDs.

The journal also records split/dividend application, split-driven order adjustments, short borrow
fees, margin calls, liquidation-origin orders, and restoration. Every valuation contains complete
per-currency cash attribution, signed per-instrument native and base-currency attribution, long,
short, net, and gross exposure, execution and borrow fees, and its initial/maintenance margin
snapshot. Those rows reconcile exactly to the aggregate valuation.

A successful replay ends with exactly one `run_completed` record containing the same scenario
hash, reconciled valuation, and mutually exclusive order-status counts. A journal without that
terminal record is incomplete. The requested journal path appears only after exclusive successful
finalization; a failed run retains the `.partial` artifact.
