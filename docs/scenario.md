# Scenario contract

A replay scenario is one strict JSON object. Exact prices, weights, quantities, money, and
sequences are canonical JSON strings. Counts and basis points are JSON integers. Unknown,
missing, duplicate, noncanonical, and non-finite values fail parsing.

Use [the v1 demo](../contracts/v1/fixtures/demo.scenario.json) as the canonical complete example.
The [scenario JSON Schema](../contracts/v1/scenario.schema.json) provides structural validation.
The engine parser also enforces cross-field and cross-record invariants.

```sh
trading-engine --input scenario.json --validate-only
```

## Top-level fields

| Field | Meaning |
|---|---|
| `contract_version` | Required string identifying this file contract; v1 is `"1"` |
| `metadata` | Required arbitrary JSON object preserved for provenance and ignored by execution |
| `run_id` | Stable identity used in generated IDs |
| `base_currency` | Single cash and quote currency |
| `initial_cash` | Nonnegative canonical cash string |
| `instruments` | Approved executable-instrument catalog |
| `risk` | Global order and position limits |
| `execution` | Capacity and fee configuration |
| `max_internal_events` | Positive reducer feedback cap |
| `schedule` | Intents emitted after named slices |
| `slices` | Complete synchronized market observations |

Metadata may contain nested JSON values. Duplicate object keys and non-finite numbers are rejected
at any depth. Metadata is retained on `Scenario.t` but never affects execution.

## Instruments, risk, and execution

Each instrument contains `instrument_id`, `symbol`, `quote_currency`, `tick_size`, and `lot_size`.
Identifiers and labels are nonempty and contain no whitespace or control characters. Tick and lot
sizes are positive. Every quote currency equals `base_currency`.

Risk contains positive `max_order_quantity` and `max_position` values. Both limits must cover at
least one lot for every instrument. Direct and computed target orders respect these limits.

Execution contains:

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

Both target forms contain every configured instrument exactly once. Weights range from zero
through one and sum to at most one. Quantity targets align to their instrument lots and do not
exceed `max_position`.

A market submission uses `"order_kind": "market"` and `"limit_price": null`. A limit submission
uses `"order_kind": "limit"` and a canonical positive price. Static order size, lot, and tick
checks run during parsing; position and outstanding-order checks run in the reducer.

## Market slices

Each slice has common timing and one bar per configured instrument:

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
  ]
}
```

Use `null` volume when unavailable; it means unlimited simulation capacity, not zero. Sequences
are positive and strictly increasing. End times strictly increase and receipt time never moves
backward. Start precedes end, availability does not precede end, and receipt does not precede
availability. OHLC values satisfy their usual range relationships.

For causal next-open execution, an order-changing schedule entry's anchor `received_at` is no later
than the next slice `start_at`.

## Audit journal

The [journal JSON Schema](../contracts/v1/journal.schema.json) validates each JSON Lines record.
Every record contains `contract_version`, `engine_sequence`, `run_id`, `recorded_at`, `event_type`,
and an event-specific `payload`. The version is repeated on every record so a journal remains
self-describing when it is streamed or split.

The first record is `run_started` with `scenario_sha256`. The CLI hashes the same bytes it parses.
`market_slice_received` contains the complete normalized slice. Portfolio requests record their
basis, original weight when applicable, computed quantity, and sizing reference price. Orders use
`eligible_after_slice_sequence`; fills use `slice_sequence`. `cash_limited` records the execution
proposal, affordable quantity, instrument, order, and actual price.

A successful replay ends with exactly one `run_completed` record containing the same scenario
hash, reconciled valuation, and mutually exclusive order-status counts. A journal without that
terminal record is incomplete. The requested journal path appears only after exclusive successful
finalization; a failed run retains the `.partial` artifact.
