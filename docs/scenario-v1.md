# Scenario version 1

A replay scenario is one strict JSON object. Every exact price, quantity, money value, and `int64`
sequence is a JSON string. Configuration counts and basis points are JSON integers. Unknown or
missing fields fail parsing.

Use [the demo](../examples/demo.json) as the canonical complete example. The
[scenario JSON Schema](../schemas/scenario-v1.schema.json) supports producer-side structural
validation. The engine parser remains authoritative for cross-field and cross-record invariants.

Run the parser and all replay invariants in memory without creating a journal:

```sh
trading-engine --input scenario.json --validate-only
```

## Top-level fields

| Field | Meaning |
|---|---|
| `schema_version` | Must equal `1` |
| `run_id` | Nonempty stable identity used in generated IDs |
| `base_currency` | One cash and quote currency for the run |
| `initial_cash` | Nonnegative fixed-point cash string |
| `instruments` | Approved executable instrument metadata |
| `risk` | Global order and position limits |
| `execution` | Capacity and fee configuration |
| `max_internal_events` | Positive reducer feedback cap |
| `schedule` | Strategy intents emitted after named bars |
| `bars` | Completed bars in replay receipt order |

Every instrument must use `base_currency`. Instrument IDs must be unique.

## Instruments

Each instrument contains:

```json
{
  "instrument_id": "example-equity",
  "symbol": "EXAMPLE",
  "quote_currency": "USD",
  "tick_size": "0.01",
  "lot_size": "1"
}
```

`tick_size` must be positive. `lot_size` must be a positive whole quantity.

## Risk and execution

Risk contains `max_order_quantity` and `max_position`. Both are positive quantities. Risk is
long-only and includes working buys and sells in projected-position checks.

Execution contains:

- `participation_bps`, from 0 through 10,000
- `fixed_fee`, a nonnegative money string
- `fee_bps`, from 0 through 10,000

## Schedule

A schedule item emits its intents after one bar closes:

```json
{
  "after_bar_sequence": "1",
  "intents": [
    {
      "type": "target_position",
      "instrument_id": "example-equity",
      "quantity": "10"
    }
  ]
}
```

Supported intents are:

- `target_position` with `instrument_id` and `quantity`
- `submit_order` with `instrument_id`, `side`, `quantity`, `order_kind`, and `limit_price`
- `cancel_order` with the deterministic `order_id`
- `emit_metric` with string `name` and `value`

For a market order, `order_kind` is `"market"` and `limit_price` is `null`. For a limit order,
`order_kind` is `"limit"` and `limit_price` is an exact price string.

Multiple schedule entries for the same bar preserve document order. Intents within an entry also
preserve order. A schedule sequence must be nonnegative. Every nonempty schedule entry must refer
to a bar present in the scenario; unmatched actions are rejected instead of silently skipped.

## Bars

Each bar contains:

```json
{
  "source_sequence": "1",
  "instrument_id": "example-equity",
  "start_at": "2026-01-02T14:30:00Z",
  "end_at": "2026-01-02T21:00:00Z",
  "available_at": "2026-01-02T21:00:01Z",
  "received_at": "2026-01-02T21:00:02Z",
  "open": "100",
  "high": "105",
  "low": "99",
  "close": "104",
  "volume": "100"
}
```

Use `null` volume when the source does not supply it. Missing volume means unlimited simulation
capacity; it does not mean zero.

The source sequence must increase globally. Receipt times cannot move backward. Start must precede
end, availability cannot precede end, and receipt cannot precede availability. Each instrument's
bar end must increase. OHLC values must satisfy their usual range relationships.

## Audit journal

The [journal JSON Schema](../schemas/journal-v1.schema.json) describes one JSON Lines record. Each
record has:

```json
{
  "schema_version": 1,
  "engine_sequence": "1",
  "run_id": "example",
  "recorded_at": "2026-01-02T21:00:02.000000Z",
  "event_type": "bar_received",
  "payload": {}
}
```

`engine_sequence` orders external and derived events. `recorded_at` is the current receipt time in
replay. Generated order IDs use `<run-id>-order-<12-digit-number>`. Fill IDs use the corresponding
`fill` form. Each order payload includes `created_at`, which equals the replay time when the order
was accepted or rejected and constrains causal bar eligibility.

For every scheduled target, submitted order, or cancellation, the engine requires the anchor
bar's `received_at` to be no later than the affected instrument's next bar `start_at`. This keeps
completed-bar callbacks from retroactively changing an opening execution. The parser checks this
cross-record rule in addition to the JSON Schema.

A successful replay writes exactly one `run_completed` record after all bar, strategy, order,
fill, and valuation records. Its payload contains the final reconciled valuation and counts for
total, active, filled, rejected, and cancelled orders. These status counts are mutually exclusive
and sum to the total. The completion time equals the last bar's `received_at`. A valid replay with
no bars uses the Unix epoch because it has no source receipt time.

The journal writer flushes the completion line before reporting success. Treat a journal without
that terminal record as incomplete, even if its preceding records are valid.
