# Scenario and journal

Replay input is either one strict JSON scenario or an equivalent JSON Lines stream. The
[v1 schema](../contracts/v1/scenario.schema.json) and
[canonical fixture](../contracts/v1/fixtures/demo.scenario.json) define the batch form.

```sh
trading-engine --input scenario.json --validate-only
trading-engine --input scenario.jsonl --input-format jsonl --validate-only
```

## Scenario structure

Every scenario carries `"contract_version": "1"` and these top-level fields:

| Field | Purpose |
| --- | --- |
| `metadata` | Producer and dataset provenance; ignored by execution |
| `run_id` | Stable namespace for generated identities |
| `base_currency` | Aggregate reporting currency |
| `initial_portfolio` | Cash, positions, marks, FX, basis, P&L, and fee state |
| `instruments` | Executable catalog with tick, lot, and quote currency |
| `venue_calendars` | Explicit sessions covering every instrument |
| `risk` | Instrument policies, groups, gross exposure, leverage, and borrow limits |
| `execution` | Compiled model and strict v1 model configuration |
| `financing` | Borrow and cash-rate policies |
| `settlement` | Calendars, lags, and cash/position availability |
| `max_internal_events` | Reducer feedback limit |
| `schedule` | Intents emitted after specified slices |
| `slices` | Synchronized market and lifecycle observations |

Decimals are canonical strings with at most six fractional digits. Counts and basis points are
JSON integers. Unknown fields, duplicate keys, non-finite values, missing catalog coverage,
misaligned ticks or lots, invalid timestamps, and noncausal schedules are rejected.

Each slice has explicit start, end, availability, and receipt instants; complete bars and FX marks;
and optional corporate actions, financing observations, settlement failures, lifecycle events,
quotes, trades, and order-book events. Slices are strictly ordered and cannot overlap.

## Stream form

The [stream schema](../contracts/v1/scenario-stream.schema.json) defines three record types:

1. `scenario_header` contains the static configuration.
2. Each `market_slice` contains one slice and its causally adjacent intents.
3. `scenario_end` declares the final slice count.

Every record repeats contract v1 and has a contiguous sequence. The engine validates the complete
stream before creating a journal, then replays it without retaining prior slices or audit events.
Standard input is spooled to a bounded private file so the exact bytes can be validated, hashed,
replayed, and verified again.

## Strategy intents

Scheduled and external strategies may submit full-catalog target weights or quantities, direct
orders, cancellations, and typed metrics. An intent produced after slice `n` cannot execute inside
that slice. External strategy scenarios use an empty schedule so there is only one decision source.

## Journal

The [journal schema](../contracts/v1/journal.schema.json) defines each append-only record. Every
record carries contract v1, a deterministic event ID, ordered prior causation IDs, the run ID,
recording time, event type, and strict payload.

A journal begins with `run_started`, `initial_state`, and an initial valuation. It then records the
normalized slices and all strategy, risk, order, execution, financing, settlement, lifecycle,
accounting, and valuation outcomes. Each completed slice emits one closing valuation. Successful
runs end with one `run_completed` record containing the scenario hash, final valuation, and order
counts.

The requested path is published only after the terminal record is closed successfully. Failures
retain the `.partial` artifact. `--durable-artifacts` additionally synchronizes file and directory
metadata on supported filesystems.
