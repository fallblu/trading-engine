# Scenario contract

A replay scenario uses either one strict JSON object or a strict JSON Lines stream. Exact prices,
weights, quantities, money, and sequences are canonical JSON strings. Counts and basis points are
JSON integers. Unknown, missing, duplicate, noncanonical, and non-finite values fail parsing.

Use [the v12 demo](../contracts/v12/fixtures/demo.scenario.json) as the canonical complete example.
The [scenario JSON Schema](../contracts/v12/scenario.schema.json) provides structural validation.
The engine parser also enforces cross-field and cross-record invariants. Diagnostics identify the
failed field or array item. Stream diagnostics additionally retain the record line and sequence.

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

The batch object and stream header share one domain-construction path and the same static semantic
checks. Stream items reuse the batch slice and intent validators directly; no synthetic batch
scenario is constructed.

The [stream record JSON Schema](../contracts/v12/scenario-stream.schema.json) validates each line,
and [the v12 stream fixture](../contracts/v12/fixtures/demo.scenario.jsonl) is the canonical example.
The engine validates the entire stream before creating a journal. It then replays one record at a
time without retaining prior slices, scheduled batches, or audit events. Reducer state still
retains current account, order, target, and latest-bar state required by execution semantics.

## Top-level fields

| Field | Meaning |
|---|---|
| `contract_version` | Required string identifying this file contract; v12 is `"12"` |
| `metadata` | Required arbitrary JSON object preserved for provenance and ignored by execution |
| `run_id` | Stable identity used in generated IDs |
| `base_currency` | Reporting currency used for aggregate risk and valuation |
| `initial_portfolio` | Signed cash and positions with accounting history, marks, and FX state |
| `instruments` | Approved executable-instrument catalog, at most 4,096 entries |
| `venue_calendars` | Immutable venue/session policies covering every configured instrument |
| `risk` | Signed position, exposure, leverage, margin, and borrow policy |
| `execution` | Capacity and fee configuration |
| `financing` | Borrow/cash day-count, compounding, locate, recall, and missing-data policies |
| `settlement` | Business-date calendars, per-instrument lags, and cash/position availability policies |
| `max_internal_events` | Positive reducer feedback cap, at most 100,000 |
| `schedule` | Intents emitted after named slices, at most 4,096 per batch |
| `slices` | Complete synchronized market observations |

Metadata may contain nested JSON values. Duplicate object keys and non-finite numbers are rejected
at any depth. Metadata is retained on `Scenario.t` but never affects execution.

An external strategy replay requires `schedule: []`. The JSON Lines form likewise requires every
slice record's `intents` array to be empty. This keeps one authoritative decision source: either
the scenario contract or the separate strategy protocol, never both.

Each JSON Lines record is limited to 1 MiB, excluding its line feed. The reader accepts a final
record without a line feed and drains an oversized record without retaining bytes above the limit.

## Instruments, risk, and execution

Each instrument contains `instrument_id`, `symbol`, `quote_currency`, `tick_size`, and `lot_size`.
Identifiers and labels are nonempty and contain no whitespace or control characters. Tick and lot
sizes are positive exact values with at most six decimal places. Quote currencies may differ from
`base_currency`; `initial_portfolio.cash` contains every distinct quote currency plus the base
currency exactly once.

## Initial portfolio

The v6 `initial_portfolio` contains `cash`, `positions`, `marks`, and `fx_rates`. Cash is signed and
has exact scenario-currency coverage. Each nonzero signed position names a catalog instrument and
records signed `quantity` and `cost_basis`, signed `realized_pnl` and `dividend_pnl`, and
nonnegative `execution_fees` and `borrow_fees`. Basis has the same sign as quantity. These P&L and
fee values are point-in-time histories; importing them does not apply them to cash again.

Position quantities align to instrument lots and respect long and short limits. Marks cover the
position set exactly, are positive, and align to instrument ticks. FX rates cover every scenario
currency exactly and the base rate is one. Before replay, the engine constructs the account,
reconciles its valuation, and enforces gross exposure, leverage, and initial margin. It accepts
negative cash when the complete marked account remains valid under the configured risk policy.

## Venue calendars

Contract v7 requires every instrument to belong to exactly one explicit venue calendar. A calendar
is identified by `venue_id`, `calendar_id`, and `calendar_version`; version 1 is the only supported
calendar payload. Its `sessions` are unique and ordered by `session_date`, and each date declares
one policy: `regular`, `early_close`, or `holiday`. Holidays have no phases. Open sessions must
contain a `regular` phase and may also contain `premarket`, `opening_auction`, `closing_auction`,
and `postmarket` phases in market order. Phase intervals cannot overlap.

All phase boundaries are absolute RFC 3339 timestamps. Scenario producers, not the reducer, resolve
venue-local civil times, time-zone database versions, daylight-saving changes, and clock effects.
Calendar lookup rejects a date without an explicit policy; it never infers weekends, holidays, or
hours from adjacent entries. This makes future DAY expiry, auction eligibility, settlement, and
daily-bar publication policies depend on versioned input rather than ambient system state.

Risk contains portfolio-wide positive `max_gross_exposure` and `max_leverage`, annualized
`short_borrow_bps`, exactly one `instrument_policies` entry per catalog instrument, and an explicit
`groups` array. Instrument policies define order, long, short, notional, initial-margin,
maintenance-margin, and shorting limits. Groups carry versioned identities and explicit membership;
they may overlap and can constrain gross, long, short, absolute net, and gross-to-equity
concentration exposure. Admission and fill clipping include working-order reservations. Every
applicable group is enforced, with group identity providing deterministic tie ordering.

Contract v10 execution contains a stable `model` and a model-owned `configuration`. For
`completed_bar_v1`, configuration version `"2"` contains:

- `version`, the strict model-configuration contract version
- `participation_bps`, from 0 through 10,000
- `fee_schedules`, exactly one schedule per instrument. Each schedule has a stable ID, instrument,
  settlement currency, nullable minimum and maximum, and one or more named components.

Each component declares `currency`, `kind` (`fixed`, `notional_bps`, or `per_unit`), a signed
`value`, `rounding` (`up`, `down`, or `nearest`), and `applies_to` (`any`, `maker`, or `taker`).
Signed values permit rebates. Minimums and maximums are nonnegative and apply per fill after the
component values are converted into the settlement currency.

The engine advertises each model's scenario and configuration versions, required fields, supported
order types, data requirements, and limits through `--capabilities.execution_model_contracts`. The
v8 and earlier contracts retain completed-bar configuration version `"1"`; v3 and v4 preserve
their flat execution object unchanged.

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
FX marks, zero or more corporate actions, and effective-time borrow and cash-rate observations:

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
  "corporate_actions": [],
  "borrow_observations": [
    { "instrument_id": "asset-a", "effective_at": "2026-01-02T14:30:00Z", "available_quantity": "1000", "annual_rate_bps": 100, "recalled": false }
  ],
  "cash_rate_observations": [
    { "currency": "USD", "effective_at": "2026-01-02T14:30:00Z", "credit_rate_bps": 100, "debit_rate_bps": 200 }
  ],
  "settlement_failures": []
}
```

Use `null` volume when unavailable; it means unlimited simulation capacity, not zero. Sequences
are positive and strictly increasing. Slices do not overlap: each start is at or after the prior
end, so equal boundaries are valid. Receipt time never moves backward. Start precedes end,
availability does not precede end, and receipt does not precede availability. OHLC values satisfy
their usual range relationships. Volume may be fractional but must align to the instrument lot.
Each slice supplies exactly one positive FX rate for every scenario currency, and the
base-currency rate is exactly one.

Financing observations are unique per instrument or currency within a slice, effective no later
than the slice start, and strictly advance the effective time for their key across slices. A recall
has zero available quantity. The latest observation remains active until replaced. The top-level
`financing` object selects `actual_365` or `actual_360`, `simple` or `daily`, `reject` or `zero`
missing-data handling, `reject_order` or `clip_fill` locate behavior, and
`reject_new_shorts` or `close_out` recall behavior.

The v12 `settlement` object selects `total_cash` or `settled_cash` buying power and
`total_positions` or `settled_positions` availability. Its immutable calendars contain ordered
canonical business dates, and each instrument has exactly one calendar and a lag from zero through
30 business days. A fill updates economic accounting immediately and creates a deterministic
instruction. Pending cash and quantity appear as unsettled attribution until the first slice on or
after the due date. A due instruction named in that slice's `settlement_failures` becomes failed
instead, retains its unsettled balances, and records the supplied reason.

Supported corporate actions are exact-ratio `split`, per-unit `cash_dividend`, `stock_dividend`,
`rights`, and `spin_off` records. Distribution records name a destination instrument, entitlement
ratio, basis allocation in basis points, and a fractional policy. `reject` fails on a non-lot
entitlement; `cash_in_lieu` requires an explicit destination-quote-currency price and journals the
delivered quantity, fractional quantity, allocated basis, fractional basis, and cash amount. Action
IDs are unique across the scenario. Actions are applied in canonical ID order before borrow fees
and matching. A split rescales the position, persistent target, and active orders while preserving
basis; it does not rescale unit-based risk limits. Split-adjusted positions and targets are
grandfathered under the existing reduce-only position policy. Split-adjusted orders remain active,
but each fill is bounded by `max_order_quantity`; GTC limit remainders may fill on later slices,
while market IOC remainders are cancelled. A dividend changes the quote-currency cash ledger and
realized dividend P&L, crediting a long and debiting a short.

Version 12 slices also carry `lifecycle_events`. Stable `instrument_id` never changes. An
`identifier_change` updates the current symbol and one named provider mapping with provenance;
`halt` and `resume` control whether new exposure is accepted. `expiration` and `delisting` are
terminal and require either `hold` or an explicit quote-currency `cash_out` price. Halts and
terminal events cancel active orders. Terminal events set persistent target exposure to zero and
cash-out clears the position with exact realized-P&L attribution.

For causal next-open execution, an order-changing schedule entry's anchor `received_at` is no later
than the next slice `start_at`.

## Audit journal

The [journal JSON Schema](../contracts/v12/journal.schema.json) validates each JSON Lines record.
Every record contains `contract_version`, `engine_sequence`, deterministic `event_id`, ordered
`causation_ids`, `run_id`, `recorded_at`, `event_type`, and an event-specific `payload`. Causal
references are unique prior event IDs from the same run. The version is repeated on every record
so a journal remains self-describing when it is streamed or split.

The first record is `run_started` with `scenario_sha256` and the selected execution model. In v6,
`initial_state` then records the imported portfolio and reconciled valuation, followed by an
initial `valuation`; both precede the first market slice. The CLI hashes the exact batch document
or stream bytes it parses.
`market_slice_received` contains the complete normalized slice. Portfolio requests record their
basis, original weight when applicable, computed quantity, and sizing reference price. Orders use
`eligible_after_slice_sequence`; fills use `slice_sequence`. `fill_clipped` records the proposed
fill and the greatest lot-aligned permitted quantity. Its reason taxonomy version `1` names one of
`max_order_quantity`, `max_long_position`, `max_short_position`, `max_gross_exposure`,
`max_leverage`, `initial_margin`, or `instrument_borrow_availability` and carries a quantity, money,
ratio, or basis-points threshold.
Each order snapshot retains both creation and latest-update event IDs.

The journal also records split/dividend/distribution application, lifecycle transitions,
action-driven order adjustments, observed
borrow charges, recalls and close-outs, cash-interest entries, margin calls, liquidation-origin
orders, and restoration. Every valuation contains complete
per-currency cash attribution, signed per-instrument native and base-currency attribution, long,
short, net, and gross exposure, execution and borrow fees, and its initial/maintenance margin
snapshot. Those rows reconcile exactly to the aggregate valuation.

A successful replay ends with exactly one `run_completed` record containing the same scenario
hash, reconciled valuation, and mutually exclusive order-status counts. A journal without that
terminal record is incomplete. The requested journal path appears only after exclusive successful
finalization; a failed run retains the `.partial` artifact.

`--durable-artifacts` synchronizes staged contents before publication and containing-directory
metadata after publication and partial cleanup. The filesystem must support hard links plus file
and directory synchronization. Unsupported durability operations fail with `artifact.io` and do
not silently fall back to buffered publication.
