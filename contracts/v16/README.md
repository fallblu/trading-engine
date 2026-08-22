# Trading Engine contract v16

This directory is the authoritative v16 process and file contract shared by Trading Engine and its
clients. Versions 14 through 3 remain readable during their client transitions.

- `scenario.schema.json` validates batch replay inputs.
- `scenario-stream.schema.json` validates each JSON Lines scenario-stream record.
- `journal.schema.json` validates each JSON Lines audit record.
- The files under `fixtures/` form the canonical valid conformance corpus.
- `fill-clipped.scenario.json` and its journal exercise a leverage-limited partial fill.

Version 7 requires exactly one explicit risk policy per catalog instrument. Each policy defines
order, signed-position, notional, initial-margin, maintenance-margin, and shorting limits. Versioned
risk groups have explicit membership, may overlap, and can constrain gross, long, short, absolute
net, and gross-to-equity concentration exposure.

Runtime validation requires exact currency, position-mark, and FX coverage; known instruments;
lot-aligned quantities; tick-aligned positive marks; basis with the same sign as quantity;
nonnegative fee histories; the base FX rate equal to one; instrument, group, aggregate exposure,
leverage, and initial-margin limits. Signed cash is valid. A successful v16 run emits `initial_state`
immediately after `run_started`, followed by a reconciled initial `valuation`, before market data.

Admission and fill clipping include working-order reservations. When multiple groups limit the same
fill, lexical group identity is the deterministic tie breaker. Valuations and strategy contexts
carry group exposure snapshots, and clipping thresholds identify the exact instrument or group.

Every v16 scenario, stream record, and journal record carries `"contract_version": "16"`.

Version 8 adds explicit `market`, `limit`, `stop`, and `stop_limit` orders with `gtc`, `ioc`,
`fok`, `day`, and `gtd` time-in-force policies. `day` orders identify both their venue and the
exact versioned calendar; `gtd` orders carry an absolute expiry timestamp. Older contracts retain
their frozen mapping: market orders are IOC and limit orders are GTC.

Stops evaluate only completed OHLCV bars. A gap through the trigger records the bar start as the
trigger time; an intrabar touch records the bar end. Trigger state and slice sequence are journaled,
and an activated order cannot execute before the following slice. A stop becomes a market order;
a stop-limit becomes its configured limit order. Splits adjust both trigger and limit prices.

IOC orders cancel any remainder after their first eligible slice. FOK orders fill only when the
full remaining quantity fits both execution capacity and risk capacity, otherwise they cancel with
no fill. DAY orders cancel after matching the slice that reaches the selected session's final
phase close. GTD orders cancel before matching any completed bar whose end reaches or passes the
expiry, avoiding ambiguous partial-bar execution.

The v10 `execution` object uses `completed_bar_v1` configuration version `"2"`: participation basis
points plus exactly one composable fee schedule per instrument. Named fixed, notional-basis-point,
and per-unit components declare currency, rounding, and maker/taker applicability. Optional
per-fill minimums and caps use the schedule settlement currency; negative components represent
rebates. Fills and valuations retain every native, quote, and base-currency attribution. Runtime
capabilities also advertise frozen configuration version `"1"` for older scenario contracts.

Version 10 adds a required `financing` policy and effective-time observations on every market
slice. Borrow observations provide per-instrument locate availability, signed annual rates, and
recall state. Cash observations provide separate annual credit and debit rates per currency.
Policies select Actual/365 or Actual/360 day count, simple or daily compounding, missing-data
handling, locate rejection or fill clipping, and recall rejection or deterministic close-out.

Borrow availability is enforced when a fill would create or increase a short. Recalls cancel
active sells and may submit priority IOC covers until the position is flat. Borrow charges and cash
interest use the exact slice interval, update native ledgers deterministically, and emit dedicated
journal records. Valuations report cash interest separately and include it in aggregate realized
P&L. Version 9 and earlier retain their frozen fixed-borrow behavior and wire shapes.

Version 12 separates trade-date economic accounting from settlement-date availability. A required
settlement policy selects total or settled cash buying power and total or settled position
availability. Versioned calendars enumerate canonical business dates, and each instrument has an
explicit business-day lag. Every fill creates a deterministic settlement instruction containing
its cash and position movements, trade date, and due date. A due instruction either settles on the
first eligible slice or records a named failure supplied by that slice.

Valuations and strategy contexts report settled and unsettled cash and quantities without changing
economic equity. Journals include instruction-created, completed, and failed events. Scenario v10
and strategy protocol v8 retain their frozen immediate-settlement wire behavior.

Version 12 adds exact stock-dividend, rights, and spin-off distributions. Each distribution names
its destination instrument, exact entitlement ratio, basis allocation in basis points, and either
rejects fractional entitlements or converts them to cash at an explicit price and currency.
Stock dividends adjust persistent targets and eligible working orders; every distribution journals
delivered quantity, fractional quantity, allocated basis, fractional basis, and cash in lieu.

Lifecycle events keep stable instrument identity separate from mutable symbol and provider
mappings. Halt and resume transitions control tradability. Expiration and delisting are terminal,
cancel active orders, clear target exposure, and require an explicit hold or cash-out policy.
Cash-out specifies its terminal price and currency. Every transition journals the source event,
resulting listing state, provider provenance, liquidated quantity, and cash attribution.

Version 14 adds `completed_bar_next_open_v1` and `completed_bar_adverse_touch_v1` without changing
the frozen `completed_bar_v1` semantics. Next-open limits require a marketable later open;
adverse-touch limits require a one-tick trade-through before a maker fill is eligible. Both models
declare fixed half-spread and linear participation-impact catalogs, including an explicit policy
for missing bar volume. Price costs round away from the reference price to instrument ticks and
cannot violate a limit. An `execution_price_selected` audit record attributes the reference price,
spread adjustment, impact adjustment, and final executable price before each fill.

Version 14 adds `quote_trade_v1` and causally ordered `market_events`. Quotes expose bid/ask price
and displayed size. Trades expose price, size, and buy, sell, or unknown aggressor side. Each event
records economic, availability, and receipt timestamps plus a positive ingest sequence. Replay
orders events by availability, receipt, and ingest sequence. Marketable orders consume only
displayed quote liquidity; passive orders require appropriately aggressed trade evidence, and an
unknown aggressor never fills them. Event capacity is shared deterministically across order
priority and fills retain the event's economic timestamp. Completed bars remain the valuation
boundary. The `quote-trade` batch, stream, and journal fixtures demonstrate equivalent replay.

Version 15 added `order_book_v1` and bounded level-two `order_book_events`. Every per-instrument
slice bundle starts with a complete snapshot and continues with contiguous absolute set, delete,
and aggressor-classified trade updates. Snapshots and updates reject crossed books, missing
deletes, sequence gaps, tick or lot misalignment, and depth beyond the configured limit; locked
books are valid. State is rebuilt from each slice snapshot, so replay never depends on hidden data
from a prior slice.

Marketable orders walk observable opposite-side depth in price priority. Passive limit orders join
behind displayed same-price quantity and earlier engine orders. Reductions decrease quantity ahead,
adds join behind, and only appropriately aggressed trades consume the queue and fill the order.
Partial fills and cancellations therefore remain deterministic. Book liquidity is independent of
bar and quote/trade execution semantics, while completed bars remain the valuation boundary. The
`order-book` batch, stream, and journal fixtures demonstrate cancellation, queue depletion, maker
fills, bounded state, and batch/stream equivalence.

Version 16 replaces string-only metrics with typed observations. Numeric values use canonical
decimal strings; string and boolean values retain their JSON types. Optional units, a closed
aggregation enum, and up to 16 unique dimensions are bounded at ingestion. Dimension keys are
sorted before journal encoding for deterministic downstream reconciliation.
