# Trading Engine contract v8

This directory is the authoritative v8 process and file contract shared by Trading Engine and its
clients. Versions 7, 6, 5, 4, and 3 remain readable during their client transitions.

- `scenario.schema.json` validates batch replay inputs.
- `scenario-stream.schema.json` validates each JSON Lines scenario-stream record.
- `journal.schema.json` validates each JSON Lines audit record.
- The files under `fixtures/` form the canonical valid conformance corpus.
- `fill-clipped.scenario.json` and its journal exercise a leverage-limited partial fill.

Version 8 requires exactly one explicit risk policy per catalog instrument. Each policy defines
order, signed-position, notional, initial-margin, maintenance-margin, and shorting limits. Versioned
risk groups have explicit membership, may overlap, and can constrain gross, long, short, absolute
net, and gross-to-equity concentration exposure.

Runtime validation requires exact currency, position-mark, and FX coverage; known instruments;
lot-aligned quantities; tick-aligned positive marks; basis with the same sign as quantity;
nonnegative fee histories; the base FX rate equal to one; instrument, group, aggregate exposure,
leverage, and initial-margin limits. Signed cash is valid. A successful v8 run emits `initial_state`
immediately after `run_started`, followed by a reconciled initial `valuation`, before market data.

Admission and fill clipping include working-order reservations. When multiple groups limit the same
fill, lexical group identity is the deterministic tie breaker. Valuations and strategy contexts
carry group exposure snapshots, and clipping thresholds identify the exact instrument or group.

Every v8 scenario, stream record, and journal record carries `"contract_version": "8"`.

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

The v8 `execution` object retains the versioned configuration introduced by v5.
`completed_bar_v1` configuration version `"1"` requires participation basis points, fixed fee,
and fee basis points. Runtime capabilities describe its required fields, supported market and limit
orders (including stop and stop-limit), completed-OHLCV data requirement, and numeric limits.
