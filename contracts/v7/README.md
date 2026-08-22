# Trading Engine contract v7

This directory is the authoritative v7 process and file contract shared by Trading Engine and its
clients. Versions 6, 5, 4, and 3 remain readable during their client transitions.

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
leverage, and initial-margin limits. Signed cash is valid. A successful v7 run emits `initial_state`
immediately after `run_started`, followed by a reconciled initial `valuation`, before market data.

Admission and fill clipping include working-order reservations. When multiple groups limit the same
fill, lexical group identity is the deterministic tie breaker. Valuations and strategy contexts
carry group exposure snapshots, and clipping thresholds identify the exact instrument or group.

Every v7 scenario, stream record, and journal record carries `"contract_version": "7"`.

The v7 `execution` object retains the versioned configuration introduced by v5.
`completed_bar_v1` configuration version `"1"` requires participation basis points, fixed fee,
and fee basis points. Runtime capabilities describe its required fields, supported market and limit
orders, completed-OHLCV data requirement, and numeric limits.
