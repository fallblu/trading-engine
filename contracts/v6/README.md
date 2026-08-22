# Trading Engine contract v6

This directory is the authoritative v6 process and file contract shared by Trading Engine and its
clients. Versions 5, 4, and 3 remain readable during their client transitions.

- `scenario.schema.json` validates batch replay inputs.
- `scenario-stream.schema.json` validates each JSON Lines scenario-stream record.
- `journal.schema.json` validates each JSON Lines audit record.
- The files under `fixtures/` form the canonical valid conformance corpus.
- `fill-clipped.scenario.json` and its journal exercise a leverage-limited partial fill.

Version 6 replaces cash-only initialization with an explicit portfolio snapshot. It carries signed
cash, signed positions, native cost basis, realized and dividend P&L histories, execution and
borrow fee histories, position marks, and currency-to-base FX marks. Historical attribution is
point-in-time state and is not applied to cash again.

Runtime validation requires exact currency, position-mark, and FX coverage; known instruments;
lot-aligned quantities; tick-aligned positive marks; basis with the same sign as quantity;
nonnegative fee histories; the base FX rate equal to one; position limits; aggregate exposure and
leverage limits; and initial margin. Signed cash is valid. A successful v6 run emits `initial_state`
immediately after `run_started`, followed by a reconciled initial `valuation`, before market data.

Version 6 retains the immutable venue-calendar and model-owned execution-configuration envelopes
introduced by v5.

Every v6 scenario, stream record, and journal record carries `"contract_version": "6"`.

The v6 `execution` object namespaces strict configuration beneath the stable model name.
`completed_bar_v1` configuration version `"1"` requires participation basis points, fixed fee,
and fee basis points. Runtime capabilities describe its required fields, supported market and limit
orders, completed-OHLCV data requirement, and numeric limits.
