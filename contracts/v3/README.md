# Trading Engine contract v3

This directory is the authoritative v3 process and file contract shared by Trading Engine and
its clients. Version 2 remains frozen under `contracts/v2`.

- `scenario.schema.json` validates batch replay inputs.
- `scenario-stream.schema.json` validates each JSON Lines scenario-stream record.
- `journal.schema.json` validates each JSON Lines audit record.
- The files under `fixtures/` form the canonical valid conformance corpus.

Version 3 adds exact six-decimal quantities, signed targets and positions, explicit per-currency
cash and FX marks, splits and cash dividends, borrow costs, exposure and margin policy, and causal
margin-call/liquidation events. Every v3 scenario, stream record, and journal record carries
`"contract_version": "3"`; consumers reject missing or unsupported versions before interpreting
the remainder of a document.
