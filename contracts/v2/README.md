# Trading Engine contract v2

This frozen directory preserves the historical v2 process and file contract. The current runtime
emits and advertises v3 only; these artifacts remain available for provenance and compatibility
testing by older consumers.

- `scenario.schema.json` validates batch replay inputs.
- `scenario-stream.schema.json` validates each JSON Lines scenario-stream record.
- `journal.schema.json` validates each JSON Lines audit record.
- `fixtures/demo.scenario.json`, `fixtures/demo.scenario.jsonl`, and
  `fixtures/demo.journal.jsonl` form the canonical valid conformance corpus.

Every batch scenario, scenario-stream record, and journal record carries
`"contract_version": "2"`. Consumers must reject missing or unsupported versions before
interpreting the rest of a document.

Version 2 adds an explicit scenario execution-model selection. Every audit record also carries a
deterministic `event_id` and an ordered list of prior `causation_ids`. Valuations contain
per-instrument position attribution that reconciles exactly to their aggregate market value, cost
basis, realized and unrealized P&L, and fees.
