# Trading Engine contract v1

This frozen directory preserves the historical v1 process and file contract. The current runtime
emits and advertises v2 only; these artifacts remain available for provenance and compatibility
testing by older consumers.

- `scenario.schema.json` validates batch replay inputs.
- `scenario-stream.schema.json` validates each JSON Lines scenario-stream record.
- `journal.schema.json` validates each JSON Lines audit record.
- `fixtures/demo.scenario.json`, `fixtures/demo.scenario.jsonl`, and
  `fixtures/demo.journal.jsonl` form the canonical valid conformance corpus.

Every batch scenario, scenario-stream record, and journal record carries
`"contract_version": "1"`. Consumers must reject missing or unsupported versions before
interpreting the rest of a document.
