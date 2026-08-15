# Trading Engine contract v1

This directory is the authoritative process and file contract shared by Trading Engine and its
clients.

- `scenario.schema.json` validates batch replay inputs.
- `scenario-stream.schema.json` validates each JSON Lines scenario-stream record.
- `journal.schema.json` validates each JSON Lines audit record.
- `fixtures/demo.scenario.json`, `fixtures/demo.scenario.jsonl`, and
  `fixtures/demo.journal.jsonl` form the canonical valid conformance corpus.

Every batch scenario, scenario-stream record, and journal record carries
`"contract_version": "1"`. Consumers must reject missing or unsupported versions before
interpreting the rest of a document.
