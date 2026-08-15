# Trading Engine contract v1

This directory is the authoritative process and file contract shared by Trading Engine and its
clients.

- `scenario.schema.json` validates replay inputs.
- `journal.schema.json` validates each JSON Lines audit record.
- `fixtures/demo.scenario.json` and `fixtures/demo.journal.jsonl` form the canonical valid
  conformance pair.

Every scenario and every journal record carries `"contract_version": "1"`. Consumers must reject
missing or unsupported versions before interpreting the rest of a document.
