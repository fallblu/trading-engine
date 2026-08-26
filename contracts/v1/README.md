# Replay contract v1

This directory is the authoritative replay contract for Trading Engine.

- `scenario.schema.json` defines batch replay input.
- `scenario-stream.schema.json` defines the equivalent JSON Lines stream.
- `journal.schema.json` defines append-only audit records.
- `fixtures/` contains canonical scenarios and journals used by the conformance suite.

Every scenario, stream record, and journal record carries `"contract_version": "1"`.
Objects are strict unless a field is explicitly open, decimal values use canonical strings, and
timestamps are bounded RFC 3339 instants. Runtime validation additionally enforces uniqueness,
causal ordering, non-overlapping slices, resource limits, and configuration coverage.

The contract models an explicit initial portfolio, instrument-level and grouped risk policy,
execution and fee schedules, financing, settlement, venue calendars, lifecycle events, market
data, strategy intents, and causal audit output. A successful replay ends with `run_completed`.

Run `make check` to validate schemas, fixtures, runtime behavior, and deterministic output.
