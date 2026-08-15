# External strategy protocol v1

Version 1 is a synchronous JSON Lines protocol over child-process standard input and output.
Trading Engine sends `initialize`, ordered `event` requests, and `shutdown`. The strategy answers
with `ready`, `intents`, and `stopped`. It may answer any request with `error`.

Every message repeats `strategy_protocol_version: "1"` and a positive canonical
`strategy_sequence`. A response must repeat the sequence of its request. Only one request is
outstanding. Trading Engine rejects unknown or duplicate fields, invalid canonical values,
oversized lines, a wrong version or sequence, unexpected response types, EOF, timeout, and a
nonzero process exit.

The event context contains the replay clock, every cash ledger and configured position, all
working orders, and the latest available bar for each instrument. Event payloads cover completed
market slices, fills, order updates, and rejected intents. Response intents use the scenario v3
intent shapes.

External replay requires an empty batch schedule and empty streamed intent batches. The engine
records both directions in a deterministic transcript. The transcript and audit journal retain
partial files after failure and finalize only after their respective success checks.

- `message.schema.json` validates individual requests and responses.
- `transcript.schema.json` validates retained transcript records.
- `fixtures/external.scenario.json` is the batch replay fixture.
- `fixtures/external.scenario.jsonl` is its bounded-memory stream form.
- `fixtures/external.strategy.jsonl` is the canonical protocol transcript.
