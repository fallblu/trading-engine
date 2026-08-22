# External strategy protocol v12

Version 12 is a synchronous JSON Lines protocol over child-process standard input and output.
Trading Engine sends `initialize`, ordered `event` requests, and `shutdown`. The strategy answers
with `ready`, `intents`, and `stopped`. It may answer any request with `error`.
Protocol v11 remains available for scenario contract v13; earlier versions retain their frozen
shapes.

Every message repeats `strategy_protocol_version: "12"` and a positive canonical
`strategy_sequence`. A response must repeat the sequence of its request. Only one request is
outstanding. Trading Engine rejects unknown or duplicate fields, invalid canonical values,
oversized lines, a wrong version or sequence, unexpected response types, EOF, timeout, and a
nonzero process exit.

The event context contains the replay clock, a marked base-currency portfolio, deterministic group
exposure snapshots, all working orders, and the latest available bar for each instrument. Every
callback emitted for a market slice uses
that slice's `received_at` as `now` and uses its complete bars and FX vector. The portfolio reports
cash, equity, net, long, short, and gross market value plus every attributed cash ledger and
configured position. Position quantities and weights reflect applied fills. Weights are truncated
toward zero to six decimal places. `weights_available` is false and all weights are null when
equity is zero or negative.

The `initialize` request identifies scenario contract v14 and includes the exact `initial_portfolio`
snapshot alongside the legacy cash projection. It also carries the complete versioned venue
calendars, nested execution configuration, financing policy, and settlement policy, so a strategy
can construct DAY orders and reject incompatible execution, financing, or settlement state before
replay.

Matching pauses after each strategy callback. The engine applies the response against the exact
account and OMS state exposed by that callback before delivering another callback or considering
the next eligible order. Later same-slice contexts include the effects of earlier responses. The
eligible-order sequence is fixed at the start of matching, so newly submitted orders wait for a
later slice. Cancelling an order before its turn leaves its unused slice capacity available to the
next eligible order.

Event payloads cover completed market slices with effective-time borrow and cash-rate observations
plus explicit settlement failures, fills, order updates, and rejected intents. Portfolio contexts
include cash-interest attribution and settled and unsettled cash and position quantities. Response
intents use the scenario v14 intent shapes. Market-slice events include lifecycle transitions and
the expanded corporate-action catalog, plus causally ordered quote/trade market events.

External replay requires an empty batch schedule and empty streamed intent batches. The engine
records accepted messages in both directions in a deterministic transcript. A response rejected
for invalid JSON, fields, version, sequence, EOF, or size is never stored as an accepted exchange.
Instead, the partial transcript ends with a `rejected_strategy_response` diagnostic record. Version
1 rejection diagnostics use the shared
[`diagnostic/v1`](../../diagnostic/v1/README.md) contract. The transcript schema narrows that
contract to the `strategy.protocol` and `resource.limit` codes in the `strategy` phase. The record
includes the structured rejection diagnostic and at most the first 256 raw response bytes encoded
as lowercase hexadecimal. `observed_bytes` counts bytes available when the engine rejected the
response, and `truncated` reports whether the prefix omits observed bytes. The transcript and audit
journal retain partial files after failure and finalize only after their respective success checks.

- `message.schema.json` validates individual requests and responses.
- `transcript.schema.json` validates accepted exchanges and rejected-response diagnostics.
- `fixtures/external.scenario.json` is the batch replay fixture.
- `fixtures/external.scenario.jsonl` is its bounded-memory stream form.
- `fixtures/external.strategy.jsonl` is the canonical protocol transcript.
