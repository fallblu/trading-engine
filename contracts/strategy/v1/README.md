# External strategy protocol v1

Trading Engine supervises external strategies over synchronous JSON Lines on standard input and
output. `message.schema.json` defines protocol messages and `transcript.schema.json` defines the
durable exchange log.

Every message carries `"strategy_protocol_version": "1"` and a positive decimal-string sequence.
The engine sends `initialize`, ordered `event` messages, and `shutdown`; the strategy replies with
`ready`, matching `intents`, and `stopped`. The runtime enforces direction, sequence pairing,
canonical values, message limits, and lifecycle order.

Initialization binds the strategy to replay contract v1, the scenario hash, initial portfolio,
instruments, risk, execution, financing, settlement, venue calendars, and metadata. Event contexts
include the current portfolio, working orders, bars, and group exposures.

The fixtures contain a complete accepted session. Run `make check` to validate both schemas and
runtime behavior.
