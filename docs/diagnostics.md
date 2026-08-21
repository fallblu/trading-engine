# Diagnostics

Process and file boundaries return diagnostic contract version `1`. The CLI prints the concise
`message` by default. Pass `--diagnostic-format json` to write one machine-readable diagnostic to
standard error. The process exits with status 123 for either format.

Every JSON diagnostic contains:

| Field | Type | Meaning |
| --- | --- | --- |
| `diagnostic_version` | string | Diagnostic contract version |
| `code` | string | Stable machine classification |
| `phase` | string | `cli`, `input`, `validation`, `replay`, `reducer`, `strategy`, or `artifact` |
| `message` | string | Concise human description; clients must not parse it |
| `context` | object | Known location and causality fields |
| `cause` | object or null | Sanitized underlying exception |

Context fields are omitted when unknown. `json_path`, `event_id`, and `order_id` are strings;
`line` is a JSON integer; `sequence` is a canonical int64 string; and `causation_ids` is an ordered
array of event ID strings. A cause contains `kind` and `message`, plus `operation` and `target` for
Unix errors. Diagnostics retain no input record, strategy message, or unrelated payload data.

Version 1 defines these codes:

| Code | Meaning |
| --- | --- |
| `cli.invalid_arguments` | Runtime option combination is invalid |
| `input.io` | Input open, read, or hash operation failed |
| `scenario.invalid_json` | Scenario or strategy JSON syntax is invalid |
| `scenario.invalid` | Batch scenario validation failed |
| `scenario.unsupported_contract` | Scenario contract version is unsupported |
| `scenario_stream.invalid` | Stream envelope, ordering, or payload validation failed |
| `scenario_stream.changed` | Stream bytes changed between validation and replay |
| `replay.failed` | Replay orchestration invariant failed |
| `reducer.failed` | Pure engine processing rejected the requested transition |
| `strategy.invalid_configuration` | Strategy command or timeout is invalid |
| `strategy.protocol` | Strategy exchange violated the protocol |
| `strategy.timeout` | Strategy exchange or shutdown exceeded its deadline |
| `strategy.process` | Strategy spawn, signaling, supervision, or process I/O failed |
| `strategy.exit` | Strategy exited with an unsuccessful status |
| `artifact.exists` | A final or partial artifact path already exists |
| `artifact.io` | Artifact creation, append, close, or publication failed |
| `artifact.state` | Artifact writer lifecycle operation is invalid |

Adding codes or optional context fields does not change the diagnostic version. Removing a code,
changing a field type, or changing a code's meaning requires a new version.
