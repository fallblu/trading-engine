# Diagnostic contract v1

This directory defines the stable JSON emitted on standard error when the CLI uses
`--diagnostic-format json`. Validate each complete document against
[`diagnostic.schema.json`](diagnostic.schema.json).

The `code` and typed `context` fields are the machine contract. Treat `message`, cause messages,
and human rendering as explanatory text. Unknown context is omitted. Diagnostics never retain an
input record, strategy response, or unrelated payload value.

Adding a code or optional context field is compatible within version 1. Removing a code, changing
a field type, or changing a code's meaning requires a new diagnostic contract version.
