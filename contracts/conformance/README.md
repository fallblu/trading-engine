# Contract conformance corpus

This directory is the machine-readable entry point for contract consumers.

- `manifest.json` maps every versioned schema branch to its canonical fixtures.
- `cases.json` records deterministic schema/runtime differential cases for the
  versions accepted by the current OCaml runtime.
- `frozen.sha256` protects the schema and fixture bytes for archived scenario
  contract v1 and v2 and strategy protocol v1 and v2.

The artifact manifest treats each schema at each version as a separate branch.
Every branch has positive canonical inputs and generated negative cases for a
missing version, an unsupported version, and an unknown field. Frozen branches
are schema-only: they are never passed to current runtime parsers.

Every top-level `oneOf` alternative also has a positive witness and a derived
unknown-field rejection. `schema_only_cases` supplies variants that do not occur
in a successful canonical run, such as strategy `error` messages and rejected
response transcript records.

Differential cases label rules as `structural` or `semantic`. Structural cases
must produce the same result from JSON Schema and the OCaml parser. Semantic
cases explicitly document invariants that JSON Schema cannot express, so an
accepted schema result and a rejected runtime result is intentional.

Run the complete corpus through the repository gate:

```sh
make check
```

When intentionally changing an archived contract, update `frozen.sha256` in the
same review. Adding a contract version requires a manifest branch, canonical
fixtures, positive and negative validation, and current-runtime differential
cases when the new version is advertised by the engine.
