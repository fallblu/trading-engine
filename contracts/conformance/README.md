# Contract conformance corpus

This directory is the machine-readable entry point for contract consumers.

- `manifest.json` maps each current v1 schema to canonical fixtures.
- `cases.json` defines structural and semantic differential cases.

Structural cases must agree between JSON Schema and the OCaml runtime. Semantic cases document
rules that JSON Schema cannot express, such as ordering, uniqueness, and cross-record invariants.

Run `make check` to validate the complete corpus. A contract change must update its schema,
fixtures, manifest entries, differential cases, and runtime checks together.
