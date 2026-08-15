# Agent instructions

Read [`CONTRIBUTING.md`](CONTRIBUTING.md) before making a change.

## Design

- Keep the deterministic reducer pure. Put clocks, files, networking, and broker effects at
  the boundary.
- Use opaque domain types and checked arithmetic for executable prices, quantities, and cash.
- Preserve event time, availability time, receipt time, and ingest order as distinct concepts.
- Keep historical replay, paper trading, and live trading behind the same source and venue
  boundaries.
- Prefer the simplest implementation that satisfies the current contract. Remove obsolete
  paths instead of preserving compatibility.
- Use short, active sentences and plain American English in documentation.

## Workflow

- Interview before a new feature, refactor, or design decision. Confirm scope, API, edge cases,
  and tests before writing code.
- Run `make check` before each logical commit.
- Use conventional, subject-only commits. Do not add trailers or AI attribution.
- Do not push, tag, publish, change a version, or merge without direct user approval.
