# Persistra integration

Persistra and Trading Engine communicate only through versioned files and processes.

Persistra owns provider ingestion, normalized point-in-time observations, research, portfolio
construction, and run manifests. Trading Engine owns causal sequencing, risk, orders, simulated
execution, financing, settlement, accounting, and audit journals. The engine never reads
Persistra's internal database.

## Handoff

Persistra produces scenario contract v1 batch or stream input and should:

1. Preserve stable executable-instrument identities and provider provenance.
2. Supply complete tick, lot, currency, calendar, risk, execution, financing, and settlement
   configuration.
3. Build ordered synchronized slices with explicit availability and receipt times.
4. Preserve target weights instead of pre-sizing them outside the engine.
5. Validate schemas and run `--validate-only` before accepting a replay.
6. Require v1 in the engine's advertised scenario and journal capabilities.
7. Retain the exact scenario hash and require one terminal `run_completed` record.
8. Reconcile journal cash, positions, exposure, P&L, fees, margin, and causal references.

External strategies use [strategy protocol v1](../contracts/strategy/v1/README.md). Persistra must
retain the scenario, transcript, journal, executable identity, input hashes, and run manifest as one
bound artifact set.

## Compatibility gate

Compatibility means the v1 wire contracts pass against an explicit pair of repository commits.
The required `persistra-compatibility` CI job pins the complete Persistra revision in
`.github/workflows/ci.yml`; Persistra owns the reciprocal Trading Engine pin. Neither repository
silently follows a moving branch for its required gate.

Advance a pin only after both exact checkouts pass their native and cross-repository suites. When
the shared boundary changes, update schemas, fixtures, documentation, and pins together. The
optional moving-head canary is informational and does not change the supported baseline.

## Time and data rules

Use raw prices for execution. Adjusted data may feed research features, but splits and
distributions must be explicit engine events. Provider as-of and Persistra retrieval times remain
provenance; `available_at` and `received_at` define replay causality.

Use the JSON Lines scenario stream for larger histories. It remains an immutable file boundary,
not a database coupling.
