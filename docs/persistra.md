# Persistra integration

Persistra and Trading Engine remain separate projects behind a strict process and file boundary.

Persistra owns provider data, normalized observations, revisions, point-in-time research,
portfolio construction, manifests, analysis, and visualization. Trading Engine owns causal event
sequencing, target sizing, risk, order and fill state, execution simulation, exact accounting, and
execution audit artifacts.

## Handoff

The JSON scenario carries:

- Required `contract_version` identifying the scenario and journal protocol
- Required producer metadata preserved as JSON but ignored by execution
- Required compiled execution-model selection
- One explicit executable-instrument catalog
- Signed position, exposure, leverage, margin, borrow, participation, and fee policies
- Strictly increasing synchronized market slices with complete FX marks and corporate actions
- Explicit initial cash ledgers for every base or quote currency
- Optional scheduled full-portfolio signed weight or fractional quantity targets
- Optional direct orders, cancellations, and metrics

Persistra should:

1. Query normalized raw executable bars through its public store API.
2. Preserve provider-scoped instrument IDs or apply an explicit catalog mapping.
3. Supply tick, lot, currency, availability, and receipt policies.
4. Group one bar per instrument into each synchronized slice.
5. Preserve original portfolio weights in `target_weights` instead of pre-sizing them.
6. Populate `metadata` with dataset, policy, and build provenance.
7. Read `--capabilities` and require support for the scenario, journal, and optional strategy
   protocol versions.
8. Validate the scenario through the JSON Schema and `--validate-only`.
9. Run the CLI as a separate process and import its audit journal.
10. Require the same contract version and deterministic run-scoped event-ID derivation on every
    journal record.
11. Reject duplicate, unknown, forward, cross-run, or noncanonical causal references.
12. Verify the same scenario SHA-256 and selected execution model in `run_started`,
    `run_completed`, and the retained manifest.
13. Reconcile every native/base position row and currency cash row to the aggregate account,
    exposure, fee, and margin values.
14. Reconcile split adjustments, dividends, borrow fees, risk-limited fills, and margin
    liquidation against scenario and runtime state.
15. For external replay, require an empty schedule, launch an explicit strategy argument vector,
    hash every declared strategy input, and validate the complete bidirectional transcript.
16. Reconcile external transcript intents to their journal outcomes.
17. Require the terminal completion record before accepting a replay.

Do not let the engine read Persistra's internal DuckDB tables. Their schema and connection
lifecycle belong to Persistra.

Use the current v3 [scenario](../contracts/v3/scenario.schema.json) and
[journal](../contracts/v3/journal.schema.json) JSON Schemas and their adjacent conformance fixtures
for structural checks. The engine parser is authoritative for ordering, catalog coverage,
causality, tick, lot, risk, and accounting invariants that JSON Schema cannot express.

External strategies use the separate
[strategy protocol v2](../contracts/strategy/v2/README.md). Persistra's host turns protocol
initialization, marked portfolio contexts, market-slice, fill, order, and rejection events into
typed callbacks. Realized weights are available only for positive equity. The retained run
manifest binds the strategy identity, executable hash, declared input hashes, transcript hash,
scenario hash, and journal hash. Strategy standard output remains protocol-only; logs and
diagnostics use standard error.

## Time mapping

- Intraday UTC timestamps map to slice event times.
- Daily labels require an explicit venue-calendar delivery policy.
- Provider as-of time remains source provenance.
- Persistra retrieval time remains acquisition provenance, not replay availability.
- `available_at` states when a strategy may use the complete synchronized slice.
- `received_at` states when the engine run observes it.
- A scheduled order-changing intent must arrive no later than the next slice start.

Use raw prices for execution. Adjusted values can feed features, but splits and dividends require
explicit engine events before adjusted histories can support share-and-cash accounting.

## Larger artifacts

JSON is suitable for small and moderate scenarios. Use the versioned JSON Lines scenario stream
for larger histories. It carries one static header, one slice with its causally adjacent intents
per record, and a required terminal count. The engine validates and replays it with bounded input
and audit memory. Persistra should retain and hash that immutable stream beside the journal and
run manifest. The stream remains a file boundary; it does not couple the engine to Persistra's
database tables.
