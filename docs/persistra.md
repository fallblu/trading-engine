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
- One explicit executable-instrument catalog
- Risk, participation, and fee policies
- Strictly increasing synchronized market slices
- Scheduled full-portfolio weight or quantity targets
- Optional direct orders, cancellations, and metrics

Persistra should:

1. Query normalized raw executable bars through its public store API.
2. Preserve provider-scoped instrument IDs or apply an explicit catalog mapping.
3. Supply tick, lot, currency, availability, and receipt policies.
4. Group one bar per instrument into each synchronized slice.
5. Preserve original portfolio weights in `target_weights` instead of pre-sizing them.
6. Populate `metadata` with dataset, policy, and build provenance.
7. Read `--capabilities` and require support for the scenario and journal contract version.
8. Validate the scenario through the JSON Schema and `--validate-only`.
9. Run the CLI as a separate process and import its audit journal.
10. Require the same contract version on every journal record.
11. Verify the same scenario SHA-256 in `run_started` and `run_completed`.
12. Require the terminal completion record before accepting a replay.

Do not let the engine read Persistra's internal DuckDB tables. Their schema and connection
lifecycle belong to Persistra.

Use the committed v1 [scenario](../contracts/v1/scenario.schema.json) and
[journal](../contracts/v1/journal.schema.json) JSON Schemas and their adjacent conformance fixtures
for structural checks. The engine parser is authoritative for ordering, catalog coverage,
causality, tick, lot, risk, and accounting invariants that JSON Schema cannot express.

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
