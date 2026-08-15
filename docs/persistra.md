# Persistra integration

Persistra and Trading Engine should remain separate projects with a versioned process and file
boundary.

Persistra owns:

- Provider acquisition and raw caching
- Normalized historical observations
- Retrieval-time and source-native revisions
- Point-in-time features and labels
- Portfolio construction and vectorized research backtests
- Research manifests, analysis, and visualization

Trading Engine owns:

- Event sequencing and strategy callbacks
- Target-to-order conversion
- Pre-trade execution risk
- Order and fill state
- Execution simulation
- Exact cash, position, fee, and P&L accounting
- Execution audit artifacts

## Prototype handoff

The current handoff is the version 1 JSON scenario. A small Persistra-side adapter can:

1. Query normalized bars through Persistra's public store API.
2. Preserve provider-scoped instrument IDs or apply an explicit catalog mapping.
3. Supply tick size, lot size, currency, and execution eligibility metadata.
4. Convert research target weights into scheduled target quantities under an explicit sizing
   policy.
5. Record the scenario file hash in the Persistra research manifest.
6. Run the OCaml CLI as a separate process.
7. Import orders, fills, valuations, and risk decisions from the audit journal for analysis.

Do not let the engine read Persistra's internal DuckDB tables. Their schema and connection
lifecycle belong to Persistra.

## Time mapping

Preserve Persistra's temporal distinctions:

- Intraday UTC timestamps map to bar event times.
- Daily calendar labels require an explicit venue-calendar delivery policy. Do not convert them to
  midnight UTC silently.
- Provider as-of time remains source provenance.
- Persistra retrieval time remains acquisition provenance. It is not replay availability.
- Scenario `available_at` states when a strategy may use the completed bar.
- Scenario `received_at` states when this engine run receives it.

Use raw executable prices for fills. Adjusted values can feed strategy features, but splits and
dividends need explicit engine events before adjusted histories can support share-and-cash
accounting.

## Later columnar handoff

JSON is intentionally simple for the first vertical slice. Larger bar, quote, or order-book
histories should move to an immutable run bundle:

```text
run-bundle/
  manifest.json
  instruments.json
  market-events.parquet
  targets.parquet
```

Keep the manifest schema normative and hash every artifact. The OCaml reader can use a narrow
DuckDB C or Arrow IPC boundary without coupling to Persistra's database tables.
