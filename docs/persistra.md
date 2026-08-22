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
- Explicit signed initial cash and positions with accounting history, marks, and FX state
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

Persistra currently uses the transitional v3
[scenario](../contracts/v3/scenario.schema.json) and
[journal](../contracts/v3/journal.schema.json) schemas and their adjacent conformance fixtures for
structural checks. The engine advertises current contract v7 while retaining v6, v5, v4, and exact v3
journal output for v3 inputs. The engine parser is authoritative for ordering, catalog coverage,
causality, tick, lot, risk, and accounting invariants that JSON Schema cannot express.

External strategies use the separate
[strategy protocol v5](../contracts/strategy/v5/README.md). Persistra's host turns protocol
initialization, marked portfolio contexts, market-slice, fill, order, and rejection events into
typed callbacks. Realized weights are available only for positive equity. The retained run
manifest binds the strategy identity, executable hash, declared input hashes, transcript hash,
scenario hash, and journal hash. Strategy standard output remains protocol-only; logs and
diagnostics use standard error.

Persistra must answer each callback before the engine continues matching. Every callback for a
slice uses the slice receipt time and complete bar and FX snapshot. A later callback therefore
includes accepted intents returned from an earlier callback at that same replay clock.

## Compatibility guarantees

Compatibility is defined by versioned wire contracts and an explicitly tested pair of repository
revisions. A branch name, package version, or successful build in only one repository is not a
compatibility claim.

- **Engine:** `--capabilities` is the authoritative machine-readable surface. The engine must
  reject unsupported versions and malformed or semantically invalid input before reporting a
  successful run.
- **Scenario:** Frozen scenario and stream artifacts do not change. The current v7 contract may
  receive additive changes only when old valid inputs retain their meaning; breaking changes need
  a new version. Transitional v3 support remains explicit in `--capabilities`.
- **Journal:** A run emits the journal version paired with its accepted scenario. Record ordering,
  causal references, scenario hashing, terminal completion, and exact accounting remain runtime
  invariants even when JSON Schema cannot express them.
- **Strategy:** Protocol and transcript versions are independent of scenario versions. The current
  external boundary is strategy v4; a host must complete its exact initialization, event,
  shutdown, timeout, and rejection lifecycle.
- **Persistra:** The required integration gate uses a full Persistra commit and its v3 scenario,
  journal, and strategy integration tests. Passing that gate claims compatibility only for the
  recorded revision pair and advertised versions.

The required `persistra-compatibility` job pins the full Persistra commit stored as
`PERSISTRA_COMPAT_REVISION` in `.github/workflows/ci.yml`. It asserts the resolved checkout and
writes the SHA to the log and job summary. It never follows a repository variable or moving branch.
Persistra owns the reciprocal required pin to a reviewed Trading Engine commit.

To advance either baseline, the repository changing its pin selects a green full commit from the
other repository, builds both exact checkouts, runs the cross-repository integration suite, and
updates the one workflow SHA in a reviewed pull request. When a contract or host/runtime behavior
changes, both repositories update their fixtures, documentation, and pins in dependency order.
Neither repository silently advances the other's required baseline.

Maintainers can manually dispatch CI with `persistra_latest_head` enabled to test Persistra
`develop`. The `persistra-latest-head` job is nonrequired and allowed to fail, so it provides an
early signal without changing the reproducible baseline or blocking an unrelated engine change.

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
