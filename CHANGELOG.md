# Changelog

## Unreleased

- Add explicit GTC, IOC, FOK, DAY, and GTD order lifetimes plus completed-bar stop and stop-limit
  activation in scenario contract v8 and external strategy protocol v6.

## Unreleased

- Add contract v7 exact per-instrument risk policies, versioned overlapping exposure groups,
  reservation-aware admission and fill clipping, group diagnostics, and strategy protocol v5.

- Add contract v6 explicit initial portfolio snapshots with signed cash and positions, accounting
  history, initial marks and FX, strict risk validation, initial-state auditing, and strategy
  protocol v4 initialization.
- Publish strict versioned configuration and machine-readable capabilities for each compiled
  execution model.
- Add contract v5 venue calendars with explicit venue and calendar identities, regular and
  extended trading phases, holidays, early closes, and reducer-independent clock resolution.
- Protect `main` with required integration checks and a no-bypass review policy, and make rebase
  merging the only supported repository merge mode.
- Establish a security baseline with private reporting guidance, grouped dependency proposals,
  dependency review, and CodeQL analysis for workflows and Python tooling.
- Add structured issue and pull-request intake, reviewed planning-label and repository metadata,
  explicit compatibility guarantees, a pinned required Persistra baseline, and a manual
  nonrequired latest-head signal.
- Verify exact canonical journal bytes across locked, dependency-bound, and operating-system CI
  cells, with safe concurrency cancellation and documented required versus informational gates.
- Publish one strict documentation site for architecture, versioned contracts, and generated OCaml
  APIs, with offline topology checks and bounded external-link validation.
- Define a reproducible release-candidate artifact set with install verification, checksums, SPDX
  inventory, SLSA provenance, and a manual tag-only signing boundary.

## 1.0.0 — 2026-08-21

- Release the deterministic completed-bar execution engine with exact checked arithmetic,
  causal audit journals, and versioned JSON and JSON Lines contracts.
- Support portfolio targets, direct orders, partial fills, fees, multi-currency accounting,
  corporate actions, borrow costs, margin controls, and deterministic liquidation.
- Add synchronous external strategies through protocol v3 with current-slice callback state and
  strategy responses applied before matching continues.
- Provide strict schemas, conformance fixtures, replay validation, and Persistra compatibility
  checks.
