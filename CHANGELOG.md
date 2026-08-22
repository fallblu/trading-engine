# Changelog

## Unreleased

- Add conservative next-open and adverse-touch completed-bar execution models with strict fixed
  spread and linear participation-impact configuration, explicit missing-volume policy,
  tick-aligned prices, and separate price-component audit attribution.
- Publish scenario/journal contract v13 and external strategy protocol v11 while preserving v12
  and protocol v10 as frozen compatibility contracts.

- Add exact stock-dividend, rights, and spin-off distributions with explicit basis allocation,
  fractional rejection or cash-in-lieu policy, destination currency validation, target adjustment,
  and complete journal attribution.
- Add stable-identity instrument lifecycle state for halt, resume, identifier/provider remapping,
  expiration, and delisting with deterministic order cancellation and explicit terminal hold or
  cash-out policy.
- Publish scenario/journal contract v12 and external strategy protocol v10 while preserving v11
  and protocol v9 as frozen compatibility contracts.
- Add deterministic trade-date and settlement-date accounting, versioned business-date settlement
  calendars, settled and unsettled cash and position attribution, explicit settlement buying-power
  policies, and auditable settlement completion and failure events.
- Publish scenario/journal contract v11 and external strategy protocol v9 while preserving v10 and
  protocol v8 as frozen compatibility contracts.
- Add effective-time borrow availability, signed rates, locate clipping or rejection, recalls,
  deterministic close-outs, and explicit missing-data behavior.
- Add effective-time currency credit/debit rates with Actual/365 or Actual/360 day count, simple or
  daily compounding, deterministic cash-ledger entries, and realized P&L attribution.
- Publish scenario/journal contract v10 and external strategy protocol v8 while preserving v9 and
  protocol v7 as frozen compatibility contracts.
- Added instrument-aware, composable fee schedules with named fixed, notional, and per-unit
  components; explicit rounding; maker/taker applicability; per-fill minimums and caps; rebates;
  and deterministic multi-currency conversion.
- Added signed fee-component attribution to fills, positions, valuations, journals, and external
  strategy events in scenario/journal contract v9 and strategy protocol v7.
- Preserved completed-bar configuration v1, scenario/journal v8, and strategy protocol v6 as
  compatibility contracts.

- Add explicit GTC, IOC, FOK, DAY, and GTD order lifetimes plus completed-bar stop and stop-limit
  activation in scenario contract v8 and external strategy protocol v6.

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
