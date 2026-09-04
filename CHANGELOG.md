# Changelog

## Unreleased

## [1.1.1] - 2026-09-04

- Make development bootstrap repair a stale globally registered coverage repository URL before
  selecting it for the repository-local switch.

## [1.1.0] - 2026-08-26

- Reset the scenario, journal, strategy, execution-configuration, CLI, and diagnostic contracts to
  one authoritative v1 surface.
- Remove historical schemas, compatibility dispatch, deprecated constructors, frozen fixtures,
  and obsolete compatibility tests.
- Require explicit initial portfolios, current risk and fee policies, complete market slices, and
  current strategy initialization.
- Simplify repository and contract documentation around the current engine boundary.
