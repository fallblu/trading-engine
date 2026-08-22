# Contributing

Install `opam`, `uv`, and Python 3, then bootstrap the repository-local development
environment:

```sh
make bootstrap
```

The command creates or updates only the repository-local opam switch and
`.venv-schema`. It installs the locked OCaml dependencies and the fully pinned JSON
Schema validator environment. It is safe to run again after either lock changes.

Check an existing environment without changing it:

```sh
make environment-check
```

The check reports missing tools, a missing or incorrect local opam switch, stale
locked dependencies, and an incomplete schema environment with a suggested repair.

Run the complete local gate before committing:

```sh
make check
```

The gate includes the fixed protocol-fuzzing smoke corpus. For longer deterministic
campaigns and reproduction controls, see [Protocol fuzzing](docs/fuzzing.md).
Reducer model properties also print reproducible seeds and shrink failures into scenario-like
traces; see [Reducer property testing](docs/reducer-property-testing.md).

Run `make coverage` to enforce the OCaml coverage floor and generate per-module, control-flow
HTML, and Cobertura reports. See [OCaml coverage](docs/coverage.md) for report locations,
instrumentation scope, and the explained-threshold-change policy.

CI additionally resolves the lowest and highest supported dependency bands and compares canonical
journal bytes on Linux and macOS. See [Continuous integration](docs/continuous-integration.md) for
the required and informational cells. Use `make dependency-band-check` only after bootstrapping a
nonlocked CI band; normal development continues to use `make check` and the exact lock.

Run `make docs-build` to create the strict local site under `site/`. It installs only the locked
documentation tools in `.venv-docs`, stages versioned contracts without modifying them, generates
the public OCaml interfaces, and validates the complete output. See the
[documentation platform](docs/documentation-platform.md) for publication and link-checking policy.

Run `make release-check` only from a clean tracked revision to reproduce the complete candidate
artifact set twice and verify its install. This never tags or publishes. See
[release artifacts and provenance](docs/release-artifacts.md) for the human approval boundary.

The gate formats a copy check, builds every target, and runs all tests. Keep commits small,
coherent, and working. Use subject-only conventional commit messages such as
`feat: implement deterministic order matching`.

## Git workflow

Create feature branches from `develop` and open pull requests back into `develop`. Use
rebase-and-merge so every coherent commit remains visible; do not use squash or merge commits.
GitHub deletes merged head branches automatically, so verify that the branch is gone afterward.

Promotion to `main` also uses a pull request and rebase merge. The protected branch requires a head
that is current with `main`, resolved review conversations, and successful `check` and
`persistra-compatibility` jobs. It blocks force pushes and branch deletion and applies to
administrators without a bypass. The rule requires no approval while the repository has one
maintainer, avoiding a self-review deadlock. See [Repository governance](docs/repository-governance.md)
for the complete policy.

Do not add secrets, provider credentials, or customer account data to fixtures or journals.
Report suspected vulnerabilities through the private channel in the
[security policy](.github/SECURITY.md), not through a public issue.

## Intake and planning metadata

Use the structured bug, feature, contract-change, or cross-repository issue form. Pull requests
retain the `Summary` and `Test plan` sections from the repository template.

Component, contract-version, and dependency labels describe stable scope. Priority and effort
labels are assigned only during explicit triage; they do not promise a release, date, or roadmap
position. Do not encode delivery commitments in labels. The reviewed label definitions and desired
repository metadata live under `.github/` and must agree with the GitHub settings.

For reciprocal Persistra compatibility guarantees and the pin-advancement procedure, read
[Persistra integration](docs/persistra.md#compatibility-guarantees).
