# Continuous integration

CI tests the supported OCaml and dependency range without creating a Cartesian product.

| Cell | Platform | Dependencies | Gate |
| --- | --- | --- | --- |
| `check` | Ubuntu | Repository lock | Full `make check` |
| `lowest-ubuntu` | Ubuntu | Oldest declared versions | Dependency-band check |
| `highest-ubuntu` | Ubuntu | Newest declared versions | Dependency-band check |
| `highest-macos` | macOS | Newest declared versions | Build and journal comparison |

Required Ubuntu jobs validate contract v1 schemas and fixtures, OCaml tests, protocol fuzzing,
deterministic journals, metadata, documentation, and benchmark smoke workloads. Coverage runs once
against the locked environment.

The required Persistra job uses a full pinned commit. A manually dispatched moving-head job is
informational and may fail without changing the supported revision pair.

Pull requests own feature-branch validation and cancel superseded runs. Push validation runs on
`develop` and release tags without cancellation so integration and publication evidence is
retained.
