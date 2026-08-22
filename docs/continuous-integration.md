# Continuous integration

CI tests a small, explicit environment matrix instead of an accidental Cartesian product. The
public package bounds in `trading_engine.opam` define supported dependencies. The repository lock
defines the reproducible development baseline.

| Cell | Operating system | OCaml | Dependencies | Gate | Status |
| --- | --- | --- | --- | --- | --- |
| `check` | Ubuntu latest | 5.5.0 | Exact lock | Full `make check` | Required |
| `lowest-ubuntu` | Ubuntu latest | 5.5.0 | Oldest solver-valid versions inside declared bounds | Full dependency-band check | Required |
| `highest-ubuntu` | Ubuntu latest | 5.5.0 | Newest solver-valid versions inside declared bounds | Full dependency-band check | Required |
| `highest-macos` | macOS 15 | 5.5.0 | Newest solver-valid versions inside declared bounds | Build and exact journal comparison | Informational |

The lower and upper cells resolve against the current opam repository. They deliberately test the
range declared by the package rather than pretending to be reproducible locks. A failure in either
required Ubuntu cell means the declared support bounds or the implementation must change. The
macOS cell is an early portability signal while Ubuntu remains the supported build platform.

Every runtime cell replays the v3 demo, v4 demo, and v4 risk-limited fill scenarios under `TZ=UTC`
and the C locale. It compares the resulting journal files byte for byte with their canonical
fixtures. Standard output and standard error are captured separately because human diagnostics may
contain platform-specific paths or process details and are not part of the journal contract.

Coverage runs once in the exact locked Ubuntu environment. The required Persistra job also runs
once against its full pinned commit; it is not repeated across dependency or operating-system
cells. The manually dispatched Persistra moving-head job remains informational.

Feature-branch pushes do not start CI; the pull-request event owns that validation and avoids a
duplicate check set. Pull requests cancel superseded commits. Push validation runs only on
`develop` and tags, where it is never cancelled, so durable integration evidence is not discarded.
The concurrency key combines the source repository and source branch without colliding with a fork
or another branch.
