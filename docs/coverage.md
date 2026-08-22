# OCaml coverage

The OCaml coverage gate instruments the production library and command-line executable with
Bisect_ppx while running the normal Dune test aliases. Normal `make build`, `make test`, and
`make check` targets remain uninstrumented, so coverage cannot change deterministic journals,
transcripts, diagnostics, or other contract output.

The repository-local opam package source selects one upstream Bisect_ppx commit that supports the
project's OCaml 5.5 and ppxlib toolchain and verifies its archive with SHA-256. Bootstrap registers
that package source before installing the lock. The environment check verifies the package source
and locked dependency versions, preventing a local fallback to an incompatible release.

Run the gate from a bootstrapped development environment:

```sh
make coverage
```

The command recreates the isolated `_build-coverage/` and `_coverage/` directories, then writes
three views of the same run. A fresh isolated build ensures every test executable and cram
invocation contributes new instrumentation data without changing the normal `_build/` tree or
source-root editor artifacts:

- `summary.txt` lists every production module and the project-wide instrumented-point result.
- `html/index.html` highlights expression and control-flow points, making unvisited match arms,
  conditions, and exception paths directly inspectable.
- `cobertura.xml` provides line-oriented machine-readable data for CI and external analysis.

CI publishes these files as the `ocaml-coverage` artifact for 14 days. The report command uses
`--expect bin/` and `--expect lib/`, so a production module that silently disappears from the
instrumented report fails the job.

## Threshold policy

`coverage/ocaml-policy.json` records the active project-wide minimum and its complete change
history. Every entry requires both an explanation and a repository issue. A lower minimum must be
appended as a new explained history entry; editing the active number alone fails the policy check.
Raise the minimum when sustained coverage permits it.

There are currently no excluded production paths. Tests, generated build files, vendored
dependencies, schemas, and Python validation tools are outside the OCaml instrumentation scope;
they are exercised by the same Dune aliases but do not contribute points. If a production
expression must use `[@coverage off]` or a production path must be excluded later, add the path and
a concrete reason to `excluded_paths` in the policy before using the exclusion. Do not exclude
defensive failures merely because they are difficult to trigger.

The initial floor was measured only after adding focused tests for bar price/volume invariants and
corporate-action validation, comparison, and rendering. These paths guard market-data integrity
and split/dividend semantics, so their coverage was addressed before adopting the project-wide
minimum.
