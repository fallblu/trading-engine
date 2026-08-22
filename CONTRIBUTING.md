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

The gate formats a copy check, builds every target, and runs all tests. Keep commits small,
coherent, and working. Use subject-only conventional commit messages such as
`feat: implement deterministic order matching`.

Do not add secrets, provider credentials, or customer account data to fixtures or journals.
