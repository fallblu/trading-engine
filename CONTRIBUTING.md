# Contributing

Use the repository-local opam switch and install development dependencies:

```sh
opam install . --deps-only --with-test --locked
```

Run the complete local gate before committing:

```sh
make check
```

The gate formats a copy check, builds every target, and runs all tests. Keep commits small,
coherent, and working. Use subject-only conventional commit messages such as
`feat: implement deterministic order matching`.

Do not add secrets, provider credentials, or customer account data to fixtures or journals.
