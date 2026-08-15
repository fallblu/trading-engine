# Contributing

Use the repository-local opam switch and install development dependencies:

```sh
opam install . --deps-only --with-test --locked
```

The schema conformance tests also require Python 3 and the JSON Schema format
validators:

```sh
python3 -m venv .venv-schema
.venv-schema/bin/python -m pip install 'jsonschema[format-nongpl]==4.26.0'
export PATH="$PWD/.venv-schema/bin:$PATH"
```

Run the complete local gate before committing:

```sh
make check
```

The gate formats a copy check, builds every target, and runs all tests. Keep commits small,
coherent, and working. Use subject-only conventional commit messages such as
`feat: implement deterministic order matching`.

Do not add secrets, provider credentials, or customer account data to fixtures or journals.
