# Protocol fuzzing

The deterministic protocol harness exercises every untrusted parsing boundary:

- batch scenario JSON and bounded JSON Lines streams
- external strategy responses
- RFC 3339 timestamps
- fixed-point decimals
- opaque identifiers
- raw JSON used by canonical journal and transcript fixtures

The seed corpus includes every file under a committed contract `fixtures/`
directory and every materialized case in `contracts/conformance/cases.json`.
Before mutating inputs, the harness replays that complete corpus and fixed hostile
inputs for malformed UTF-8, 512-level nesting, a token larger than one MiB,
duplicate keys, and truncation.

Run the bounded campaign used by `make check` and CI:

```sh
make fuzz-smoke
```

Run a longer local campaign by choosing the seed and mutation count:

```sh
FUZZ_SEED=1401 FUZZ_CASES=1000000 make fuzz
```

Each mutation truncates, flips, inserts, deletes, or duplicates bytes. A failure
prints the boundary, source name, input length, hexadecimal prefix, and exception.
Repeat the same command with the reported seed and at least the reported case
index to reproduce it. Keep the seed and minimized regression input when adding a
failure-path test.

The harness checks parser totality, not acceptance. Invalid input may return any
documented diagnostic, but it must not raise an exception, abort, or bypass the
stream and strategy size limits.
