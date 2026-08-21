# Performance

The engine treats complexity regressions in validation and pure reduction as correctness risks for
large deterministic replays. Performance tests use contract-sized batches and dense schedules but
do not impose machine-specific time limits in `make check`.

## Reducer feedback queue

The reducer stores pending intents and notifications in an immutable two-list queue. Tail insertion
and removal are amortized `O(1)`. Prepending a strategy response is `O(k)` in the response size,
independent of work already pending. Processing `n` queued items is therefore `O(n)` apart from the
domain work performed by each item.

`test/test_reducer.ml` exercises the maximum 4,096-intent strategy batch. Every intent creates a
notification, and the test completes at the exact 8,193-event feedback limit. On the reference
machine, three warm runs of that focused test had these median wall times:

| Queue implementation | Median |
|---|---:|
| Pending-list append | 0.11 s |
| Immutable two-list queue | 0.01 s |

## Dense batch schedules

Batch validation builds one ordered index that associates every slice sequence with its slice and
successor. For `s` slices and `m` schedule entries, index construction and lookup cost
`O((s + m) log s)`, plus intent validation. The prior complete-list lookup cost `O(s * m)`.

Run the public-CLI benchmark after building the executable:

```sh
opam exec -- dune build bin/main.exe
python3 bench/benchmark_batch_schedule.py
```

The script generates valid batch scenarios outside the timed section, invokes `--validate-only`
three times per size, and prints medians plus the observed range. These results were measured on
2026-08-21 under Linux/WSL2 on an Intel Core i7-10750H using the default Dune development build:

| Slices and schedule entries | Complete-list lookup | Ordered index | Speedup |
|---:|---:|---:|---:|
| 5,000 | 0.346 s | 0.216 s | 1.6x |
| 10,000 | 0.911 s | 0.407 s | 2.2x |
| 20,000 | 3.264 s | 0.845 s | 3.9x |

The exact timings are illustrative rather than service-level targets. The growing baseline ratio
and near-linear indexed results are the relevant regression signal.
