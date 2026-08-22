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

## Replay regression suite

The replay suite measures complete public-CLI runs, including scenario parsing, deterministic
reduction, and artifact publication. It generates every input before the timed interval and covers
the batch and JSON Lines paths independently.

```sh
make benchmark
```

The default run performs one warmup and records the median of three samples in
`benchmark-results/replay.json`. Generated reports are ignored by Git. The table printed to the
terminal and the JSON report include:

- Wall-clock seconds measured with the monotonic high-resolution clock.
- Peak resident memory of the direct engine process. On Linux this is `ru_maxrss`, normalized to
  KiB; an external strategy's own resident memory is intentionally excluded.
- Audit-event throughput, using the CLI's audit count checked against journal line count.
- Artifact-byte throughput, using the published journal size and, for external cases, the strategy
  transcript size.

The full matrix holds all unlisted dimensions constant while varying the source of likely
regressions:

| Workload pair | Catalog | Slices | Active orders | Strategy latency |
|---|---:|---:|---:|---:|
| Standard history | 1 | 500 | 0 | none |
| Large catalog | 128 | 100 | 0 | none |
| Dense OMS | 1 | 100 | 256 | none |
| External strategy | 1 | 100 | 0 | 0 ms/event |
| Latent external strategy | 1 | 100 | 0 | 5 ms/event |

Every workload is run in both batch and stream form. Dense-OMS cases submit persistent buy limits
far below the market after the first slice and verify that exactly 256 orders remain active. The
latency strategy returns no intents and sleeps only before each event response, keeping protocol
initialization and shutdown outside the modeled per-event delay.

### Baseline and tolerance policy

`bench/baselines/linux-x86_64.json` stores the initial Linux/WSL2 development-build baseline from
the reference machine described in that file. Wall time allows a 35% increase; peak RSS allows a
25% increase; event and artifact throughput allow a 30% decrease. These deliberately broad
tolerances account for scheduler, filesystem-cache, and allocator noise while the project gathers
measurements across more runners.

Baseline comparison is advisory by default and therefore cannot make `make benchmark` fail. A
reported regression is a prompt to repeat the run on comparable hardware and profile the affected
dimension. On a controlled, baseline-compatible runner, opt into a failing gate with:

```sh
python3 bench/benchmark_replay.py --enforce
```

Refresh a baseline only after explaining an intentional workload or performance change and
recording the engine version, build profile, machine, warmup count, and repetition count. Do not
replace a baseline solely to clear an advisory regression.

`make check` runs a one-sample smoke matrix with tiny versions of all four replay routes. It checks
input generation, external-strategy protocol behavior, active-order retention, audit counts, and
artifact publication without comparing timing values.
