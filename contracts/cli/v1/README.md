# CLI result contract v1

Pass `--output-format json` to receive one compact JSON success document. The
[`result.schema.json`](result.schema.json) schema defines its stable fields. The document identifies
the operation and run, binds scenario and artifact hashes, reports replay counts and the normalized
current valuation, and names any created artifacts.

Failures use the existing
[diagnostic contract v1](../../diagnostic/v1/diagnostic.schema.json). Selecting JSON output also
selects JSON diagnostics, so automation does not need to combine two format flags. Diagnostics are
always written to standard error.

When `--journal -` is selected, standard output contains only the complete JSON Lines journal. The
success document moves to standard error. Its journal artifact is named `stdout`, and the final
`run_completed` record signals successful completion. A consumer must also require a zero process
exit status. Strategy protocol messages remain confined to the supervised child process.

This result contract is independent of the scenario contract version. Its `valuation` uses the
current v16 valuation shape so consumers receive one stable automation model for older accepted
scenarios.
