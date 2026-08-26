# Trading Engine

Trading Engine is a deterministic OCaml engine for replaying trading strategies.

It accepts a strict scenario, runs strategy decisions through risk, order management, execution,
financing, settlement, and accounting, then writes a hash-bound JSON Lines audit journal. The same
reducer supports scheduled intents and supervised external strategy processes.

## Highlights

- Exact fixed-point prices, quantities, money, weights, and FX rates
- Completed-bar, conservative-bar, quote/trade, and order-book execution models
- Market, limit, stop, stop-limit, IOC, GTC, GTD, DAY, and FOK orders
- Instrument and portfolio risk, margin, short locates, recalls, and liquidation
- Multi-currency accounting, financing, settlement, fees, and corporate actions
- Deterministic event IDs, causal references, transcripts, and durable artifact publication
- Strict v1 JSON Schemas for scenarios, journals, strategy messages, diagnostics, and CLI results

## Quick start

The repository uses a local opam switch.

```sh
make bootstrap
make check
```

Validate the canonical scenario:

```sh
opam exec -- dune exec trading-engine -- \
  --input contracts/v1/fixtures/demo.scenario.json \
  --validate-only
```

Replay it to a journal:

```sh
opam exec -- dune exec trading-engine -- \
  --input contracts/v1/fixtures/demo.scenario.json \
  --journal demo.journal.jsonl
```

Use `--input-format jsonl` for bounded-memory stream input. Use `--capabilities` for the
machine-readable runtime surface and `--output-format json` for structured success and failure
output.

## External strategies

External strategies exchange one synchronous JSON Lines message at a time:

```sh
opam exec -- dune exec trading-engine -- \
  --input contracts/strategy/v1/fixtures/external.scenario.json \
  --journal external.journal.jsonl \
  --strategy-executable ./my-strategy \
  --strategy-transcript external.strategy.jsonl
```

The child process is supervised but not sandboxed. Protocol output belongs on standard output;
strategy logs belong on standard error.

## Documentation

- [Architecture](docs/architecture.md)
- [Execution model](docs/execution-model.md)
- [Scenario and journal](docs/scenario.md)
- [Replay contract v1](contracts/v1/README.md)
- [Strategy protocol v1](contracts/strategy/v1/README.md)
- [Diagnostics](docs/diagnostics.md)
- [Persistra integration](docs/persistra.md)
- [Contributing](CONTRIBUTING.md)
- [Security](.github/SECURITY.md)

The complete documentation site is published at
[fallblu.github.io/trading-engine](https://fallblu.github.io/trading-engine/).
