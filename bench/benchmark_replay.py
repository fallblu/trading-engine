#!/usr/bin/env python3
"""Benchmark representative batch, stream, OMS, and strategy workloads."""

from __future__ import annotations

import argparse
import copy
import json
import os
import platform
import re
import statistics
import subprocess
import sys
import tempfile
import time
from dataclasses import asdict, dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_EXECUTABLE = ROOT / "_build/default/bin/main.exe"
DEFAULT_BASELINE = ROOT / "bench/baselines/linux-x86_64.json"
FIXTURE = ROOT / "contracts/v5/fixtures/demo.scenario.json"
STRATEGY = ROOT / "bench/latency_strategy.py"
SUMMARY_PATTERN = re.compile(
    r"\baudits=(?P<audits>[0-9]+).*\bactive=(?P<active>[0-9]+)"
)
METRIC_DIRECTIONS = {
    "median_wall_seconds": "higher",
    "median_peak_rss_kib": "higher",
    "median_events_per_second": "lower",
    "median_artifact_bytes_per_second": "lower",
}


@dataclass(frozen=True)
class BenchmarkCase:
    name: str
    replay_format: str
    catalog_size: int
    slice_count: int
    active_order_count: int = 0
    strategy_latency_ms: float | None = None

    @property
    def uses_external_strategy(self) -> bool:
        return self.strategy_latency_ms is not None


@dataclass(frozen=True)
class Sample:
    wall_seconds: float
    peak_rss_kib: int
    audit_events: int
    artifact_bytes: int
    events_per_second: float
    artifact_bytes_per_second: float


SMOKE_CASES = (
    BenchmarkCase("smoke-batch", "batch", 2, 8, active_order_count=4),
    BenchmarkCase("smoke-stream", "stream", 2, 8, active_order_count=4),
    BenchmarkCase("smoke-external-batch", "batch", 1, 4, strategy_latency_ms=1.0),
    BenchmarkCase("smoke-external-stream", "stream", 1, 4, strategy_latency_ms=1.0),
)

FULL_CASES = (
    BenchmarkCase("batch-standard", "batch", 1, 500),
    BenchmarkCase("stream-standard", "stream", 1, 500),
    BenchmarkCase("batch-large-catalog", "batch", 128, 100),
    BenchmarkCase("stream-large-catalog", "stream", 128, 100),
    BenchmarkCase("batch-dense-oms", "batch", 1, 100, active_order_count=256),
    BenchmarkCase("stream-dense-oms", "stream", 1, 100, active_order_count=256),
    BenchmarkCase("external-batch-zero-latency", "batch", 1, 100, strategy_latency_ms=0.0),
    BenchmarkCase("external-stream-zero-latency", "stream", 1, 100, strategy_latency_ms=0.0),
    BenchmarkCase("external-batch-five-ms", "batch", 1, 100, strategy_latency_ms=5.0),
    BenchmarkCase("external-stream-five-ms", "stream", 1, 100, strategy_latency_ms=5.0),
)


def timestamp(value: datetime) -> str:
    return value.isoformat(timespec="seconds").replace("+00:00", "Z")


def _instrument(index: int) -> dict[str, object]:
    return {
        "instrument_id": f"benchmark-equity-{index:04d}",
        "symbol": f"B{index:04d}",
        "quote_currency": "USD",
        "tick_size": "0.01",
        "lot_size": "1",
    }


def build_scenario(case: BenchmarkCase) -> dict[str, object]:
    """Build a deterministic scenario whose declared dimensions match a case."""
    if case.replay_format not in {"batch", "stream"}:
        raise ValueError("replay_format must be batch or stream")
    if case.catalog_size <= 0 or case.slice_count <= 0:
        raise ValueError("catalog_size and slice_count must be positive")
    if not 0 <= case.active_order_count <= 4096:
        raise ValueError("active_order_count must be between 0 and 4096")
    if case.uses_external_strategy and case.active_order_count:
        raise ValueError("external-strategy cases cannot contain a schedule")

    document = json.loads(FIXTURE.read_text(encoding="utf-8"))
    template = document["slices"][0]
    instruments = [_instrument(index + 1) for index in range(case.catalog_size)]
    base = datetime(2026, 8, 21, tzinfo=timezone.utc)
    slices = []
    for offset in range(case.slice_count):
        start = base + timedelta(seconds=offset * 4)
        market_slice = copy.deepcopy(template)
        market_slice.update(
            {
                "slice_sequence": str(offset + 1),
                "start_at": timestamp(start),
                "end_at": timestamp(start + timedelta(seconds=1)),
                "available_at": timestamp(start + timedelta(seconds=2)),
                "received_at": timestamp(start + timedelta(seconds=3)),
                "bars": [
                    {
                        "instrument_id": instrument["instrument_id"],
                        "open": "100",
                        "high": "101",
                        "low": "99",
                        "close": "100",
                        "volume": "1000000",
                    }
                    for instrument in instruments
                ],
                "corporate_actions": [],
            }
        )
        slices.append(market_slice)

    schedule = []
    if case.active_order_count:
        schedule.append(
            {
                "after_slice_sequence": "1",
                "intents": [
                    {
                        "type": "submit_order",
                        "instrument_id": instruments[0]["instrument_id"],
                        "side": "buy",
                        "quantity": "1",
                        "order_kind": "limit",
                        "limit_price": "1",
                    }
                    for _ in range(case.active_order_count)
                ],
            }
        )

    document.update(
        {
            "metadata": {
                "producer": "trading-engine-benchmark",
                "benchmark_case": case.name,
            },
            "run_id": f"benchmark-{case.name}",
            "instruments": instruments,
            "venue_calendars": [
                {
                    "calendar_id": "benchmark-venue-calendar",
                    "calendar_version": "1",
                    "venue_id": "BENCHMARK",
                    "instrument_ids": [
                        instrument["instrument_id"] for instrument in instruments
                    ],
                    "sessions": [
                        {
                            "session_date": "2026-02-01",
                            "policy": "regular",
                            "phases": [
                                {
                                    "phase": "regular",
                                    "opens_at": "2026-02-01T00:00:00Z",
                                    "closes_at": "2026-02-02T00:00:00Z",
                                }
                            ],
                        }
                    ],
                }
            ],
            "risk": {
                "max_order_quantity": "1000000",
                "max_long_position": "1000000",
                "max_short_position": "1000000",
                "max_gross_exposure": "1000000000",
                "max_leverage": "1000000",
                "initial_margin_bps": 1,
                "maintenance_margin_bps": 1,
                "short_borrow_bps": 0,
            },
            "execution": {
                "model": "completed_bar_v1",
                "configuration": {
                    "version": "1",
                    "participation_bps": 10000,
                    "fixed_fee": "0",
                    "fee_bps": 0,
                },
            },
            "max_internal_events": max(1000, case.active_order_count * 4 + 16),
            "schedule": schedule,
            "slices": slices,
        }
    )
    return document


def stream_records(document: dict[str, object]) -> list[dict[str, object]]:
    """Convert a batch document to the semantically equivalent stream records."""
    schedule = {
        item["after_slice_sequence"]: item["intents"]
        for item in document["schedule"]
    }
    header_fields = (
        "metadata",
        "run_id",
        "base_currency",
        "initial_cash",
        "instruments",
        "venue_calendars",
        "risk",
        "execution",
        "max_internal_events",
    )
    records = [
        {
            "contract_version": document["contract_version"],
            "scenario_sequence": "1",
            "record_type": "scenario_header",
            "payload": {field: document[field] for field in header_fields},
        }
    ]
    for index, market_slice in enumerate(document["slices"], start=2):
        records.append(
            {
                "contract_version": document["contract_version"],
                "scenario_sequence": str(index),
                "record_type": "market_slice",
                "payload": {
                    "market_slice": market_slice,
                    "intents": schedule.get(market_slice["slice_sequence"], []),
                },
            }
        )
    records.append(
        {
            "contract_version": document["contract_version"],
            "scenario_sequence": str(len(records) + 1),
            "record_type": "scenario_end",
            "payload": {"slice_count": str(len(document["slices"]))},
        }
    )
    return records


def write_input(case: BenchmarkCase, directory: Path) -> Path:
    document = build_scenario(case)
    if case.replay_format == "batch":
        path = directory / "scenario.json"
        path.write_text(
            json.dumps(document, separators=(",", ":")) + "\n", encoding="utf-8"
        )
    else:
        path = directory / "scenario.jsonl"
        with path.open("w", encoding="utf-8") as channel:
            for record in stream_records(document):
                channel.write(json.dumps(record, separators=(",", ":")) + "\n")
    return path


def parse_summary(stdout: str) -> tuple[int, int]:
    match = SUMMARY_PATTERN.search(stdout)
    if match is None:
        raise ValueError(f"could not parse replay summary: {stdout.strip()}")
    return int(match.group("audits")), int(match.group("active"))


def _peak_rss_kib(usage: Any) -> int:
    peak = int(usage.ru_maxrss)
    return peak // 1024 if sys.platform == "darwin" else peak


def run_once(
    executable: Path, case: BenchmarkCase, scenario: Path, directory: Path, run: int
) -> Sample:
    journal = directory / f"journal-{run}.jsonl"
    transcript = directory / f"strategy-{run}.jsonl"
    command = [str(executable), "--input", str(scenario), "--journal", str(journal)]
    if case.replay_format == "stream":
        command.extend(("--input-format", "jsonl"))
    if case.uses_external_strategy:
        command.extend(
            (
                "--strategy-executable",
                sys.executable,
                "--strategy-arg",
                str(STRATEGY),
                "--strategy-arg",
                str(case.strategy_latency_ms),
                "--strategy-timeout",
                "30",
                "--strategy-transcript",
                str(transcript),
            )
        )

    environment = os.environ.copy()
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    with tempfile.TemporaryFile() as stdout_file, tempfile.TemporaryFile() as stderr_file:
        started = time.perf_counter_ns()
        process = subprocess.Popen(
            command,
            cwd=ROOT,
            env=environment,
            stdout=stdout_file,
            stderr=stderr_file,
        )
        _, status, usage = os.wait4(process.pid, 0)
        elapsed = (time.perf_counter_ns() - started) / 1_000_000_000
        process.returncode = os.waitstatus_to_exitcode(status)
        stdout_file.seek(0)
        stderr_file.seek(0)
        stdout = stdout_file.read().decode("utf-8", errors="replace")
        stderr = stderr_file.read().decode("utf-8", errors="replace")
    if process.returncode != 0:
        raise RuntimeError(
            f"benchmark command failed with exit {process.returncode}: {stderr.strip()}"
        )

    audits, active = parse_summary(stdout)
    if active != case.active_order_count:
        raise RuntimeError(
            f"{case.name} retained {active} active orders; expected "
            f"{case.active_order_count}"
        )
    artifact_paths = [journal]
    if case.uses_external_strategy:
        artifact_paths.append(transcript)
    if any(not path.is_file() for path in artifact_paths):
        raise RuntimeError(f"{case.name} did not publish every expected artifact")
    journal_events = sum(1 for _ in journal.open("rb"))
    if journal_events != audits:
        raise RuntimeError(
            f"{case.name} reported {audits} audits but wrote {journal_events} journal records"
        )
    artifact_bytes = sum(path.stat().st_size for path in artifact_paths)
    return Sample(
        wall_seconds=elapsed,
        peak_rss_kib=_peak_rss_kib(usage),
        audit_events=audits,
        artifact_bytes=artifact_bytes,
        events_per_second=audits / elapsed,
        artifact_bytes_per_second=artifact_bytes / elapsed,
    )


def summarize(case: BenchmarkCase, samples: list[Sample]) -> dict[str, object]:
    if not samples:
        raise ValueError("at least one sample is required")
    audit_counts = {sample.audit_events for sample in samples}
    artifact_sizes = {sample.artifact_bytes for sample in samples}
    if len(audit_counts) != 1 or len(artifact_sizes) != 1:
        raise RuntimeError(f"{case.name} produced nondeterministic artifacts")
    return {
        "case": asdict(case),
        "audit_events": samples[0].audit_events,
        "artifact_bytes": samples[0].artifact_bytes,
        "median_wall_seconds": statistics.median(
            sample.wall_seconds for sample in samples
        ),
        "median_peak_rss_kib": statistics.median(
            sample.peak_rss_kib for sample in samples
        ),
        "median_events_per_second": statistics.median(
            sample.events_per_second for sample in samples
        ),
        "median_artifact_bytes_per_second": statistics.median(
            sample.artifact_bytes_per_second for sample in samples
        ),
        "samples": [asdict(sample) for sample in samples],
    }


def find_regressions(
    result: dict[str, object], baseline: dict[str, object]
) -> list[str]:
    regressions = []
    tolerances = baseline["tolerances"]
    metrics = baseline["metrics"]
    for metric, direction in METRIC_DIRECTIONS.items():
        observed = float(result[metric])
        reference = float(metrics[metric])
        tolerance = float(tolerances[metric])
        threshold = reference * (1 + tolerance if direction == "higher" else 1 - tolerance)
        regressed = observed > threshold if direction == "higher" else observed < threshold
        if regressed:
            regressions.append(
                f"{metric}={observed:.3f} crossed advisory threshold {threshold:.3f}"
            )
    return regressions


def benchmark_environment(executable: Path) -> dict[str, str]:
    version = subprocess.run(
        [str(executable), "--version"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    ).stdout.strip()
    return {
        "system": platform.system(),
        "machine": platform.machine(),
        "python": platform.python_version(),
        "engine": version,
        "dune_profile": os.environ.get("DUNE_PROFILE", "dev"),
    }


def render(results: list[dict[str, object]]) -> None:
    print(
        "case | format | catalog | slices | active | latency ms | wall s | "
        "peak MiB | events/s | artifact MiB/s | baseline"
    )
    print("--- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---")
    for result in results:
        case = result["case"]
        regressions = result.get("advisory_regressions", [])
        latency = case["strategy_latency_ms"]
        print(
            f"{case['name']} | {case['replay_format']} | {case['catalog_size']} | "
            f"{case['slice_count']} | {case['active_order_count']} | "
            f"{'-' if latency is None else latency} | "
            f"{result['median_wall_seconds']:.4f} | "
            f"{result['median_peak_rss_kib'] / 1024:.1f} | "
            f"{result['median_events_per_second']:.0f} | "
            f"{result['median_artifact_bytes_per_second'] / 1048576:.2f} | "
            f"{'advisory regression' if regressions else result.get('baseline_status', 'not compared')}"
        )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--executable", type=Path, default=DEFAULT_EXECUTABLE)
    parser.add_argument("--suite", choices=("smoke", "full"), default="full")
    parser.add_argument("--repetitions", type=int, default=3)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--baseline", type=Path, default=DEFAULT_BASELINE)
    parser.add_argument("--no-baseline", action="store_true")
    parser.add_argument("--enforce", action="store_true")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    if args.repetitions <= 0 or args.warmups < 0:
        parser.error("repetitions must be positive and warmups cannot be negative")
    if args.enforce and args.no_baseline:
        parser.error("--enforce requires baseline comparison")
    executable = args.executable.resolve()
    if not executable.is_file():
        parser.error(f"executable does not exist: {executable}")

    baseline = None
    if not args.no_baseline and args.baseline.is_file():
        baseline = json.loads(args.baseline.read_text(encoding="utf-8"))
    if args.enforce and baseline is None:
        parser.error(f"baseline does not exist: {args.baseline}")
    cases = SMOKE_CASES if args.suite == "smoke" else FULL_CASES
    results = []
    with tempfile.TemporaryDirectory(prefix="trading-engine-benchmark-") as raw_directory:
        root = Path(raw_directory)
        for case in cases:
            case_directory = root / case.name
            case_directory.mkdir()
            scenario = write_input(case, case_directory)
            for warmup in range(args.warmups):
                run_once(executable, case, scenario, case_directory, -(warmup + 1))
            samples = [
                run_once(executable, case, scenario, case_directory, repetition)
                for repetition in range(args.repetitions)
            ]
            result = summarize(case, samples)
            baseline_case = None if baseline is None else baseline["cases"].get(case.name)
            if baseline_case is None:
                result["baseline_status"] = "not compared"
            else:
                result["baseline_status"] = "within tolerance"
                result["advisory_regressions"] = find_regressions(result, baseline_case)
            results.append(result)

    report = {
        "schema_version": 1,
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "suite": args.suite,
        "repetitions": args.repetitions,
        "warmups": args.warmups,
        "environment": benchmark_environment(executable),
        "results": results,
    }
    render(results)
    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
        print(f"wrote {args.output}")
    regression_count = sum(
        len(result.get("advisory_regressions", [])) for result in results
    )
    if regression_count:
        print(
            f"{regression_count} advisory regression(s) detected; "
            "use --enforce to make them fatal",
            file=sys.stderr,
        )
    return 1 if args.enforce and regression_count else 0


if __name__ == "__main__":
    raise SystemExit(main())
