#!/usr/bin/env python3
"""Benchmark dense batch-schedule validation through the public CLI."""

from __future__ import annotations

import argparse
import copy
import json
import statistics
import subprocess
import tempfile
import time
from datetime import datetime, timedelta, timezone
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_EXECUTABLE = ROOT / "_build/default/bin/main.exe"
FIXTURE = ROOT / "contracts/v4/fixtures/demo.scenario.json"


def timestamp(value: datetime) -> str:
    return value.isoformat(timespec="seconds").replace("+00:00", "Z")


def dense_scenario(size: int) -> dict[str, object]:
    document = json.loads(FIXTURE.read_text(encoding="utf-8"))
    template = document["slices"][0]
    base = datetime(2026, 2, 1, tzinfo=timezone.utc)
    slices = []
    schedule = []
    for offset in range(size):
        sequence = offset + 1
        start = base + timedelta(seconds=offset * 4)
        market_slice = copy.deepcopy(template)
        market_slice.update(
            {
                "slice_sequence": str(sequence),
                "start_at": timestamp(start),
                "end_at": timestamp(start + timedelta(seconds=1)),
                "available_at": timestamp(start + timedelta(seconds=2)),
                "received_at": timestamp(start + timedelta(seconds=3)),
                "corporate_actions": [],
            }
        )
        slices.append(market_slice)
        schedule.append(
            {
                "after_slice_sequence": str(sequence),
                "intents": [
                    {
                        "type": "emit_metric",
                        "name": "dense_schedule",
                        "value": str(sequence),
                    }
                ],
            }
        )
    document["schedule"] = schedule
    document["slices"] = slices
    return document


def measure(executable: Path, scenario: Path, repetitions: int) -> list[float]:
    durations = []
    command = [str(executable), "--input", str(scenario), "--validate-only"]
    for _ in range(repetitions):
        started = time.perf_counter()
        subprocess.run(
            command,
            cwd=ROOT,
            check=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        durations.append(time.perf_counter() - started)
    return durations


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--executable", type=Path, default=DEFAULT_EXECUTABLE
    )
    parser.add_argument(
        "--sizes", type=int, nargs="+", default=[5_000, 10_000, 20_000]
    )
    parser.add_argument("--repetitions", type=int, default=3)
    args = parser.parse_args()
    if args.repetitions <= 0 or any(size <= 0 for size in args.sizes):
        parser.error("sizes and repetitions must be positive")
    executable = args.executable.resolve()
    if not executable.is_file():
        parser.error(f"executable does not exist: {executable}")

    print("slices,schedule,repetitions,median_seconds,min_seconds,max_seconds")
    for size in args.sizes:
        with tempfile.NamedTemporaryFile(
            mode="w", encoding="utf-8", suffix=".scenario.json"
        ) as scenario:
            json.dump(dense_scenario(size), scenario, separators=(",", ":"))
            scenario.flush()
            durations = measure(executable, Path(scenario.name), args.repetitions)
        print(
            f"{size},{size},{args.repetitions},"
            f"{statistics.median(durations):.6f},{min(durations):.6f},"
            f"{max(durations):.6f}"
        )


if __name__ == "__main__":
    main()
