#!/usr/bin/env python3
"""Focused contract tests for the replay benchmark harness."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location(
    "benchmark_replay", ROOT / "bench/benchmark_replay.py"
)
assert SPEC is not None and SPEC.loader is not None
BENCHMARK = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = BENCHMARK
SPEC.loader.exec_module(BENCHMARK)


class BenchmarkReplayTest(unittest.TestCase):
    def test_generated_batch_has_requested_dimensions(self) -> None:
        case = BENCHMARK.BenchmarkCase("unit", "batch", 3, 4, 2)
        scenario = BENCHMARK.build_scenario(case)

        self.assertEqual(3, len(scenario["instruments"]))
        self.assertEqual(4, len(scenario["slices"]))
        self.assertTrue(all(len(item["bars"]) == 3 for item in scenario["slices"]))
        self.assertEqual(2, len(scenario["schedule"][0]["intents"]))
        self.assertTrue(
            all(
                intent["limit_price"] == "1"
                for intent in scenario["schedule"][0]["intents"]
            )
        )

    def test_stream_preserves_slices_and_scheduled_intents(self) -> None:
        case = BENCHMARK.BenchmarkCase("unit", "stream", 2, 3, 5)
        records = BENCHMARK.stream_records(BENCHMARK.build_scenario(case))

        self.assertEqual("scenario_header", records[0]["record_type"])
        self.assertEqual("scenario_end", records[-1]["record_type"])
        self.assertEqual("3", records[-1]["payload"]["slice_count"])
        self.assertEqual(5, len(records[1]["payload"]["intents"]))
        self.assertEqual([], records[2]["payload"]["intents"])

    def test_summary_parser_reads_batch_and_stream_counts(self) -> None:
        self.assertEqual(
            (321, 17),
            BENCHMARK.parse_summary(
                "run=benchmark audits=321 orders=17 active=17 filled=0 rejected=0\n"
            ),
        )

    def test_tolerance_comparison_checks_both_metric_directions(self) -> None:
        result = {
            "median_wall_seconds": 1.31,
            "median_peak_rss_kib": 120.0,
            "median_events_per_second": 79.0,
            "median_artifact_bytes_per_second": 90.0,
        }
        baseline = {
            "metrics": {
                "median_wall_seconds": 1.0,
                "median_peak_rss_kib": 100.0,
                "median_events_per_second": 100.0,
                "median_artifact_bytes_per_second": 100.0,
            },
            "tolerances": {
                "median_wall_seconds": 0.30,
                "median_peak_rss_kib": 0.25,
                "median_events_per_second": 0.20,
                "median_artifact_bytes_per_second": 0.20,
            },
        }

        regressions = BENCHMARK.find_regressions(result, baseline)

        self.assertEqual(2, len(regressions))
        self.assertTrue(any("wall" in regression for regression in regressions))
        self.assertTrue(any("events" in regression for regression in regressions))


if __name__ == "__main__":
    unittest.main()
