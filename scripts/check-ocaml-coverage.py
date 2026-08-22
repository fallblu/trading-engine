#!/usr/bin/env python3
"""Validate the OCaml coverage result and its change history."""

from __future__ import annotations

import json
import re
import sys
from decimal import Decimal
from pathlib import Path


SUMMARY_PATTERN = re.compile(
    r"^\s*(?P<percent>\d+(?:\.\d+)?)\s+%\s+"
    r"(?P<covered>\d+)/(?P<total>\d+)\s+Project coverage\s*$",
    re.MULTILINE,
)
ISSUE_PREFIX = "https://github.com/fallblu/trading-engine/issues/"


def fail(message: str) -> None:
    raise SystemExit(f"coverage policy error: {message}")


def load_policy(path: Path) -> dict[str, object]:
    try:
        policy = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        fail(f"cannot read {path}: {error}")
    if not isinstance(policy, dict):
        fail("policy must be a JSON object")
    return policy


def decimal_field(value: object, name: str) -> Decimal:
    if isinstance(value, bool) or not isinstance(value, (int, float, str)):
        fail(f"{name} must be a number")
    try:
        result = Decimal(str(value))
    except Exception as error:
        fail(f"{name} is invalid: {error}")
    if result < 0 or result > 100:
        fail(f"{name} must be between 0 and 100")
    return result


def validate_policy(policy: dict[str, object]) -> Decimal:
    if policy.get("format_version") != 1:
        fail("format_version must be 1")
    minimum = decimal_field(
        policy.get("minimum_coverage_percent"), "minimum_coverage_percent"
    )
    history = policy.get("threshold_history")
    if not isinstance(history, list) or not history:
        fail("threshold_history must contain at least one entry")

    previous: Decimal | None = None
    for index, entry in enumerate(history):
        if not isinstance(entry, dict):
            fail(f"threshold_history[{index}] must be an object")
        entry_minimum = decimal_field(
            entry.get("minimum_coverage_percent"),
            f"threshold_history[{index}].minimum_coverage_percent",
        )
        reason = entry.get("reason")
        issue_url = entry.get("issue_url")
        if not isinstance(reason, str) or not reason.strip():
            fail(f"threshold_history[{index}] must explain the change")
        if not isinstance(issue_url, str) or not issue_url.startswith(ISSUE_PREFIX):
            fail(f"threshold_history[{index}] must link a repository issue")
        if previous is not None and entry_minimum < previous and len(reason.strip()) < 20:
            fail(f"threshold_history[{index}] must explain the threshold reduction")
        previous = entry_minimum

    if previous != minimum:
        fail("the latest threshold history entry must match the active minimum")

    exclusions = policy.get("excluded_paths")
    if not isinstance(exclusions, list):
        fail("excluded_paths must be a list")
    for index, exclusion in enumerate(exclusions):
        if not isinstance(exclusion, dict):
            fail(f"excluded_paths[{index}] must be an object")
        path = exclusion.get("path")
        reason = exclusion.get("reason")
        if not isinstance(path, str) or not path.strip():
            fail(f"excluded_paths[{index}] must name a path")
        if not isinstance(reason, str) or not reason.strip():
            fail(f"excluded_paths[{index}] must explain the exclusion")
    return minimum


def main() -> None:
    if len(sys.argv) != 3:
        fail("usage: check-ocaml-coverage.py POLICY SUMMARY")
    policy_path, summary_path = map(Path, sys.argv[1:])
    minimum = validate_policy(load_policy(policy_path))
    try:
        summary = summary_path.read_text(encoding="utf-8")
    except OSError as error:
        fail(f"cannot read {summary_path}: {error}")
    match = SUMMARY_PATTERN.search(summary)
    if match is None:
        fail("summary does not contain project coverage")
    covered = int(match.group("covered"))
    total = int(match.group("total"))
    if total <= 0 or covered > total:
        fail("summary contains invalid coverage counts")
    actual = Decimal(covered * 100) / Decimal(total)
    if actual < minimum:
        fail(
            f"{covered}/{total} points ({actual:.2f}%) is below the "
            f"{minimum}% minimum"
        )
    print(
        f"OCaml coverage {covered}/{total} points ({actual:.2f}%) "
        f"meets the {minimum}% minimum"
    )


if __name__ == "__main__":
    main()
