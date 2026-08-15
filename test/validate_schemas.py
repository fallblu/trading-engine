#!/usr/bin/env python3
"""Validate the committed interchange fixtures against their JSON Schemas."""

from __future__ import annotations

import copy
import json
import sys
from pathlib import Path

from jsonschema import Draft202012Validator
from jsonschema.exceptions import ValidationError


def load(path: Path) -> object:
    def object_pairs(pairs: list[tuple[str, object]]) -> dict[str, object]:
        result: dict[str, object] = {}
        for key, value in pairs:
            if key in result:
                raise ValueError(f"duplicate key {key!r} in {path}")
            result[key] = value
        return result

    return json.loads(
        path.read_text(encoding="utf-8"),
        object_pairs_hook=object_pairs,
        parse_constant=lambda value: (_ for _ in ()).throw(
            ValueError(f"non-finite number {value!r} in {path}")
        ),
    )


def expect_invalid(validator: Draft202012Validator, instance: object) -> None:
    try:
        validator.validate(instance)
    except ValidationError:
        return
    raise AssertionError("invalid conformance case unexpectedly passed")


def main() -> None:
    if len(sys.argv) != 5:
        raise SystemExit(
            "usage: validate_schemas.py SCENARIO_SCHEMA JOURNAL_SCHEMA SCENARIO JOURNAL"
        )
    scenario_schema_path, journal_schema_path, scenario_path, journal_path = map(
        Path, sys.argv[1:]
    )
    scenario_schema = load(scenario_schema_path)
    journal_schema = load(journal_schema_path)
    Draft202012Validator.check_schema(scenario_schema)
    Draft202012Validator.check_schema(journal_schema)
    scenario_validator = Draft202012Validator(
        scenario_schema,
        format_checker=Draft202012Validator.FORMAT_CHECKER,
    )
    journal_validator = Draft202012Validator(
        journal_schema,
        format_checker=Draft202012Validator.FORMAT_CHECKER,
    )

    scenario = load(scenario_path)
    scenario_validator.validate(scenario)
    for line_number, line in enumerate(
        journal_path.read_text(encoding="utf-8").splitlines(), start=1
    ):
        try:
            journal_validator.validate(json.loads(line))
        except (ValidationError, ValueError) as error:
            raise AssertionError(
                f"{journal_path}:{line_number} does not satisfy the journal schema"
            ) from error

    extra_field = copy.deepcopy(scenario)
    extra_field["unexpected_contract_field"] = True
    expect_invalid(scenario_validator, extra_field)
    noncanonical = copy.deepcopy(scenario)
    noncanonical["initial_cash"] = "10000.0"
    expect_invalid(scenario_validator, noncanonical)
    first_journal_record = json.loads(
        journal_path.read_text(encoding="utf-8").splitlines()[0]
    )
    for timestamp in (
        "2026-01-02t14:30:00.1z",
        "2026-01-02T14:30:00.123456+05:30",
        "2026-01-02T14:30:00-05:00",
    ):
        valid_scenario_timestamp = copy.deepcopy(scenario)
        valid_scenario_timestamp["slices"][0]["start_at"] = timestamp
        scenario_validator.validate(valid_scenario_timestamp)
        valid_journal_timestamp = copy.deepcopy(first_journal_record)
        valid_journal_timestamp["recorded_at"] = timestamp
        journal_validator.validate(valid_journal_timestamp)
    for timestamp in (
        "2026-01-02 14:30:00Z",
        "2026-01-02T14:30:00+0000",
        "2026-01-02T14:30:00-05",
        "2026-01-02T14:30:60Z",
        "2026-01-02T14:30:00.1234567Z",
    ):
        invalid_scenario_timestamp = copy.deepcopy(scenario)
        invalid_scenario_timestamp["slices"][0]["start_at"] = timestamp
        expect_invalid(scenario_validator, invalid_scenario_timestamp)
        invalid_journal_timestamp = copy.deepcopy(first_journal_record)
        invalid_journal_timestamp["recorded_at"] = timestamp
        expect_invalid(journal_validator, invalid_journal_timestamp)
    stale_intent = copy.deepcopy(scenario)
    stale_intent["schedule"][0]["intents"][0] = {
        "type": "unsupported_intent",
    }
    expect_invalid(scenario_validator, stale_intent)


if __name__ == "__main__":
    main()
