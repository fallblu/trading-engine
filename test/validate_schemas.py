#!/usr/bin/env python3
"""Validate the committed interchange fixtures against their JSON Schemas."""

from __future__ import annotations

import copy
import json
import sys
from pathlib import Path

from jsonschema import Draft202012Validator
from jsonschema.exceptions import ValidationError
from referencing import Registry, Resource


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
    if len(sys.argv) != 7:
        raise SystemExit(
            "usage: validate_schemas.py SCENARIO_SCHEMA STREAM_SCHEMA JOURNAL_SCHEMA "
            "SCENARIO STREAM JOURNAL"
        )
    (
        scenario_schema_path,
        stream_schema_path,
        journal_schema_path,
        scenario_path,
        stream_path,
        journal_path,
    ) = map(Path, sys.argv[1:])
    scenario_schema = load(scenario_schema_path)
    stream_schema = load(stream_schema_path)
    journal_schema = load(journal_schema_path)
    Draft202012Validator.check_schema(scenario_schema)
    Draft202012Validator.check_schema(stream_schema)
    Draft202012Validator.check_schema(journal_schema)
    registry = Registry().with_resource(
        scenario_schema["$id"], Resource.from_contents(scenario_schema)
    )
    scenario_validator = Draft202012Validator(
        scenario_schema,
        format_checker=Draft202012Validator.FORMAT_CHECKER,
    )
    journal_validator = Draft202012Validator(
        journal_schema,
        format_checker=Draft202012Validator.FORMAT_CHECKER,
    )
    stream_validator = Draft202012Validator(
        stream_schema,
        format_checker=Draft202012Validator.FORMAT_CHECKER,
        registry=registry,
    )

    scenario = load(scenario_path)
    contract_version = scenario["contract_version"]
    unsupported_version = "unsupported"
    scenario_validator.validate(scenario)
    stream_records = [
        json.loads(line) for line in stream_path.read_text(encoding="utf-8").splitlines()
    ]
    for line_number, record in enumerate(stream_records, start=1):
        try:
            stream_validator.validate(record)
        except ValidationError as error:
            raise AssertionError(
                f"{stream_path}:{line_number} does not satisfy the stream schema"
            ) from error
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
    unversioned_scenario = copy.deepcopy(scenario)
    del unversioned_scenario["contract_version"]
    expect_invalid(scenario_validator, unversioned_scenario)
    unsupported_scenario = copy.deepcopy(scenario)
    unsupported_scenario["contract_version"] = unsupported_version
    expect_invalid(scenario_validator, unsupported_scenario)
    missing_execution_model = copy.deepcopy(scenario)
    del missing_execution_model["execution"]["model"]
    expect_invalid(scenario_validator, missing_execution_model)
    unsupported_execution_model = copy.deepcopy(scenario)
    unsupported_execution_model["execution"]["model"] = "future_model"
    expect_invalid(scenario_validator, unsupported_execution_model)
    if contract_version in {"3", "4"}:
        excessive_feedback_cap = copy.deepcopy(scenario)
        excessive_feedback_cap["max_internal_events"] = 4611686018427387904
        expect_invalid(scenario_validator, excessive_feedback_cap)
    unversioned_stream_record = copy.deepcopy(stream_records[0])
    del unversioned_stream_record["contract_version"]
    expect_invalid(stream_validator, unversioned_stream_record)
    unsupported_stream_record = copy.deepcopy(stream_records[1])
    unsupported_stream_record["contract_version"] = unsupported_version
    expect_invalid(stream_validator, unsupported_stream_record)
    malformed_stream_slice = copy.deepcopy(stream_records[1])
    malformed_stream_slice["payload"]["market_slice"]["unexpected"] = True
    expect_invalid(stream_validator, malformed_stream_slice)
    noncanonical = copy.deepcopy(scenario)
    if contract_version in {"3", "4"}:
        noncanonical["initial_cash"][0]["amount"] = "10000.0"
    else:
        noncanonical["initial_cash"] = "10000.0"
    expect_invalid(scenario_validator, noncanonical)
    first_journal_record = json.loads(
        journal_path.read_text(encoding="utf-8").splitlines()[0]
    )
    missing_event_id = copy.deepcopy(first_journal_record)
    del missing_event_id["event_id"]
    expect_invalid(journal_validator, missing_event_id)
    duplicate_causes = copy.deepcopy(first_journal_record)
    duplicate_causes["causation_ids"] = ["prior-event", "prior-event"]
    expect_invalid(journal_validator, duplicate_causes)
    unversioned_journal_record = copy.deepcopy(first_journal_record)
    del unversioned_journal_record["contract_version"]
    expect_invalid(journal_validator, unversioned_journal_record)
    unsupported_journal_record = copy.deepcopy(first_journal_record)
    unsupported_journal_record["contract_version"] = unsupported_version
    expect_invalid(journal_validator, unsupported_journal_record)
    journal_records = [
        json.loads(line)
        for line in journal_path.read_text(encoding="utf-8").splitlines()
    ]
    if contract_version == "4":
        fill_clipped = copy.deepcopy(first_journal_record)
        fill_clipped["event_type"] = "fill_clipped"
        fill_clipped["payload"] = {
            "reason": {
                "version": "1",
                "policy": "max_leverage",
                "threshold": {"unit": "ratio", "value": "2"},
            },
            "order_id": "fixture-order",
            "instrument_id": "fixture-instrument",
            "proposed_quantity": "10",
            "permitted_quantity": "5",
            "price": "100",
        }
        journal_validator.validate(fill_clipped)
        mismatched_threshold = copy.deepcopy(fill_clipped)
        mismatched_threshold["payload"]["reason"]["threshold"] = {
            "unit": "money",
            "value": "2",
        }
        expect_invalid(journal_validator, mismatched_threshold)
        unknown_policy = copy.deepcopy(fill_clipped)
        unknown_policy["payload"]["reason"]["policy"] = "future_policy"
        expect_invalid(journal_validator, unknown_policy)
    order_record = next(
        record for record in journal_records if record["event_type"] == "order_accepted"
    )
    missing_creation_event = copy.deepcopy(order_record)
    del missing_creation_event["payload"]["created_event_id"]
    expect_invalid(journal_validator, missing_creation_event)
    valuation_record = next(
        record for record in journal_records if record["event_type"] == "valuation"
    )
    missing_positions = copy.deepcopy(valuation_record)
    del missing_positions["payload"]["positions"]
    expect_invalid(journal_validator, missing_positions)
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
