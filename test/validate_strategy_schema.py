#!/usr/bin/env python3
"""Validate the external strategy protocol schemas and canonical transcript."""

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

    return json.loads(path.read_text(encoding="utf-8"), object_pairs_hook=object_pairs)


def expect_invalid(validator: Draft202012Validator, instance: object) -> None:
    try:
        validator.validate(instance)
    except ValidationError:
        return
    raise AssertionError("invalid strategy protocol case unexpectedly passed")


def main() -> None:
    if len(sys.argv) != 7:
        raise SystemExit(
            "usage: validate_strategy_schema.py SCENARIO_SCHEMA JOURNAL_SCHEMA "
            "DIAGNOSTIC_SCHEMA MESSAGE_SCHEMA TRANSCRIPT_SCHEMA TRANSCRIPT"
        )
    (
        scenario_path,
        journal_path,
        diagnostic_path,
        message_path,
        transcript_path,
        fixture_path,
    ) = map(Path, sys.argv[1:])
    schemas = [
        load(path)
        for path in (scenario_path, journal_path, diagnostic_path, message_path)
    ]
    transcript_schema = load(transcript_path)
    for schema in [*schemas, transcript_schema]:
        Draft202012Validator.check_schema(schema)
    registry = Registry().with_resources(
        (schema["$id"], Resource.from_contents(schema)) for schema in schemas
    )
    message_validator = Draft202012Validator(
        schemas[-1],
        format_checker=Draft202012Validator.FORMAT_CHECKER,
        registry=registry,
    )
    transcript_registry = registry.with_resource(
        transcript_schema["$id"], Resource.from_contents(transcript_schema)
    )
    transcript_validator = Draft202012Validator(
        transcript_schema,
        format_checker=Draft202012Validator.FORMAT_CHECKER,
        registry=transcript_registry,
    )
    records = [json.loads(line) for line in fixture_path.read_text().splitlines()]
    for expected_sequence, record in enumerate(records, start=1):
        transcript_validator.validate(record)
        message_validator.validate(record["message"])
        assert record["transcript_sequence"] == str(expected_sequence)
        expected_direction = (
            "engine_to_strategy" if expected_sequence % 2 else "strategy_to_engine"
        )
        assert record["direction"] == expected_direction
        if expected_sequence % 2 == 0:
            assert (
                record["message"]["strategy_sequence"]
                == records[expected_sequence - 2]["message"]["strategy_sequence"]
            )
    assert records[0]["message"]["message_type"] == "initialize"
    assert records[1]["message"]["message_type"] == "ready"
    assert records[-2]["message"]["message_type"] == "shutdown"
    assert records[-1]["message"]["message_type"] == "stopped"

    extra_field = copy.deepcopy(records[0]["message"])
    extra_field["unexpected"] = True
    expect_invalid(message_validator, extra_field)
    unsupported_version = copy.deepcopy(records[1]["message"])
    unsupported_version["strategy_protocol_version"] = "2"
    expect_invalid(message_validator, unsupported_version)
    malformed_sequence = copy.deepcopy(records[1]["message"])
    malformed_sequence["strategy_sequence"] = "01"
    expect_invalid(message_validator, malformed_sequence)
    intents = next(
        copy.deepcopy(record["message"])
        for record in records
        if record["message"]["message_type"] == "intents"
        and record["message"]["payload"]["intents"]
    )
    intent = intents["payload"]["intents"][0]
    intents["payload"]["intents"] = [intent] * 4097
    expect_invalid(message_validator, intents)
    malformed_transcript = copy.deepcopy(records[0])
    malformed_transcript["direction"] = "network"
    expect_invalid(transcript_validator, malformed_transcript)

    rejected_response = {
        "strategy_diagnostic_version": "1",
        "transcript_sequence": "2",
        "record_type": "rejected_strategy_response",
        "expected_strategy_sequence": "1",
        "diagnostic": {
            "diagnostic_version": "1",
            "code": "strategy.protocol",
            "phase": "strategy",
            "message": "strategy initialization: invalid strategy response JSON",
            "context": {"json_path": "$", "sequence": "1"},
            "cause": None,
        },
        "evidence": {
            "encoding": "hex",
            "prefix": "7b",
            "observed_bytes": 1,
            "truncated": False,
        },
    }
    transcript_validator.validate(rejected_response)
    assert "direction" not in rejected_response
    assert "message" not in rejected_response

    rejection_as_exchange = copy.deepcopy(rejected_response)
    rejection_as_exchange["direction"] = "strategy_to_engine"
    rejection_as_exchange["message"] = records[1]["message"]
    expect_invalid(transcript_validator, rejection_as_exchange)
    oversized_prefix = copy.deepcopy(rejected_response)
    oversized_prefix["evidence"]["prefix"] = "00" * 257
    expect_invalid(transcript_validator, oversized_prefix)
    unversioned_rejection = copy.deepcopy(rejected_response)
    del unversioned_rejection["strategy_diagnostic_version"]
    expect_invalid(transcript_validator, unversioned_rejection)


if __name__ == "__main__":
    main()
