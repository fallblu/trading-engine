#!/usr/bin/env python3
"""Validate the stable diagnostic schema and canonical fixture."""

from __future__ import annotations

import copy
import json
import sys
from pathlib import Path

from jsonschema import Draft202012Validator
from jsonschema.exceptions import ValidationError


def expect_invalid(validator: Draft202012Validator, instance: object) -> None:
    try:
        validator.validate(instance)
    except ValidationError:
        return
    raise AssertionError("invalid diagnostic unexpectedly passed")


def main() -> None:
    if len(sys.argv) != 3:
        raise SystemExit(
            "usage: validate_diagnostic_schema.py DIAGNOSTIC_SCHEMA FIXTURE"
        )
    schema_path, fixture_path = map(Path, sys.argv[1:])
    schema = json.loads(schema_path.read_text(encoding="utf-8"))
    fixture = json.loads(fixture_path.read_text(encoding="utf-8"))
    Draft202012Validator.check_schema(schema)
    validator = Draft202012Validator(schema)
    validator.validate(fixture)

    unknown_code = copy.deepcopy(fixture)
    unknown_code["code"] = "strategy.other"
    expect_invalid(validator, unknown_code)
    prose_sequence = copy.deepcopy(fixture)
    prose_sequence["context"]["sequence"] = "01"
    expect_invalid(validator, prose_sequence)
    duplicate_cause = copy.deepcopy(fixture)
    duplicate_cause["context"]["causation_ids"] *= 2
    expect_invalid(validator, duplicate_cause)
    exposed_payload = copy.deepcopy(fixture)
    exposed_payload["payload"] = {"secret": True}
    expect_invalid(validator, exposed_payload)
    incomplete_cause = copy.deepcopy(fixture)
    incomplete_cause["cause"] = {"kind": "system_error"}
    expect_invalid(validator, incomplete_cause)


if __name__ == "__main__":
    main()
