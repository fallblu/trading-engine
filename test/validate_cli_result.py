#!/usr/bin/env python3
"""Validate live CLI success results against their versioned JSON Schema."""

from __future__ import annotations

import hashlib
import json
import subprocess
import sys
import tempfile
from pathlib import Path

from jsonschema import Draft202012Validator
from referencing import Registry, Resource


def load(path: Path) -> object:
    return json.loads(path.read_text(encoding="utf-8"))


def run(binary: Path, *arguments: str) -> tuple[object, str]:
    completed = subprocess.run(
        [str(binary), "--output-format", "json", *arguments],
        check=True,
        capture_output=True,
        text=True,
    )
    return json.loads(completed.stdout), completed.stderr


def main() -> None:
    if len(sys.argv) != 5:
        raise SystemExit(
            "usage: validate_cli_result.py RESULT_SCHEMA JOURNAL_SCHEMA BINARY SCENARIO"
        )
    result_path, journal_path, binary, scenario = map(Path, sys.argv[1:])
    result_schema = load(result_path)
    journal_schema = load(journal_path)
    Draft202012Validator.check_schema(result_schema)
    registry = Registry().with_resource(
        journal_schema["$id"], Resource.from_contents(journal_schema)
    )
    validator = Draft202012Validator(result_schema, registry=registry)

    validation, validation_stderr = run(
        binary, "--validate-only", "--input", str(scenario)
    )
    validator.validate(validation)
    assert validation_stderr == ""
    assert validation["operation"] == "validate"

    with tempfile.TemporaryDirectory() as directory:
        journal = Path(directory) / "run.journal.jsonl"
        replay, replay_stderr = run(
            binary, "--input", str(scenario), "--journal", str(journal)
        )
        validator.validate(replay)
        assert replay_stderr == ""
        assert replay["operation"] == "replay"
        assert replay["hashes"]["journal_sha256"] == hashlib.sha256(
            journal.read_bytes()
        ).hexdigest()


if __name__ == "__main__":
    main()
