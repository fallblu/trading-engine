#!/usr/bin/env python3
"""Validate every committed contract branch and the differential corpus."""

from __future__ import annotations

import copy
import hashlib
import json
from pathlib import Path
from typing import Any

from jsonschema.exceptions import ValidationError
from jsonschema.validators import validator_for
from referencing import Registry, Resource


ROOT = Path(__file__).resolve().parents[1]
CONTRACTS = ROOT / "contracts"
CONFORMANCE = CONTRACTS / "conformance"


def loads(document: str, label: str) -> Any:
    def object_pairs(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
        result: dict[str, Any] = {}
        for key, value in pairs:
            if key in result:
                raise ValueError(f"duplicate key {key!r} in {label}")
            result[key] = value
        return result

    return json.loads(
        document,
        object_pairs_hook=object_pairs,
        parse_constant=lambda value: (_ for _ in ()).throw(
            ValueError(f"non-finite number {value!r} in {label}")
        ),
    )


def load(path: Path) -> Any:
    return loads(path.read_text(encoding="utf-8"), str(path.relative_to(ROOT)))


def extract(instance: Any, path: list[str | int]) -> Any | None:
    current = instance
    for component in path:
        if isinstance(component, str) and isinstance(current, dict):
            if component not in current:
                return None
            current = current[component]
        elif isinstance(component, int) and isinstance(current, list):
            current = current[component]
        else:
            raise AssertionError(f"cannot extract {path!r} from {instance!r}")
    return current


def source_instances(source: dict[str, Any]) -> list[tuple[int, Any]]:
    path = CONTRACTS / source["path"]
    if source["format"] == "json":
        records = [(1, load(path))]
    elif source["format"] == "jsonl":
        records = [
            (line_number, loads(line, f"{path.relative_to(ROOT)}:{line_number}"))
            for line_number, line in enumerate(
                path.read_text(encoding="utf-8").splitlines(), start=1
            )
            if line
        ]
    else:
        raise AssertionError(f"unsupported source format {source['format']!r}")
    extraction = source.get("extract", [])
    selected = [
        (line_number, extracted)
        for line_number, record in records
        if (extracted := extract(record, extraction)) is not None
    ]
    if not selected:
        raise AssertionError(f"{source['path']} selected no conformance inputs")
    return selected


def schema_paths() -> set[Path]:
    return {
        path
        for path in CONTRACTS.rglob("*.schema.json")
        if "conformance" not in path.parts
    }


def fixture_paths() -> set[Path]:
    return {
        path
        for path in CONTRACTS.rglob("fixtures/*")
        if path.is_file() and "conformance" not in path.parts
    }


def schema_registry() -> tuple[dict[str, Any], Registry[Any]]:
    schemas = {
        str(path.relative_to(CONTRACTS)): load(path) for path in schema_paths()
    }
    resources = []
    for relative, schema in schemas.items():
        if not isinstance(schema, dict):
            raise AssertionError(f"{relative} must contain a JSON object")
        if "$schema" not in schema or "$id" not in schema:
            raise AssertionError(f"{relative} must declare $schema and $id")
        validator_class = validator_for(schema)
        if schema["$schema"] != validator_class.META_SCHEMA.get("$id"):
            raise AssertionError(
                f"{relative} declares unsupported draft {schema['$schema']!r}"
            )
        validator_class.check_schema(schema)
        resources.append((schema["$id"], Resource.from_contents(schema)))
    registry: Registry[Any] = Registry().with_resources(resources)
    for relative, schema in schemas.items():
        resolver = registry.resolver(schema["$id"])
        for reference in references(schema):
            try:
                resolver.lookup(reference)
            except Exception as error:
                raise AssertionError(
                    f"broken reference {reference!r} in {relative}"
                ) from error
    return schemas, registry


def references(value: Any) -> list[str]:
    if isinstance(value, dict):
        found = [value["$ref"]] if isinstance(value.get("$ref"), str) else []
        return found + [
            reference
            for child in value.values()
            for reference in references(child)
        ]
    if isinstance(value, list):
        return [reference for child in value for reference in references(child)]
    return []


def make_validator(
    schema: dict[str, Any], registry: Registry[Any]
) -> Any:
    validator_class = validator_for(schema)
    return validator_class(
        schema,
        format_checker=validator_class.FORMAT_CHECKER,
        registry=registry,
    )


def expect_invalid(validator: Any, instance: Any, label: str) -> None:
    try:
        validator.validate(instance)
    except ValidationError:
        return
    raise AssertionError(f"{label} unexpectedly satisfied its schema")


def verify_manifest(
    schemas: dict[str, Any], registry: Registry[Any]
) -> tuple[dict[str, dict[str, Any]], dict[str, Any]]:
    manifest = load(CONFORMANCE / "manifest.json")
    if manifest.get("format_version") != "1":
        raise AssertionError("unsupported contract manifest version")
    artifacts = manifest.get("artifacts")
    if not isinstance(artifacts, list) or not artifacts:
        raise AssertionError("contract manifest must list artifacts")
    by_name = {artifact["name"]: artifact for artifact in artifacts}
    if len(by_name) != len(artifacts):
        raise AssertionError("contract manifest artifact names must be unique")

    declared_schemas = {artifact["schema"] for artifact in artifacts}
    discovered_schemas = {
        str(path.relative_to(CONTRACTS)) for path in schema_paths()
    }
    if declared_schemas != discovered_schemas:
        raise AssertionError(
            "contract manifest schema set differs from committed schemas: "
            f"declared={sorted(declared_schemas)} "
            f"committed={sorted(discovered_schemas)}"
        )
    declared_sources = {
        source["path"] for artifact in artifacts for source in artifact["sources"]
    }
    discovered_sources = {
        str(path.relative_to(CONTRACTS)) for path in fixture_paths()
    }
    if declared_sources != discovered_sources:
        raise AssertionError(
            "contract manifest fixture set differs from committed fixtures: "
            f"declared={sorted(declared_sources)} "
            f"committed={sorted(discovered_sources)}"
        )

    for artifact in artifacts:
        schema = schemas[artifact["schema"]]
        validator = make_validator(schema, registry)
        inputs = [
            instance
            for source in artifact["sources"]
            for _, instance in source_instances(source)
        ]
        for index, instance in enumerate(inputs, start=1):
            validator.validate(instance)
            if not isinstance(instance, dict):
                raise AssertionError(f"{artifact['name']} input must be an object")
            version = instance.get(artifact["version_field"])
            if version != artifact["version"]:
                raise AssertionError(
                    f"{artifact['name']} input {index} has version {version!r}"
                )

        representative = inputs[0]
        missing_version = copy.deepcopy(representative)
        del missing_version[artifact["version_field"]]
        expect_invalid(validator, missing_version, f"{artifact['name']} missing version")
        unsupported_version = copy.deepcopy(representative)
        unsupported_version[artifact["version_field"]] = "__unsupported__"
        expect_invalid(
            validator, unsupported_version, f"{artifact['name']} unsupported version"
        )
        unknown_field = copy.deepcopy(representative)
        unknown_field["unexpected_contract_field"] = True
        expect_invalid(validator, unknown_field, f"{artifact['name']} unknown field")
    return by_name, manifest


def resolve_parent(instance: Any, path: list[str | int]) -> tuple[Any, str | int]:
    if not path:
        raise AssertionError("mutation path must not be empty")
    current = instance
    for component in path[:-1]:
        if isinstance(component, str) and isinstance(current, dict):
            current = current[component]
        elif isinstance(component, int) and isinstance(current, list):
            current = current[component]
        else:
            raise AssertionError(f"invalid mutation path {path!r}")
    return current, path[-1]


def apply_mutations(instance: Any, mutations: list[dict[str, Any]]) -> Any:
    result = copy.deepcopy(instance)
    for mutation in mutations:
        operation = mutation["op"]
        path = mutation["path"]
        if operation == "append_copy":
            target = extract(result, path)
            if not isinstance(target, list):
                raise AssertionError(f"append_copy target {path!r} is not an array")
            target.append(copy.deepcopy(target[mutation["index"]]))
            continue
        parent, component = resolve_parent(result, path)
        if operation == "remove":
            if not isinstance(parent, dict) or not isinstance(component, str):
                raise AssertionError("remove currently requires an object field")
            del parent[component]
        elif operation in {"add", "replace"}:
            value = copy.deepcopy(mutation["value"])
            if isinstance(parent, dict) and isinstance(component, str):
                parent[component] = value
            elif isinstance(parent, list) and isinstance(component, int):
                parent[component] = value
            else:
                raise AssertionError(f"invalid mutation target {path!r}")
        else:
            raise AssertionError(f"unsupported mutation operation {operation!r}")
    return result


def case_instances(case: dict[str, Any]) -> list[Any]:
    if "instance" in case:
        return [apply_mutations(case["instance"], case["mutations"])]
    source = {
        "path": case["source"],
        "format": "jsonl" if case["source"].endswith(".jsonl") else "json",
        "extract": case.get("extract", []),
    }
    inputs = source_instances(source)
    record = case.get("record")
    if record is not None:
        inputs = [instance for line, instance in inputs if line == record]
        if len(inputs) != 1:
            raise AssertionError(
                f"{case['name']} did not select exactly one source record"
            )
    else:
        inputs = [instance for _, instance in inputs]
    mutations = case["mutations"]
    if mutations and len(inputs) != 1:
        raise AssertionError(f"{case['name']} mutates more than one source record")
    return [apply_mutations(instance, mutations) for instance in inputs]


def verify_cases(
    artifacts: dict[str, dict[str, Any]],
    schemas: dict[str, Any],
    registry: Registry[Any],
) -> dict[str, list[Any]]:
    corpus = load(CONFORMANCE / "cases.json")
    if corpus.get("format_version") != "1":
        raise AssertionError("unsupported differential corpus version")
    cases = corpus.get("cases")
    if not isinstance(cases, list) or not cases:
        raise AssertionError("differential corpus must list cases")
    names = {case["name"] for case in cases}
    if len(names) != len(cases):
        raise AssertionError("differential case names must be unique")
    schema_only_cases = corpus.get("schema_only_cases")
    if not isinstance(schema_only_cases, list):
        raise AssertionError("differential corpus must list schema-only cases")
    all_cases = [*cases, *schema_only_cases]
    all_names = {case["name"] for case in all_cases}
    if len(all_names) != len(all_cases):
        raise AssertionError("all conformance case names must be unique")
    accepted_by_artifact: dict[str, list[Any]] = {}
    for case in all_cases:
        artifact = artifacts[case["artifact"]]
        validator = make_validator(schemas[artifact["schema"]], registry)
        inputs = case_instances(case)
        schema_accepts = True
        try:
            for instance in inputs:
                validator.validate(instance)
        except ValidationError:
            schema_accepts = False
        expected_schema = case["schema_expectation"] == "accept"
        if schema_accepts != expected_schema:
            raise AssertionError(
                f"{case['name']} schema expectation was "
                f"{case['schema_expectation']}"
            )
        if schema_accepts:
            accepted_by_artifact.setdefault(case["artifact"], []).extend(inputs)
        if case in schema_only_cases:
            continue
        schema_expectation = case["schema_expectation"]
        runtime_expectation = case["runtime_expectation"]
        if case["rule"] == "structural" and schema_expectation != runtime_expectation:
            raise AssertionError(
                f"{case['name']} structural expectations must agree"
            )
        if case["rule"] == "semantic" and (
            schema_expectation != "accept" or runtime_expectation != "reject"
        ):
            raise AssertionError(
                f"{case['name']} semantic case must document schema accept/runtime reject"
            )
    return accepted_by_artifact


def verify_top_level_branches(
    artifacts: dict[str, dict[str, Any]],
    schemas: dict[str, Any],
    registry: Registry[Any],
    accepted_cases: dict[str, list[Any]],
) -> None:
    for artifact_name, artifact in artifacts.items():
        schema = schemas[artifact["schema"]]
        branches = schema.get("oneOf", [])
        if not branches:
            continue
        candidates = [
            instance
            for source in artifact["sources"]
            for _, instance in source_instances(source)
        ]
        candidates.extend(accepted_cases.get(artifact_name, []))
        full_validator = make_validator(schema, registry)
        for branch_index, branch in enumerate(branches, start=1):
            branch_schema = copy.deepcopy(schema)
            branch_schema["oneOf"] = [branch]
            branch_validator = make_validator(branch_schema, registry)
            witness = next(
                (candidate for candidate in candidates if branch_validator.is_valid(candidate)),
                None,
            )
            if witness is None:
                raise AssertionError(
                    f"{artifact_name} oneOf branch {branch_index} has no positive case"
                )
            if not isinstance(witness, dict):
                raise AssertionError(
                    f"{artifact_name} oneOf branch {branch_index} is not an object"
                )
            negative = copy.deepcopy(witness)
            negative["unexpected_contract_field"] = True
            expect_invalid(
                full_validator,
                negative,
                f"{artifact_name} oneOf branch {branch_index} negative case",
            )


def verify_frozen_integrity() -> None:
    ledger_path = CONFORMANCE / "frozen.sha256"
    expected: dict[str, str] = {}
    for line_number, line in enumerate(
        ledger_path.read_text(encoding="utf-8").splitlines(), start=1
    ):
        try:
            digest, relative = line.split("  ", maxsplit=1)
        except ValueError as error:
            raise AssertionError(
                f"{ledger_path.relative_to(ROOT)}:{line_number} is malformed"
            ) from error
        if relative in expected:
            raise AssertionError(f"duplicate frozen artifact {relative}")
        expected[relative] = digest
    frozen_roots = [
        CONTRACTS / "v1",
        CONTRACTS / "v2",
        CONTRACTS / "strategy" / "v1",
        CONTRACTS / "strategy" / "v2",
    ]
    discovered = {
        str(path.relative_to(ROOT))
        for root in frozen_roots
        for path in root.rglob("*")
        if path.is_file()
        and (path.name.endswith(".schema.json") or "fixtures" in path.parts)
    }
    if set(expected) != discovered:
        raise AssertionError(
            "frozen integrity ledger differs from archived artifacts: "
            f"ledger={sorted(expected)} archived={sorted(discovered)}"
        )
    for relative, digest in expected.items():
        actual = hashlib.sha256((ROOT / relative).read_bytes()).hexdigest()
        if actual != digest:
            raise AssertionError(
                f"frozen artifact changed: {relative}; "
                "update frozen.sha256 only for an intentional contract revision"
            )


def main() -> None:
    schemas, registry = schema_registry()
    artifacts, _ = verify_manifest(schemas, registry)
    accepted_cases = verify_cases(artifacts, schemas, registry)
    verify_top_level_branches(artifacts, schemas, registry, accepted_cases)
    verify_frozen_integrity()


if __name__ == "__main__":
    main()
