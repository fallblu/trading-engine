from __future__ import annotations

import importlib.metadata
import pathlib
import re
import sys


PIN = re.compile(r"^([A-Za-z0-9_.-]+)==([^\s;]+)$")


def normalized(name: str) -> str:
    return re.sub(r"[-_.]+", "-", name).lower()


def locked_versions(lock_path: pathlib.Path) -> dict[str, str]:
    locked: dict[str, str] = {}
    for raw_line in lock_path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        match = PIN.fullmatch(line)
        if match is not None:
            locked[normalized(match.group(1))] = match.group(2)
    if not locked:
        raise ValueError(f"no pinned dependencies found in {lock_path}")
    return locked


def installed_versions() -> dict[str, str]:
    installed: dict[str, str] = {}
    for distribution in importlib.metadata.distributions():
        name = distribution.metadata.get("Name")
        if name is not None:
            installed[normalized(name)] = distribution.version
    return installed


def dependency_differences(
    locked: dict[str, str], installed: dict[str, str]
) -> list[str]:
    differences = [
        f"{name}: expected {version}, found {installed.get(name, 'missing')}"
        for name, version in sorted(locked.items())
        if installed.get(name) != version
    ]
    unexpected = sorted(set(installed) - set(locked))
    differences.extend(f"{name}: installed but not locked" for name in unexpected)
    return differences


def main() -> int:
    repository_root = pathlib.Path(__file__).resolve().parent.parent
    lock_path = repository_root / "requirements" / "schema.lock"
    try:
        locked = locked_versions(lock_path)
    except (OSError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1

    installed = installed_versions()
    differences = dependency_differences(locked, installed)
    if differences:
        print("error: schema dependency mismatch:", file=sys.stderr)
        for difference in differences:
            print(f"  - {difference}", file=sys.stderr)
        return 1

    try:
        from jsonschema import (  # noqa: PLC0415
            Draft4Validator,
            Draft6Validator,
            Draft7Validator,
            Draft201909Validator,
            Draft202012Validator,
            FormatChecker,
        )

        validators = (
            Draft4Validator,
            Draft6Validator,
            Draft7Validator,
            Draft201909Validator,
            Draft202012Validator,
        )
        if not validators or not FormatChecker.checkers:
            raise RuntimeError("JSON Schema validators or format checkers are unavailable")
    except (ImportError, RuntimeError) as error:
        print(f"error: incomplete JSON Schema installation: {error}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
