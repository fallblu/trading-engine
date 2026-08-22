"""Generate and verify deterministic release-candidate metadata."""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import re
import subprocess
import tarfile
import tempfile
from pathlib import Path, PurePosixPath
from urllib.parse import quote


OPAM_DEPENDENCY = re.compile(
    r'^\s*"(?P<name>[^"]+)"\s+\{=\s+"(?P<version>[^"]+)"'
)
PYTHON_DEPENDENCY = re.compile(
    r"^(?P<name>[A-Za-z0-9_.-]+)==(?P<version>[^\s;]+)$"
)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def artifact_names(version: str, target: str) -> dict[str, str]:
    stem = f"trading-engine-{version}"
    return {
        "binary": f"{stem}-{target}.tar.gz",
        "source": f"{stem}-source.tar.gz",
        "contracts": f"{stem}-contracts.tar.gz",
        "documentation": f"{stem}-documentation.tar.gz",
        "opam": f"{stem}.opam",
    }


def parse_opam_dependencies(path: Path) -> list[tuple[str, str, str]]:
    dependencies = []
    for line in path.read_text(encoding="utf-8").splitlines():
        match = OPAM_DEPENDENCY.match(line)
        if match:
            dependencies.append(
                ("opam", match.group("name"), match.group("version"))
            )
    return dependencies


def parse_python_dependencies(path: Path) -> list[tuple[str, str, str]]:
    dependencies = []
    for line in path.read_text(encoding="utf-8").splitlines():
        match = PYTHON_DEPENDENCY.match(line)
        if match:
            dependencies.append(
                ("pypi", match.group("name").lower(), match.group("version"))
            )
    return dependencies


def dependency_inventory(root: Path) -> list[tuple[str, str, str]]:
    dependencies = parse_opam_dependencies(root / "trading_engine.opam.locked")
    dependencies.extend(parse_python_dependencies(root / "requirements/docs.lock"))
    dependencies.extend(parse_python_dependencies(root / "requirements/schema.lock"))
    return sorted(set(dependencies))


def spdx_id(ecosystem: str, name: str, version: str) -> str:
    value = re.sub(r"[^A-Za-z0-9.-]", "-", f"{ecosystem}-{name}-{version}")
    return f"SPDXRef-Package-{value}"


def generate_sbom(
    root: Path,
    version: str,
    target: str,
    revision: str,
    epoch: int,
) -> dict[str, object]:
    project_id = "SPDXRef-Package-trading-engine"
    packages: list[dict[str, object]] = [
        {
            "SPDXID": project_id,
            "name": "trading-engine",
            "versionInfo": version,
            "downloadLocation": "NOASSERTION",
            "filesAnalyzed": False,
            "licenseConcluded": "MIT",
            "licenseDeclared": "MIT",
            "copyrightText": "NOASSERTION",
            "primaryPackagePurpose": "APPLICATION",
            "externalRefs": [
                {
                    "referenceCategory": "PACKAGE-MANAGER",
                    "referenceType": "purl",
                    "referenceLocator": f"pkg:opam/trading_engine@{quote(version)}",
                }
            ],
        }
    ]
    relationships: list[dict[str, str]] = [
        {
            "spdxElementId": "SPDXRef-DOCUMENT",
            "relationshipType": "DESCRIBES",
            "relatedSpdxElement": project_id,
        }
    ]
    for ecosystem, name, dependency_version in dependency_inventory(root):
        dependency_id = spdx_id(ecosystem, name, dependency_version)
        packages.append(
            {
                "SPDXID": dependency_id,
                "name": name,
                "versionInfo": dependency_version,
                "downloadLocation": "NOASSERTION",
                "filesAnalyzed": False,
                "licenseConcluded": "NOASSERTION",
                "licenseDeclared": "NOASSERTION",
                "copyrightText": "NOASSERTION",
                "primaryPackagePurpose": "LIBRARY",
                "externalRefs": [
                    {
                        "referenceCategory": "PACKAGE-MANAGER",
                        "referenceType": "purl",
                        "referenceLocator": (
                            f"pkg:{ecosystem}/{quote(name)}@{quote(dependency_version)}"
                        ),
                    }
                ],
            }
        )
        if ecosystem == "opam":
            relationships.append(
                {
                    "spdxElementId": project_id,
                    "relationshipType": "DEPENDS_ON",
                    "relatedSpdxElement": dependency_id,
                }
            )
        else:
            relationships.append(
                {
                    "spdxElementId": dependency_id,
                    "relationshipType": "BUILD_DEPENDENCY_OF",
                    "relatedSpdxElement": project_id,
                }
            )

    created = dt.datetime.fromtimestamp(epoch, tz=dt.timezone.utc).isoformat()
    return {
        "spdxVersion": "SPDX-2.3",
        "dataLicense": "CC0-1.0",
        "SPDXID": "SPDXRef-DOCUMENT",
        "name": f"trading-engine-{version}-{target}",
        "documentNamespace": (
            "https://github.com/fallblu/trading-engine/releases/sbom/"
            f"{revision}/{target}"
        ),
        "creationInfo": {
            "created": created.replace("+00:00", "Z"),
            "creators": ["Tool: scripts/release_artifacts.py"],
            "licenseListVersion": "3.27.0",
        },
        "packages": packages,
        "relationships": relationships,
    }


def write_json(path: Path, value: object) -> None:
    path.write_text(
        json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )


def checksum_lines(directory: Path, names: list[str]) -> str:
    return "".join(f"{sha256(directory / name)}  {name}\n" for name in sorted(names))


def generate_metadata(
    root: Path,
    output: Path,
    version: str,
    target: str,
    revision: str,
    epoch: int,
) -> None:
    names = artifact_names(version, target)
    missing = [name for name in names.values() if not (output / name).is_file()]
    if missing:
        raise ValueError(f"release subjects are missing: {', '.join(missing)}")

    artifacts = [
        {
            "name": names["binary"],
            "role": "installable binary and OCaml package",
            "contents": ["bin", "lib", "share/trading_engine/contracts", "doc"],
        },
        {
            "name": names["source"],
            "role": "Git source",
            "contents": ["tracked repository files at sourceRevision"],
        },
        {
            "name": names["contracts"],
            "role": "versioned contracts",
            "contents": ["contract READMEs", "schemas", "fixtures", "conformance"],
        },
        {
            "name": names["documentation"],
            "role": "offline documentation site",
            "contents": ["project guides", "versioned contracts", "generated OCaml API"],
        },
        {
            "name": names["opam"],
            "role": "opam package definition",
            "contents": ["trading_engine.opam"],
        },
    ]
    subject_names = sorted(names.values())
    subject_digests = {name: sha256(output / name) for name in subject_names}
    manifest = {
        "schemaVersion": 1,
        "project": "fallblu/trading-engine",
        "version": version,
        "target": target,
        "sourceRevision": revision,
        "sourceDateEpoch": epoch,
        "artifacts": [
            {**artifact, "sha256": subject_digests[artifact["name"]]}
            for artifact in artifacts
        ],
    }
    write_json(output / "release-manifest.json", manifest)
    write_json(
        output / "sbom.spdx.json",
        generate_sbom(root, version, target, revision, epoch),
    )

    statement = {
        "_type": "https://in-toto.io/Statement/v1",
        "subject": [
            {"name": name, "digest": {"sha256": subject_digests[name]}}
            for name in subject_names
        ],
        "predicateType": "https://slsa.dev/provenance/v1",
        "predicate": {
            "buildDefinition": {
                "buildType": (
                    "https://github.com/fallblu/trading-engine/blob/develop/"
                    "docs/release-artifacts.md#deterministic-build"
                ),
                "externalParameters": {"version": version, "target": target},
                "internalParameters": {"sourceDateEpoch": epoch},
                "resolvedDependencies": [
                    {
                        "uri": "git+https://github.com/fallblu/trading-engine",
                        "digest": {"gitCommit": revision},
                    },
                    *[
                        {
                            "uri": path,
                            "digest": {"sha256": sha256(root / path)},
                        }
                        for path in (
                            "trading_engine.opam.locked",
                            "requirements/docs.lock",
                            "requirements/schema.lock",
                        )
                    ],
                ],
            },
            "runDetails": {
                "builder": {
                    "id": (
                        "https://github.com/fallblu/trading-engine/"
                        ".github/workflows/release-candidate.yml"
                    )
                }
            },
        },
    }
    (output / "provenance.intoto.jsonl").write_text(
        json.dumps(statement, sort_keys=True, separators=(",", ":")) + "\n",
        encoding="utf-8",
    )
    (output / "SUBJECTS.sha256").write_text(
        checksum_lines(output, subject_names), encoding="utf-8"
    )
    checksum_names = subject_names + [
        "SUBJECTS.sha256",
        "provenance.intoto.jsonl",
        "release-manifest.json",
        "sbom.spdx.json",
    ]
    (output / "SHA256SUMS").write_text(
        checksum_lines(output, checksum_names), encoding="utf-8"
    )


def parse_checksums(path: Path) -> dict[str, str]:
    checksums: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        digest, separator, name = line.partition("  ")
        if not separator or not re.fullmatch(r"[0-9a-f]{64}", digest):
            raise ValueError(f"invalid checksum line in {path.name}: {line}")
        if name in checksums:
            raise ValueError(f"duplicate checksum subject in {path.name}: {name}")
        checksums[name] = digest
    return checksums


def verify_checksums(directory: Path, filename: str) -> dict[str, str]:
    checksums = parse_checksums(directory / filename)
    for name, expected in checksums.items():
        path = directory / name
        if not path.is_file():
            raise ValueError(f"{filename} references missing file: {name}")
        actual = sha256(path)
        if actual != expected:
            raise ValueError(f"{filename} digest mismatch for {name}")
    return checksums


def verify_archive(
    path: Path, prefix: str, required: tuple[str, ...], epoch: int
) -> None:
    with tarfile.open(path, "r:gz") as archive:
        names = set()
        for member in archive.getmembers():
            pure = PurePosixPath(member.name)
            if pure.is_absolute() or ".." in pure.parts:
                raise ValueError(f"unsafe archive member in {path.name}: {member.name}")
            if member.uid != 0 or member.gid != 0 or member.mtime != epoch:
                raise ValueError(f"nondeterministic archive metadata in {path.name}: {member.name}")
            names.add(member.name.rstrip("/"))
    for relative in required:
        expected = f"{prefix}/{relative}".rstrip("/")
        if expected not in names:
            raise ValueError(f"{path.name} is missing {expected}")


def verify_release(
    root: Path, output: Path, version: str, target: str, revision: str, epoch: int
) -> None:
    names = artifact_names(version, target)
    expected_files = set(names.values()) | {
        "SHA256SUMS",
        "SUBJECTS.sha256",
        "provenance.intoto.jsonl",
        "release-manifest.json",
        "sbom.spdx.json",
    }
    actual_files = {path.name for path in output.iterdir() if path.is_file()}
    if actual_files != expected_files:
        raise ValueError(
            f"release file set differs: expected {sorted(expected_files)}, "
            f"found {sorted(actual_files)}"
        )

    subjects = verify_checksums(output, "SUBJECTS.sha256")
    if set(subjects) != set(names.values()):
        raise ValueError("SUBJECTS.sha256 does not describe the distributable set")
    checksums = verify_checksums(output, "SHA256SUMS")
    if set(checksums) != expected_files - {"SHA256SUMS"}:
        raise ValueError("SHA256SUMS does not describe every release file")

    package = f"trading-engine-{version}"
    verify_archive(
        output / names["binary"],
        package,
        (
            "bin/trading-engine",
            "lib/trading_engine/opam",
            "share/trading_engine/contracts/v16/scenario.schema.json",
            "share/trading_engine/contracts/v16/fixtures/demo.scenario.json",
            "doc/trading_engine/README.md",
        ),
        epoch,
    )
    verify_archive(
        output / names["source"],
        package,
        (
            "trading_engine.opam",
            "contracts/v1/scenario.schema.json",
            "contracts/v16/fixtures/demo.scenario.json",
            "docs/architecture.md",
            ".github/workflows/release-candidate.yml",
        ),
        epoch,
    )
    verify_archive(
        output / names["contracts"],
        package,
        (
            "contracts/conformance/manifest.json",
            "contracts/v1/scenario.schema.json",
            "contracts/v16/fixtures/demo.scenario.json",
            "contracts/strategy/v14/message.schema.json",
        ),
        epoch,
    )
    verify_archive(
        output / names["documentation"],
        package,
        (
            "index.html",
            "docs/architecture/index.html",
            "contracts/v1/index.html",
            "contracts/v16/scenario.schema.json",
            "api/trading_engine/Trading_engine/index.html",
        ),
        epoch,
    )

    if (output / names["opam"]).read_bytes() != (root / "trading_engine.opam").read_bytes():
        raise ValueError("opam artifact differs from trading_engine.opam")

    manifest = json.loads((output / "release-manifest.json").read_text(encoding="utf-8"))
    if (
        manifest.get("version") != version
        or manifest.get("target") != target
        or manifest.get("sourceRevision") != revision
        or manifest.get("sourceDateEpoch") != epoch
    ):
        raise ValueError("release manifest identity differs from the requested build")

    sbom = json.loads((output / "sbom.spdx.json").read_text(encoding="utf-8"))
    if sbom.get("spdxVersion") != "SPDX-2.3":
        raise ValueError("SBOM is not SPDX 2.3")
    if not any(
        package_entry.get("name") == "trading-engine"
        and package_entry.get("versionInfo") == version
        for package_entry in sbom.get("packages", [])
    ):
        raise ValueError("SBOM does not describe the project package")

    statement = json.loads(
        (output / "provenance.intoto.jsonl").read_text(encoding="utf-8")
    )
    if (
        statement.get("_type") != "https://in-toto.io/Statement/v1"
        or statement.get("predicateType") != "https://slsa.dev/provenance/v1"
    ):
        raise ValueError("provenance is not an in-toto SLSA v1 statement")
    provenance_subjects = {
        subject["name"]: subject["digest"]["sha256"]
        for subject in statement.get("subject", [])
    }
    if provenance_subjects != subjects:
        raise ValueError("provenance subjects differ from SUBJECTS.sha256")

    with tempfile.TemporaryDirectory() as directory:
        destination = Path(directory)
        with tarfile.open(output / names["binary"], "r:gz") as archive:
            archive.extractall(destination, filter="data")
        executable = destination / package / "bin" / "trading-engine"
        result = subprocess.run(
            [executable, "--capabilities"],
            check=True,
            capture_output=True,
            text=True,
        )
        capabilities = json.loads(result.stdout)
        if capabilities.get("engine_version") != version:
            raise ValueError("installed binary reports a different version")


def main() -> None:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)
    for command in ("generate", "verify"):
        child = subparsers.add_parser(command)
        child.add_argument("--root", type=Path, required=True)
        child.add_argument("--output", type=Path, required=True)
        child.add_argument("--version", required=True)
        child.add_argument("--target", required=True)
        child.add_argument("--revision", required=True)
        child.add_argument("--epoch", type=int, required=True)
    args = parser.parse_args()
    root = args.root.resolve()
    output = args.output.resolve()
    if args.command == "generate":
        generate_metadata(
            root, output, args.version, args.target, args.revision, args.epoch
        )
    else:
        verify_release(
            root, output, args.version, args.target, args.revision, args.epoch
        )


if __name__ == "__main__":
    main()
