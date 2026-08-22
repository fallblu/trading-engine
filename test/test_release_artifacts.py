from __future__ import annotations

import importlib.util
import json
import pathlib
import tempfile
import unittest


REPOSITORY_ROOT = pathlib.Path(__file__).resolve().parent.parent
MODULE_PATH = REPOSITORY_ROOT / "scripts" / "release_artifacts.py"
SPEC = importlib.util.spec_from_file_location("release_artifacts", MODULE_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"cannot load {MODULE_PATH}")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class ReleaseArtifactsTest(unittest.TestCase):
    def create_root(self, directory: pathlib.Path) -> pathlib.Path:
        root = directory / "repository"
        (root / "requirements").mkdir(parents=True)
        (root / "trading_engine.opam.locked").write_text(
            'depends: [\n  "ocaml" {= "5.5.0"}\n]\n', encoding="utf-8"
        )
        (root / "requirements" / "docs.lock").write_text(
            "mkdocs==1.6.1\n", encoding="utf-8"
        )
        (root / "requirements" / "schema.lock").write_text(
            "jsonschema==4.26.0\n", encoding="utf-8"
        )
        return root

    def test_generates_deterministic_spdx_and_provenance(self) -> None:
        with tempfile.TemporaryDirectory() as directory_name:
            directory = pathlib.Path(directory_name)
            root = self.create_root(directory)
            output = directory / "release"
            output.mkdir()
            names = MODULE.artifact_names("1.2.3", "linux-x86_64")
            for index, name in enumerate(names.values()):
                (output / name).write_bytes(f"artifact-{index}".encode())

            arguments = (root, output, "1.2.3", "linux-x86_64", "a" * 40, 1234)
            MODULE.generate_metadata(*arguments)
            first = {path.name: path.read_bytes() for path in output.iterdir()}
            MODULE.generate_metadata(*arguments)
            second = {path.name: path.read_bytes() for path in output.iterdir()}

            self.assertEqual(first, second)
            self.assertEqual(
                set(MODULE.verify_checksums(output, "SUBJECTS.sha256")),
                set(names.values()),
            )
            sbom = json.loads((output / "sbom.spdx.json").read_text())
            self.assertEqual(sbom["spdxVersion"], "SPDX-2.3")
            self.assertEqual(
                {package["name"] for package in sbom["packages"]},
                {"trading-engine", "ocaml", "mkdocs", "jsonschema"},
            )
            provenance = json.loads(
                (output / "provenance.intoto.jsonl").read_text()
            )
            self.assertEqual(
                provenance["predicateType"], "https://slsa.dev/provenance/v1"
            )

    def test_rejects_duplicate_and_mismatched_checksums(self) -> None:
        with tempfile.TemporaryDirectory() as directory_name:
            directory = pathlib.Path(directory_name)
            artifact = directory / "artifact"
            artifact.write_text("contents", encoding="utf-8")
            digest = MODULE.sha256(artifact)
            checksums = directory / "SHA256SUMS"
            checksums.write_text(
                f"{digest}  artifact\n{digest}  artifact\n", encoding="utf-8"
            )
            with self.assertRaisesRegex(ValueError, "duplicate checksum"):
                MODULE.verify_checksums(directory, "SHA256SUMS")

            checksums.write_text(f"{'0' * 64}  artifact\n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "digest mismatch"):
                MODULE.verify_checksums(directory, "SHA256SUMS")


if __name__ == "__main__":
    unittest.main()
