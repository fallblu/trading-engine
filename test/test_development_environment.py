from __future__ import annotations

import importlib.util
import pathlib
import tempfile
import unittest


REPOSITORY_ROOT = pathlib.Path(__file__).resolve().parent.parent
MODULE_PATH = REPOSITORY_ROOT / "scripts" / "check-schema-environment.py"
SPEC = importlib.util.spec_from_file_location("check_schema_environment", MODULE_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"cannot load {MODULE_PATH}")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class SchemaEnvironmentTest(unittest.TestCase):
    def test_reads_exact_pins_and_normalizes_names(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            lock_path = pathlib.Path(directory) / "schema.lock"
            lock_path.write_text(
                "# generated\nTyping_Extensions==4.16.0\njsonschema==4.26.0\n",
                encoding="utf-8",
            )

            self.assertEqual(
                MODULE.locked_versions(lock_path),
                {"typing-extensions": "4.16.0", "jsonschema": "4.26.0"},
            )

    def test_rejects_a_lock_without_exact_pins(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            lock_path = pathlib.Path(directory) / "schema.lock"
            lock_path.write_text("jsonschema>=4\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "no pinned dependencies"):
                MODULE.locked_versions(lock_path)

    def test_reports_missing_mismatched_and_unexpected_packages(self) -> None:
        differences = MODULE.dependency_differences(
            {"attrs": "26.1.0", "jsonschema": "4.26.0"},
            {"attrs": "25.0.0", "extra": "1.0.0"},
        )

        self.assertEqual(
            differences,
            [
                "attrs: expected 26.1.0, found 25.0.0",
                "jsonschema: expected 4.26.0, found missing",
                "extra: installed but not locked",
            ],
        )


if __name__ == "__main__":
    unittest.main()
