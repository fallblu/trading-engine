from __future__ import annotations

import importlib.util
import os
import pathlib
import shutil
import subprocess
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


class BootstrapEnvironmentTest(unittest.TestCase):
    def test_repairs_a_registered_repository_before_selecting_it(self) -> None:
        result, commands, repository_root = self.run_bootstrap(repository_registered=True)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(
            f"repository set-url trading-engine-coverage {repository_root / 'opam-repository'} --yes",
            commands,
        )
        self.assertIn(
            "repository add trading-engine-coverage --rank 1 "
            f"--switch {repository_root} --yes",
            commands,
        )

    def test_registers_a_missing_repository_with_its_local_url(self) -> None:
        result, commands, repository_root = self.run_bootstrap(repository_registered=False)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(any("repository set-url" in command for command in commands))
        self.assertIn(
            "repository add trading-engine-coverage "
            f"{repository_root / 'opam-repository'} --rank 1 "
            f"--switch {repository_root} --yes",
            commands,
        )

    def run_bootstrap(
        self, *, repository_registered: bool
    ) -> tuple[subprocess.CompletedProcess[str], list[str], pathlib.Path]:
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        repository_root = pathlib.Path(temporary.name)
        scripts = repository_root / "scripts"
        scripts.mkdir()
        bootstrap = scripts / "bootstrap-development-environment"
        shutil.copy2(REPOSITORY_ROOT / "scripts" / bootstrap.name, bootstrap)
        self.write_executable(
            scripts / "check-development-environment", "#!/bin/sh\nexit 0\n"
        )
        (repository_root / "_opam/.opam-switch").mkdir(parents=True)
        (repository_root / "_opam/.opam-switch/switch-config").touch()
        schema_bin = repository_root / ".venv-schema/bin"
        schema_bin.mkdir(parents=True)
        self.write_executable(schema_bin / "python", "#!/bin/sh\nexit 0\n")
        (repository_root / "requirements").mkdir()
        (repository_root / "requirements/schema.lock").touch()
        (repository_root / "opam-repository").mkdir()

        fake_bin = repository_root / "fake-bin"
        fake_bin.mkdir()
        opam_log = repository_root / "opam.log"
        self.write_executable(
            fake_bin / "opam",
            "#!/bin/sh\n"
            'printf \'%s\\n\' "$*" >> "$OPAM_LOG"\n'
            'if [ "$1" = repository ] && [ "$2" = list ] '
            '&& [ "$REPOSITORY_REGISTERED" = 1 ]; then\n'
            "  printf '%s\\n' trading-engine-coverage\n"
            "fi\n",
        )
        self.write_executable(fake_bin / "uv", "#!/bin/sh\nexit 0\n")
        self.write_executable(fake_bin / "python3", "#!/bin/sh\nexit 0\n")
        environment = os.environ.copy()
        environment.update(
            {
                "OPAM_LOG": str(opam_log),
                "PATH": f"{fake_bin}:{environment['PATH']}",
                "REPOSITORY_REGISTERED": "1" if repository_registered else "0",
            }
        )

        result = subprocess.run(
            [str(bootstrap)],
            cwd=repository_root,
            env=environment,
            check=False,
            capture_output=True,
            text=True,
        )
        return result, opam_log.read_text(encoding="utf-8").splitlines(), repository_root

    @staticmethod
    def write_executable(path: pathlib.Path, contents: str) -> None:
        path.write_text(contents, encoding="utf-8")
        path.chmod(0o755)


if __name__ == "__main__":
    unittest.main()
