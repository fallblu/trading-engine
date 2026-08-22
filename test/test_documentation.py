from __future__ import annotations

import importlib.util
import pathlib
import tempfile
import unittest


REPOSITORY_ROOT = pathlib.Path(__file__).resolve().parent.parent
MODULE_PATH = REPOSITORY_ROOT / "scripts" / "check-documentation.py"
SPEC = importlib.util.spec_from_file_location("check_documentation", MODULE_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"cannot load {MODULE_PATH}")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class DocumentationCheckTest(unittest.TestCase):
    def test_reports_missing_and_insecure_links(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            page = root / "page.md"
            page.write_text(
                "[missing](missing.md) [insecure](http://example.com) "
                "[secure](https://example.com)\n",
                encoding="utf-8",
            )

            self.assertEqual(
                MODULE.markdown_link_failures(page, root),
                [
                    f"{page}: missing link target missing.md",
                    f"{page}: insecure external link http://example.com",
                ],
            )

    def test_resolves_generated_directory_links(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = pathlib.Path(directory)
            page = site / "guide" / "index.html"
            target = site / "api" / "index.html"
            page.parent.mkdir()
            target.parent.mkdir()
            page.write_text('<a href="../api/">API</a>', encoding="utf-8")
            target.write_text("API", encoding="utf-8")

            self.assertEqual(MODULE.generated_link_failures(site), [])


if __name__ == "__main__":
    unittest.main()
