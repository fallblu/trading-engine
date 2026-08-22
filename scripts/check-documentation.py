"""Validate documentation sources and generated site topology."""

from __future__ import annotations

import filecmp
import re
import sys
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit


REPOSITORY_ROOT = Path(__file__).resolve().parent.parent
LINK = re.compile(r"(?<!!)\[[^\]]+\]\(([^)]+)\)")
PUBLIC_MODULE_EXCLUSIONS = {
    "reducer_phases",
    "scenario_shape",
    "scenario_validation",
}
REQUIRED_NAVIGATION = (
    "index.md",
    "docs/architecture.md",
    "docs/execution-model.md",
    "docs/scenario.md",
    "docs/diagnostics.md",
    "docs/persistra.md",
    "SECURITY.md",
    "contracts/conformance/README.md",
    "contracts/v4/README.md",
    "contracts/v3/README.md",
    "contracts/v2/README.md",
    "contracts/v1/README.md",
    "contracts/strategy/v3/README.md",
    "contracts/strategy/v2/README.md",
    "contracts/strategy/v1/README.md",
    "docs/api-reference.md",
    "docs/continuous-integration.md",
    "docs/coverage.md",
    "docs/performance.md",
    "docs/reducer-property-testing.md",
    "docs/fuzzing.md",
    "docs/documentation-platform.md",
    "docs/release-artifacts.md",
    "docs/security-maintenance.md",
    "CONTRIBUTING.md",
    "SUPPORT.md",
    "CHANGELOG.md",
)


class _AnchorParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.links: list[str] = []

    def handle_starttag(
        self, tag: str, attrs: list[tuple[str, str | None]]
    ) -> None:
        if tag != "a":
            return
        for name, value in attrs:
            if name == "href" and value is not None:
                self.links.append(value)


def source_markdown_files(root: Path = REPOSITORY_ROOT) -> tuple[Path, ...]:
    """Return every Markdown source that belongs to the published site."""
    fixed = (
        root / "README.md",
        root / "CONTRIBUTING.md",
        root / "CHANGELOG.md",
        root / ".github" / "SECURITY.md",
        root / ".github" / "SUPPORT.md",
    )
    discovered = tuple(sorted((root / "docs").rglob("*.md"))) + tuple(
        sorted((root / "contracts").rglob("README.md"))
    )
    return fixed + discovered


def markdown_link_failures(path: Path, root: Path = REPOSITORY_ROOT) -> list[str]:
    """Return actionable failures for repository-relative Markdown links."""
    failures: list[str] = []
    for target in LINK.findall(path.read_text(encoding="utf-8")):
        parsed = urlsplit(target)
        if parsed.scheme == "http":
            failures.append(f"{path}: insecure external link {target}")
            continue
        if parsed.scheme or parsed.netloc or target.startswith("mailto:"):
            continue
        clean = unquote(parsed.path)
        if not clean:
            continue
        if path == root / "docs" / "api-reference.md" and clean.startswith("../api/"):
            continue
        resolved = (path.parent / clean).resolve()
        if not resolved.is_file():
            failures.append(f"{path}: missing link target {target}")
    return failures


def source_failures(root: Path = REPOSITORY_ROOT) -> list[str]:
    """Validate source topology, navigation, and local links."""
    failures: list[str] = []
    config = (root / "mkdocs.yml").read_text(encoding="utf-8")
    for relative in REQUIRED_NAVIGATION:
        if relative not in config:
            failures.append(f"mkdocs.yml: navigation is missing {relative}")
    for path in source_markdown_files(root):
        if not path.is_file():
            failures.append(f"missing documentation source: {path}")
            continue
        failures.extend(markdown_link_failures(path, root))
    api_page = root / "docs" / "api-reference.md"
    if "<!-- generated-api-link -->" not in api_page.read_text(encoding="utf-8"):
        failures.append("docs/api-reference.md: generated API marker is missing")
    return failures


def _site_target(site: Path, page: Path, href: str) -> Path | None:
    parsed = urlsplit(href)
    if parsed.scheme or parsed.netloc or href.startswith(("mailto:", "javascript:")):
        return None
    clean = unquote(parsed.path)
    if not clean:
        return None
    if clean.startswith("/trading-engine/"):
        target = site / clean.removeprefix("/trading-engine/")
    elif clean.startswith("/"):
        return site / "__invalid_absolute_path__"
    else:
        target = (page.parent / clean).resolve()
    if clean.endswith("/") or not target.suffix:
        target /= "index.html"
    return target


def generated_link_failures(site: Path) -> list[str]:
    """Return broken local links from generated HTML pages."""
    failures: list[str] = []
    for page in sorted(site.rglob("*.html")):
        parser = _AnchorParser()
        parser.feed(page.read_text(encoding="utf-8"))
        for href in parser.links:
            target = _site_target(site, page, href)
            if target is not None and not target.is_file():
                failures.append(f"{page.relative_to(site)}: broken generated link {href}")
    return failures


def site_failures(root: Path = REPOSITORY_ROOT) -> list[str]:
    """Validate generated pages, public modules, and exact contract assets."""
    site = root / "site"
    failures: list[str] = []
    required_pages = (
        site / "index.html",
        site / "docs" / "architecture" / "index.html",
        site / "docs" / "execution-model" / "index.html",
        site / "docs" / "scenario" / "index.html",
        site / "docs" / "security-maintenance" / "index.html",
        site / "SECURITY" / "index.html",
        site / "docs" / "api-reference" / "index.html",
        site / "contracts" / "v4" / "index.html",
        site / "contracts" / "v3" / "index.html",
        site / "contracts" / "v2" / "index.html",
        site / "contracts" / "v1" / "index.html",
        site / "api" / "trading_engine" / "Trading_engine" / "index.html",
    )
    for page in required_pages:
        if not page.is_file():
            failures.append(f"generated documentation is missing {page.relative_to(site)}")

    api_root = site / "api" / "trading_engine" / "Trading_engine"
    for interface in sorted((root / "lib").glob("*.mli")):
        if interface.stem in PUBLIC_MODULE_EXCLUSIONS:
            continue
        module = interface.stem.capitalize()
        page = api_root / module / "index.html"
        if not page.is_file():
            failures.append(f"generated API is missing public module {module}")

    for source in sorted((root / "contracts").rglob("*")):
        if not source.is_file() or source.suffix not in {".json", ".jsonl"}:
            continue
        published = site / source.relative_to(root)
        if not published.is_file():
            failures.append(f"published contracts are missing {source.relative_to(root)}")
        elif not filecmp.cmp(source, published, shallow=False):
            failures.append(f"published contract differs from {source.relative_to(root)}")

    failures.extend(generated_link_failures(site))
    return failures


def main(argv: list[str]) -> int:
    if len(argv) != 2 or argv[1] not in {"source", "site"}:
        print("usage: check-documentation.py source|site", file=sys.stderr)
        return 2
    failures = source_failures() if argv[1] == "source" else site_failures()
    if failures:
        print("\n".join(failures), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
