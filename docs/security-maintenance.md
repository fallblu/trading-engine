# Security maintenance

Trading Engine combines GitHub security settings, bounded dependency proposals, static analysis,
and repository verification. These controls identify changes for a maintainer to assess. They do
not replace the complete gate, threat analysis, or the human-controlled release process.

Report an undisclosed vulnerability through the private channel in the
[security policy](https://github.com/fallblu/trading-engine/security/policy).

## Dependency updates

Dependabot checks GitHub Actions and the Python documentation and schema tools every Monday. Patch
and minor updates are grouped by ecosystem. Major updates remain separate so their compatibility
impact is visible. At most five version-update pull requests per ecosystem remain open at once.

Version-update pull requests target `develop`. GitHub always targets Dependabot security-update
pull requests at the repository's default branch, which is `main`; `target-branch` cannot change
that behavior. Treat such a pull request as a security warning and hotfix input. Do not merge it
directly as an ordinary feature change. Reproduce the dependency and lockfile change through the
documented hotfix or `develop` integration flow, then use the human-controlled release process.

Review each Python input and generated lockfile together. Run the complete repository gate after a
change to `requirements/docs.in`, `requirements/docs.lock`, `requirements/schema.in`, or
`requirements/schema.lock`.

Dependabot does not support opam manifests. Update `trading_engine.opam` and
`trading_engine.opam.locked` together through a reviewed pull request. The exact locked gate and
the required lowest and highest dependency-band cells must pass. The bands resolve the declared
opam bounds independently, so they catch compatibility errors that one lock cannot.

Dependabot configuration lives on `develop` until the next human release carries it to `main`,
where GitHub reads `.github/dependabot.yml`. Repository vulnerability alerts and automatic
security-fix proposals are enabled independently through GitHub security settings.

## Static and dependency analysis

CodeQL analyzes GitHub Actions workflows and Python build tooling on pull requests and pushes to
`develop` and `main`, on a weekly schedule, and when started manually. Both languages use the
`security-extended` query suite and `none` build mode. The workflow checks out source without
credentials and does not build or execute repository code.

CodeQL does not provide an OCaml extractor. The CodeQL check therefore makes no static-analysis
claim about the reducer, protocol parsers, external process supervisor, or artifact writer. OCaml
assurance comes from compiler warnings, formatting, deterministic tests, schema conformance,
property tests, fixed fuzz corpora, coverage, and dependency-band checks. These are verification
controls, not a substitute for OCaml security review.

Dependency review runs on pull requests to `develop` and `main`. It rejects newly introduced
dependencies represented in GitHub's dependency graph when they have vulnerabilities of moderate
severity or higher. It does not execute pull-request code. GitHub does not natively resolve the
opam lock for this check, so the opam review and CI gates remain required.

## Findings and suppressions

Investigate each CodeQL or dependency-review finding against the affected path and supported
dependency range. Prefer a code fix, dependency update, or constraint change. Record the evidence
and affected versions in the pull request or a linked issue.

Do not add broad query exclusions or advisory allowlists. A narrow suppression requires maintainer
review, a linked tracking issue, a reason such as confirmed false positive or unreachable test
code, and a condition for removal. Use GitHub's finding dismissal controls for CodeQL so the reason
and reviewer remain auditable. Any future dependency-review advisory exception must name one GHSA
and follow the same review rules. Re-run the affected workflow after a fix or suppression change.
