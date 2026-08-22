# Security policy

## Supported versions

Trading Engine provides security fixes for the latest patch release in the current release line.

| Release line | Supported |
| --- | --- |
| Latest 1.0.x patch | Yes |
| Earlier releases | No |

The `develop` branch contains unreleased work and is not a supported release. A fix is staged there
or on a hotfix branch according to the repository's release workflow. This table is updated when a
new release line becomes supported.

## Report a vulnerability privately

Use [GitHub private vulnerability reporting](https://github.com/fallblu/trading-engine/security/advisories/new)
to report a suspected vulnerability. Do not open a public issue for an undisclosed vulnerability.

A useful report includes the affected version or commit, security impact, trigger conditions, a
minimal sanitized reproduction, operating-system and dependency versions, and any disclosure
constraints. Do not include credentials, customer data, proprietary strategies, account details,
or licensed market data. Use synthetic inputs or describe the behavior when a safe reproduction
cannot be shared.

The maintainer aims to acknowledge a report within three business days and provide an initial
assessment within seven business days. Remediation timing depends on severity, exploitability, and
release risk. These targets are goals, not guarantees. Keep the report private while it is being
assessed and fixed. The maintainer and reporter will coordinate public disclosure after a fix or
mitigation is available. The project does not currently offer a bug bounty.

## Security boundaries

An external strategy is an arbitrary executable, not a sandboxed plugin. The engine starts it
directly without a shell, but the child inherits the engine process's operating-system identity,
environment, filesystem access, network access, and standard error. Run only trusted strategies or
isolate them with an operating-system account, container, or sandbox that supplies the minimum
environment and permissions. Do not put secrets in strategy arguments, scenarios, or engine logs.

Committed fixtures must contain only synthetic or redistributable data. Never add provider
credentials, customer account data, proprietary strategies, or licensed market data. Sanitize any
reproduction before sharing it in an issue, pull request, test, journal, or transcript.

Journals and strategy transcripts can contain market events, orders, positions, diagnostics, and
a bounded prefix of a rejected strategy response. Store them according to the sensitivity of their
inputs and review them before sharing. Artifact hashes and release attestations provide integrity
and provenance; they do not encrypt data, enforce access control, or prove that an artifact is safe
to execute.

Repository dependency and analysis controls are described in the
[security maintenance guide](../docs/security-maintenance.md).
