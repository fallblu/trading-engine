# Repository governance

GitHub settings enforce the integration workflow for the release branch. The reviewed source of
truth lives in `.github/branch-protection.json` and `.github/repository.json`.

## Main branch protection

Every change to `main` requires a pull request whose head is current with `main`. The pull request
must resolve all review conversations and pass both checks produced by the mainline workflow:

- `check`, which runs the complete locked repository gate;
- `persistra-compatibility`, which validates the paired cross-repository baseline.

The protection blocks force pushes and branch deletion, requires linear history, and applies to
administrators without a bypass. Trading Engine currently has one maintainer, so the rule requires
zero approving reviews. Requiring approval would make self-authored changes impossible to land.
Stale-approval dismissal, code-owner review, and last-push approval are disabled. Revisit that
choice when a second regular reviewer is available.

## Merge behavior

Rebase merging is the only supported GitHub merge mode. Merge commits and squash merging are
disabled so coherent commits remain individually visible on a linear history. GitHub deletes
merged head branches automatically. These repository-wide merge settings apply to feature pull
requests into `develop` and promotion pull requests into `main`.
