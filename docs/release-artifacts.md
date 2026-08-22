# Release artifacts and provenance

This repository can build and verify a release candidate, but it does not automatically choose a
version, create or push a tag, publish an opam package, or create a GitHub release. Each of those
actions requires separate human approval.

## Artifact set

An approved version produces these distributable subjects:

| Artifact | Intended contents |
| --- | --- |
| `trading-engine-VERSION-linux-x86_64.tar.gz` | Installed CLI, OCaml library, package metadata, versioned contracts, fixtures, and license documents |
| `trading-engine-VERSION-source.tar.gz` | Every Git-tracked source file at the exact revision |
| `trading-engine-VERSION-contracts.tar.gz` | Conformance data plus every versioned schema, fixture, and contract README |
| `trading-engine-VERSION-documentation.tar.gz` | Offline strict site with project guides, contract pages and assets, and generated OCaml API pages |
| `trading-engine-VERSION.opam` | Exact checked-in opam package definition |

The candidate also contains `release-manifest.json`, `SUBJECTS.sha256`, `SHA256SUMS`, an SPDX 2.3
SBOM, and a deterministic in-toto statement with a SLSA v1 provenance predicate. The SBOM covers
the project and every locked OCaml and Python dependency used to build the artifact set. The
provenance binds subject hashes to the Git revision, lockfile hashes, target, version, and source
date epoch.

The Linux archive is the supported prebuilt target. Other systems install through the source and
opam artifacts until an equally strict native target is added and independently reproduced.

## Deterministic build

Run `make release-check` from a clean tracked revision. The check derives `SOURCE_DATE_EPOCH` from
that commit, performs two clean builds, normalizes archive ownership and timestamps, suppresses
gzip timestamps, and compares the complete candidate directories byte for byte. It then validates
archive topology and metadata, every checksum, SPDX structure, provenance subjects, exact opam
bytes and lint result, and the installed CLI's reported version.

Generated files are written to ignored `release/`. A candidate is disposable evidence; it is not a
release. The `Release candidate` workflow repeats the check on a fresh Ubuntu runner for relevant
pull requests and `develop` changes and retains the candidate for 14 days.

## Approval, signing, and publication

The following is a human-controlled release procedure, not an automated promise:

1. Approve a version change separately, update public version references, and pass all repository
   and cross-repository checks.
2. Review the exact release commit, create an approved signed `vVERSION` tag, and push that tag.
3. Configure required reviewers on the `release` GitHub environment. Manually dispatch the
   `Release candidate` workflow from the exact tag and enter the matching version. The workflow
   rejects branches and mismatched versions.
4. Approve the environment deployment. GitHub OIDC then obtains short-lived Sigstore certificates
   and records signed build-provenance and SPDX SBOM attestations for the subjects. No long-lived
   signing key is stored in the repository.
5. Download the candidate and attestation bundles. Verify `sha256sum -c SHA256SUMS`, then verify
   each distributable with `gh attestation verify ARTIFACT --repo fallblu/trading-engine`.
6. If an additional offline signature is required, a human signer reviews the hashes and runs
   `cosign sign-blob --yes --bundle SHA256SUMS.sigstore.json SHA256SUMS`; a second person verifies
   the bundle before publication.
7. Create a draft GitHub release with the existing signed tag and the complete candidate set.
   Review downloaded assets and attestations again, then explicitly publish the draft. Never
   regenerate or replace artifacts under an existing version.
8. Submit the exact opam file and source checksum to `opam-repository` in a separate reviewed pull
   request. Documentation remains versioned inside the release artifact even though the latest
   project documentation also lives on GitHub Pages.

The manual workflow uploads only short-lived Actions artifacts and attestations. It does not create
a tag, GitHub release, release commit, package publication, or version bump.
