# Documentation platform

The canonical documentation site is published at `https://fallblu.github.io/trading-engine/` from
one immutable GitHub Pages artifact. It combines project guides, versioned contract sources, and
generated OCaml API pages without committing generated HTML.

## Toolchain

[MkDocs](https://www.mkdocs.org/user-guide/configuration/) 1.6 and Material for MkDocs 9 render the
Markdown navigation and search index. [odoc](https://ocaml.github.io/odoc/odoc/odoc_for_authors.html)
renders the public `.mli` interfaces through Dune's `@doc` target. Exact Python package versions
live in `requirements/docs.lock`; the exact odoc version lives in `trading_engine.opam.locked`.

The build stages the repository Markdown and entire `contracts/` tree under `_build`, adds the odoc
HTML tree, and then runs `mkdocs build --strict`. Staging publishes contract README files, schemas,
and fixtures directly from their source locations, so the v1 pages cannot diverge from the
repository copies.

`make docs-check` performs the deterministic offline source check. `make docs-build` bootstraps the
locked documentation tools, builds odoc, runs strict MkDocs, checks every generated local link, and
confirms that public modules and contract assets are present.

## Deployment boundary

The [GitHub Pages custom workflow](https://docs.github.com/en/pages/getting-started-with-github-pages/using-custom-workflows-with-github-pages)
uses a read-only build job. Pull requests build and package the complete site but cannot deploy it.
Only a push to `develop` enables the separate deployment job, whose only elevated permissions are
`pages: write` and `id-token: write`. The `github-pages` environment records the deployed URL.

All actions use full commit pins. Pages deployment never writes a generated branch, repository
commit, tag, or release artifact.

## Link validation

Offline checks validate every repository-relative Markdown target and every generated HTML link.
External HTTPS links run through a separate pinned Lychee workflow on relevant pull requests,
`develop` changes, a weekly schedule, and manual dispatch. It uses no token, rejects insecure or
private targets, bounds redirects and retries, and begins with an empty exception list.
