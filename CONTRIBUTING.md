# Contributing

## Local checks

Run these before opening or updating a PR:

```bash
opam exec -- dune build
opam exec -- dune runtest
bash scripts/check-release-metadata.sh
```

If you touch transport behavior or protocol compatibility, also run:

```bash
bash scripts/run-conformance.sh
```

## Issue labels

This repository follows the MCP SDK tiering label set.

- Type: `bug`, `enhancement`, `question`
- Status: `needs confirmation`, `needs repro`, `ready for work`, `good first issue`, `help wanted`
- Priority: `P0`, `P1`, `P2`, `P3`

New issues should always have exactly one type label. Status and priority labels are added during triage.

## Pull requests

- Keep changes scoped to one behavior or one release-quality concern.
- Update docs when public behavior, transport behavior, or release workflow changes.
- Leave changelog and version metadata consistent before merge.
- Prefer draft PRs until local checks are green.

## Release checklist

- `dune-project`, generated opam files, and the top changelog entry agree on one version.
- The conformance example server advertises the same version as the release metadata.
- CI is green.
- Any release-line policy changes are reflected in `docs/RELEASE-POLICY.md`.
