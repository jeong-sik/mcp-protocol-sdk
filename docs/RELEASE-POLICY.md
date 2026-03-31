# Release Policy

## Stable line

- The stable line is `1.x`.
- Non-breaking additions and fixes go into the next patch or minor release.
- Breaking API changes require a minor or major version bump with explicit migration notes.

## Required metadata

Every release candidate must keep these files in sync:

- `dune-project`
- `mcp_protocol.opam`
- top release entry in `CHANGELOG.md`
- `examples/conformance_server.ml`

`bash scripts/check-release-metadata.sh` is the canonical local and CI check for this.

## Required checks

Before cutting a release:

```bash
opam exec -- dune build
opam exec -- dune runtest
bash scripts/check-release-metadata.sh
bash scripts/run-conformance.sh
```

If the official conformance harness is unavailable in the current environment, note that explicitly in the release PR and rerun it in CI or a connected environment before tagging.

## Changelog discipline

- Keep `## [Unreleased]` empty or minimal.
- The next released version must be the first versioned entry in `CHANGELOG.md`.
- Release notes should call out protocol-facing changes, transport changes, and compatibility changes first.
