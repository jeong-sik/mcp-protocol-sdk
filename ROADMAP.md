# SDK Roadmap

This roadmap focuses on getting the OCaml MCP SDK to Tier 1 operating quality.

## Now

- Keep the stable line on `1.0.x` with changelog, opam, and `dune-project` in sync.
- Keep Streamable HTTP, stdio, OAuth, logging, completion, roots, elicitation, and conformance example flows documented and locally reproducible.
- Use `scripts/check-release-metadata.sh` as a required CI gate.

## Next

- Run the official MCP conformance harness regularly against the bundled conformance server and publish the score.
- Promote conformance from a local/manual workflow to a required release check after the scenario set is stable.
- Expand docs into focused guides for transport compatibility, auth, and server-to-client requests.

## Later

- Add a higher-level ergonomic server builder layer without replacing the low-level core packages.
- Add stronger release automation around tags, changelog generation, and artifact publication.
- Add more example servers for stateless HTTP, stateful HTTP, and OAuth-protected deployments.
