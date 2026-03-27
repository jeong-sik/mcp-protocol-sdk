# Conformance

This directory documents how to run the official MCP conformance harness against the OCaml SDK.

## Local flow

```bash
bash scripts/run-conformance.sh
```

Optional environment variables:

- `MCP_CONFORMANCE_PORT`: TCP port for the bundled server, default `9100`
- `MCP_CONFORMANCE_HOST`: bind host used in the probe URL, default `127.0.0.1`
- `MCP_CONFORMANCE_PACKAGE`: override the `npx` package reference, default `@modelcontextprotocol/conformance`

Run a single scenario:

```bash
bash scripts/run-conformance.sh initialize
```

## Bundled server

The conformance target is [examples/conformance_server.ml](/Users/dancer/me/workspace/yousleepwhen/mcp-protocol-sdk/.worktrees/refactor/sdk-canonical-mcp/examples/conformance_server.ml). It exposes:

- tools
- resources
- resource templates
- prompts
- completion

The example should track the current SDK release line so conformance output matches the release metadata.
