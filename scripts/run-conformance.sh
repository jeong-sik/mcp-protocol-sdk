#!/usr/bin/env bash
# Run MCP conformance tests against the OCaml SDK's HTTP server.
#
# Prerequisites:
#   - Node.js (for npx)
#   - opam exec -- dune build (OCaml project must compile)
#
# Usage:
#   ./scripts/run-conformance.sh              # Run all scenarios
#   ./scripts/run-conformance.sh initialize    # Run specific scenario

set -euo pipefail

PORT="${MCP_CONFORMANCE_PORT:-9100}"
URL="http://localhost:${PORT}/mcp"
SCENARIO="${1:-}"

cd "$(git rev-parse --show-toplevel)"

echo "Building conformance server..."
opam exec -- dune build examples/conformance_server.exe

echo "Starting conformance server on port ${PORT}..."
opam exec -- dune exec examples/conformance_server.exe &
SERVER_PID=$!

cleanup() {
  echo "Stopping server (pid ${SERVER_PID})..."
  kill "${SERVER_PID}" 2>/dev/null || true
  wait "${SERVER_PID}" 2>/dev/null || true
}
trap cleanup EXIT

# Wait for server to be ready
for i in $(seq 1 30); do
  if curl -sf "${URL}" -X POST \
    -H 'Content-Type: application/json' \
    -d '{"jsonrpc":"2.0","id":0,"method":"ping"}' \
    >/dev/null 2>&1; then
    echo "Server ready."
    break
  fi
  if [ "$i" -eq 30 ]; then
    echo "Server failed to start after 30s."
    exit 1
  fi
  sleep 1
done

echo "Running conformance tests..."
echo "URL: ${URL}"
echo ""

if [ -n "${SCENARIO}" ]; then
  npx @modelcontextprotocol/conformance server \
    --url "${URL}" \
    --scenario "${SCENARIO}"
else
  npx @modelcontextprotocol/conformance server \
    --url "${URL}"
fi

echo ""
echo "Conformance tests completed."
