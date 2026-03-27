#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${script_dir}/.."

project_version="$(
  sed -nE 's/^\(version ([^)]+)\)$/\1/p' dune-project | head -1
)"

if [ -z "${project_version}" ]; then
  echo "Failed to parse version from dune-project" >&2
  exit 1
fi

check_opam_version() {
  local path="$1"
  local version
  version="$(sed -nE 's/^version: "([^"]+)"$/\1/p' "${path}" | head -1)"
  if [ "${version}" != "${project_version}" ]; then
    echo "${path}: expected version ${project_version}, got ${version}" >&2
    exit 1
  fi
}

check_opam_version "mcp_protocol.opam"
check_opam_version "mcp_protocol_eio.opam"
check_opam_version "mcp_protocol_http.opam"

changelog_version="$(
  sed -nE 's/^## \[([0-9]+\.[0-9]+\.[0-9]+)\].*/\1/p' CHANGELOG.md | head -1
)"

if [ "${changelog_version}" != "${project_version}" ]; then
  echo "CHANGELOG.md: expected top release ${project_version}, got ${changelog_version}" >&2
  exit 1
fi

example_version="$(
  sed -nE 's/.*~version:"([^"]+)".*/\1/p' examples/conformance_server.ml | head -1
)"

if [ "${example_version}" != "${project_version}" ]; then
  echo "examples/conformance_server.ml: expected version ${project_version}, got ${example_version}" >&2
  exit 1
fi

echo "Release metadata is consistent for version ${project_version}."
