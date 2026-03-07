# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- `Jsonrpc.inbound` plus `inbound_of_yojson` / `inbound_of_string` for adapters that normalize requests and notifications through one server-facing shape.
- `Jsonrpc.make_*_json` helpers for emitting wire JSON directly without an extra `message_to_yojson` conversion step.

### Fixed
- `Jsonrpc` serializers now always include the required `"jsonrpc": "2.0"` field for requests, notifications, responses, and errors.

## [0.6.0] - 2026-03-07

### Added
- `Client` module in `mcp_protocol_eio`: typed API for connecting to MCP servers
  (`initialize`, `ping`, `list_tools`, `call_tool`, `list_resources`, `read_resource`,
  `list_prompts`, `get_prompt`, `close`).
- `Server.context` type: handler context with `send_notification`, `send_log`, `send_progress`
  for server-to-client notifications during request handling.
- `logging/setLevel` auto-handler in Server (updates internal log level, filters `send_log`).
- `completion/complete` handler registration via `Server.add_completion_handler`.
- `Server.send_notification`: send arbitrary notifications to the connected client.
- `echo_client` example: subprocess-based MCP client connecting to `echo_server`.
- 15 new client unit tests (mock I/O based).
- 5 new integration tests (Server ↔ Client via Eio pipes).

### Changed
- **BREAKING**: Handler signatures now receive `context` as first parameter
  (`tool_handler`, `resource_handler`, `prompt_handler` all take `context ->` prefix).

## [0.5.0] - 2026-03-07

### Added
- `Server` module in `mcp_protocol_eio`: handler registration, automatic lifecycle management
  (initialize/initialized handshake, ping/pong, version negotiation), and method dispatch.
- Tool, Resource, and Prompt handler types with `add_tool`, `add_resource`, `add_prompt` builder functions.
- `Server.run` main loop with automatic MCP protocol handling.
- 19 new tests for server framework (initialize, ping, tools, resources, prompts, dispatch).
- `echo_server` example rewritten to use `Server` framework (125 lines to 48 lines).

### Changed
- `echo_server` example now uses `Server.create |> add_tool |> run` instead of manual dispatch.

## [0.4.0] - 2026-03-07

### Added
- `mcp_protocol_eio` sub-library: Eio-based stdio transport (`Stdio_transport`).
- `echo_server` example: minimal MCP server handling initialize, ping, tools/list, tools/call.
- 14 new tests for stdio transport (read/write/roundtrip/close).

### Removed
- **BREAKING**: `Protocol` module removed (deprecated in v0.3.0). Use `Mcp_types`, `Jsonrpc`, `Error_codes` directly.
- **BREAKING**: `Resources` module removed (deprecated in v0.3.0). Use `Mcp_types` directly.

## [0.3.0] - 2026-03-07

### Added
- `.mli` interface files for all 11 non-legacy modules (public API contract).
- `CHANGELOG.md` (this file).
- `Transport` module type signature for transport layer abstraction.
- Immutable `session_info` with functional update helpers (`session_ready`, `session_close` now return new values).

### Changed
- `session_ready` and `session_close` return `session_info` instead of `unit`.
- `Sampling.role` is now an alias for `Mcp_types.role` (type deduplication).
- `Mcp_result.request_id` is now an alias for `Jsonrpc.request_id` (type deduplication).
- `Session.request_id` is now an alias for `Jsonrpc.request_id` (type deduplication).
- `check_timeouts` uses `List.fold_left` instead of `ref` (immutable).
- `parse_media_type` uses fold instead of `ref` (immutable).
- README: fixed error code values, updated tool example with `title`/`annotations`, bumped dependency version.

### Deprecated
- `Protocol` module: use `Mcp_types`, `Jsonrpc`, `Error_codes` directly.
- `Resources` module: use `Mcp_types` directly.

## [0.2.2] - 2026-02-20

### Added
- MCP 2025-11-25 spec compliance: `tool_annotations`, `title` field on tools.
- `AudioContent`, `ResourceLinkContent` content types.
- `content_annotations` (audience, priority) on all content types.
- `structured_content` field on `tool_result`.
- `elicitation_schema`, `elicitation_params`, `elicitation_result` types.
- `completion_context` type.
- `Notifications.elicitation_create` method constant.
- Convenience constructors: `make_tool`, `make_resource`, `make_prompt`, `make_root`, `make_completion_argument`, `make_completion_result`.
- Root and completion types (`root`, `roots_capability`, `completion_reference`, `completion_argument`, `completion_result`).

### Changed
- `Request_tracker` refactored to immutable `Map` (was mutable `Hashtbl`).
- `cancel` returns updated request with state (was `unit`).
- Annotation parse errors now propagate instead of being silently ignored.

### Fixed
- `resource_template` type: `uri_template` field key corrected to `uriTemplate`.

## [0.2.0] - 2026-02-18

### Added
- `Logging` module: log levels (RFC 5424), `logging_message`, `set_level_params`.
- `Sampling` module: `sampling_message`, `model_preferences`, `create_message_params/result`.
- `Notifications` module: all MCP method string constants, `is_notification` predicate.
- `Error_codes`: `tool_execution_error`, `prompt_not_found`, `url_elicitation_required`, `stateless_mode_not_supported`.
- `Http_negotiation`: `Sse_only` transport mode, `format_sse_json` helper.
- `Version`: `features_of_version` with `has_sampling`, `has_elicitation`, `has_streamable_http`.

## [0.1.1] - 2026-02-15

### Added
- 107 tests covering all modules.
- `Mcp_result` module: phantom types, progress tokens, cancellation.
- `Session` module: request tracking, session lifecycle, connection errors.
- `Protocol` backward compatibility module.
- `Resources` backward compatibility module.
- Convenience re-exports in `Mcp_protocol` entry point.

## [0.1.0] - 2026-02-14

### Added
- Initial release.
- `Jsonrpc` module: JSON-RPC 2.0 message types and constructors.
- `Mcp_types` module: Tool, Resource, Prompt, Initialize types.
- `Error_codes` module: JSON-RPC and MCP error codes.
- `Http_negotiation` module: Accept header parsing, transport negotiation.
- `Version` module: protocol version handling and negotiation.

[0.6.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.2.2...v0.3.0
[0.2.2]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.2.0...v0.2.2
[0.2.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/jeong-sik/mcp-protocol-sdk/releases/tag/v0.1.0
