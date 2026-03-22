# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.14.0] - 2026-03-23

### Added
- **Generic_server functor**: `Generic_server.Make(T: Transport.S)` produces a transport-agnostic
  MCP server. Run the same server logic over stdio, in-memory, HTTP, or custom transports.
- **Generic_client functor**: `Generic_client.Make(T: Transport.S)` produces a transport-agnostic
  MCP client with the full typed API (initialize, tools, resources, prompts).
- **Logging middleware**: `Middleware.Logging(T)` wraps any transport with message logging.
  Demonstrates composable middleware pattern via functor chaining.
- **Transport re-exported**: `Mcp_protocol.Transport.S` is now accessible from the core library,
  enabling external functor applications.
- **E2E memory tests**: Full client-server lifecycle test (initialize, ping, tools/list,
  tools/call, prompts/get, resources/read) running entirely in-memory.
- **Tool_arg module**: Type-safe tool argument extraction with `required`, `optional`,
  `optional_opt` field accessors and `string`/`int`/`float`/`bool`/`list_of` extractors.
  Monadic `let*` binding for chaining required extractions.
- **Ergonomic registration**: `Server.tool`, `Server.resource`, `Server.prompt` convenience
  functions that combine definition creation + handler registration in one call.
- **Typed capabilities**: `server_capabilities` and `client_capabilities` now use typed records
  (`tools_capability`, `resources_capability`, `prompts_capability`) instead of `Yojson.Safe.t option`.
  `logging` and `completions` use `unit option` — `Some ()` = enabled.
- **Tool `outputSchema`**: Tools can declare an output JSON Schema (`output_schema` field).
- **Tool `execution`**: Tools can declare task support level via `execution.task_support`
  (`Task_required | Task_optional | Task_forbidden`).
- **Elicitation mode variant**: `elicitation_params.mode` is now `elicitation_mode option`
  (`Form | Url`) instead of `string option`. URL mode adds `url` field for redirect target.
- **Classified task status**: `terminal_status`, `active_status`, and `classified_status` types
  with `classify_status`, `of_terminal`, `of_active` functions. Enables compile-time separation
  of terminal vs active task states without breaking existing `task_status` type.
- **In-memory transport**: `Memory_transport.create_pair ()` returns paired transports for
  testing without IO. Messages pass as OCaml values (zero serialization overhead).

### Changed
- **BREAKING**: `server_capabilities.tools` is `tools_capability option` (was `Yojson.Safe.t option`).
- **BREAKING**: `server_capabilities.logging` is `unit option` (was `Yojson.Safe.t option`).
- **BREAKING**: `client_capabilities.sampling` is `unit option` (was `Yojson.Safe.t option`).
- **BREAKING**: `client_capabilities.elicitation` is `unit option` (was `Yojson.Safe.t option`).
- **BREAKING**: `tool` record has two new fields: `output_schema` and `execution`.
- `server_capabilities` gains `completions: unit option` field (was in `experimental`).
- `Handler.build_initialize_params` uses typed capabilities and `initialize_params_to_yojson`.

## [0.12.1] - 2026-03-16

### Fixed
- **C1**: JSON-RPC null ID vs absent ID — `List.assoc_opt` distinguishes `{"id":null}` (Request) from missing `id` (Notification).
- **C2**: OAuth `parse_token_response` rejects empty `access_token` strings.
- **C4**: CSPRNG `ensure_rng` consolidated to single `Tls_helpers.ensure_rng` (was duplicated in `oauth_client.ml`).
- **H4**: `content_annotations.priority` accepts JSON integer values (e.g. `"priority": 1`), not only floats.
- **H5**: HTTP POST callback parses JSON once (was parsing twice for auth check + dispatch).
- **H6**: `WWW-Authenticate` header escaping strips DEL (0x7F) character per RFC 7230.
- **M3**: `embedded_resource_of_yojson` requires at least one of `text` or `blob`.
- **M5**: `Http_session.event_counter` uses `Atomic.t` for fiber-safe monotonic IDs.

### Added
- `make_text_content`, `make_image_content`, `make_audio_content`, `make_resource_content`, `make_resource_link_content` type-safe constructors (M4 fix: enforce correct `type_` discriminator).
- `Tls_helpers.ensure_rng` exposed in `.mli`.
- `Auth_middleware.escape_quoted_string` exposed in `.mli`.
- 16 regression tests in `test/test_bug_fixes.ml`.

### Changed
- **H2**: Version negotiation logs warning when no compatible version found (was silent fallback to latest).
- **H3**: `Handler.add_tool` detects duplicate tool names and replaces the previous registration with a warning (was silent shadowing).
- **C3**: `in_memory_store` documents single-domain safety constraint.
- **M1**: `Http_session.validate` documents pre-init acceptance intent.

## [0.12.0] - 2026-03-15

### Added
- **OAuth Discovery** (RFC 8414): `Oauth_client.discover` fetches `.well-known/oauth-authorization-server` metadata.
- **Dynamic Client Registration** (RFC 7591): `Oauth_client.register_client` for runtime client registration.
- **HTTPS transport**: `Tls_helpers` module with system CA certificate support via `tls-eio` + `ca-certs`.
- **OAuth 2.1 auth middleware**: `Auth_middleware` module implementing RFC 6750 bearer token verification, scope checking, `WWW-Authenticate` responses.
- **Protected Resource Metadata** (RFC 9728): `GET /.well-known/oauth-protected-resource` endpoint.
- HTTPS enforcement for OAuth endpoints (loopback exception for development).
- `http_auth_server` example demonstrating OAuth-protected MCP server.

### Changed
- `Oauth_client.post_form` and `get_json` use `Tls_helpers.make_client` for HTTPS support.
- `Http_server.create` accepts optional `?auth` parameter for OAuth configuration.

## [0.11.0] - 2026-03-14

### Added
- **ppx_deriving_jsonschema** integration: `[@@deriving jsonschema]` for automatic JSON Schema generation from OCaml types.
- **OAuth 2.1 client** (`Oauth_client`): PKCE (RFC 7636), token exchange, refresh, pluggable credential store.
- `Auth` module: OAuth token types, error types, server metadata types.
- `base64url_encode` for URL-safe base64 encoding.
- `inject_bearer_token` helper for adding Authorization headers.
- `test_ppx_schema.ml` and `test_auth.ml` test suites.

## [0.10.0] - 2026-03-11

### Added
- **Tasks primitive** (MCP 2025-11-25 experimental, SEP-1686): 5-state lifecycle (working/input_required/completed/failed/cancelled), `tasks/get`, `tasks/result`, `tasks/list`, `tasks/cancel`, `notifications/tasks/status`. Server and client support with task handler registration.
- **Icons metadata** (SEP-973): `icon: string option` field on `tool`, `resource`, `resource_template`, `prompt` types. `make_tool`, `make_resource`, `make_prompt` constructors accept `?icon` parameter.
- **URL elicitation mode** (SEP-1036): `mode: string option` field on `elicitation_params` for URL input requests.
- **Sampling tool calling** (SEP-1577): `tools` and `tool_choice` fields on `create_message_params`. None values are omitted from JSON output.
- `version_features`: `has_tasks`, `has_icons` flags. `2025-11-25` version enables both.
- 12 new tests across 6 test suites: Tasks lifecycle, icons round-trip (tool/resource/resource_template/prompt), URL elicitation mode, Sampling tools/toolChoice.

### Changed
- **Breaking**: `tool`, `resource`, `resource_template`, `prompt` records have a new `icon` field. Code constructing these records directly must add `icon = None`.
- **Breaking**: `version_features` has new `has_tasks` and `has_icons` fields.
- `create_message_params` has new `tools` and `tool_choice` fields (default `None`).

## [0.9.1] - 2026-03-09

### Changed
- `Handler.subscribed_uris` now returns URIs in sorted order (was insertion order) due to `StringSet` migration.
- `parse_list_field` and `build_initialize_params` moved to shared `Handler` module (was duplicated in stdio and HTTP clients).
- `validate_session_or_error` extracted in HTTP server (was copy-pasted 3x across POST/GET/DELETE handlers).

### Fixed
- `Http_session.generate_session_id`: added `/dev/urandom` fallback (uses `Random` when urandom unavailable).
- `http/sse.ml`: replaced manual string slicing with `starts_with`/`strip_prefix` helpers.
- `http/http_server.ml`: renamed `_sse_content_type` to `sse_content_type` (was prefixed as unused but actually referenced).

### Improved
- `subscribed_uris`: O(n) `List.mem` replaced with O(log n) `StringSet.mem`.

## [0.9.0] - 2026-03-08

### Added
- **`mcp_protocol_http` package**: Streamable HTTP transport for MCP.
  - `Sse` module: SSE event encoding/parsing and `Broadcaster` (Eio.Stream-based).
  - `Http_session` module: session ID generation (/dev/urandom 128-bit hex), state machine, header validation.
  - `Http_server` module: POST/GET/DELETE/OPTIONS routing, `Mcp-Session-Id` management, CORS support.
    - Builder API: `create ~name ~version () |> add_tool ... |> add_resource ...`
    - `callback` function for integration with `Cohttp_eio.Server`.
  - `Http_client` module: typed API matching `Client` (initialize, ping, list/call tools, resources, prompts, close).
    - Per-request `Eio.Switch` pattern for correct cohttp-eio resource cleanup.
    - Automatic `Mcp-Session-Id` header management.
- Transport-agnostic `Handler` module extracted from `Server` (shared by stdio and HTTP servers).
- 7 HTTP integration tests (multi-tool, multi-resource, close-reuse, large payload, sequential, full e2e).
- `http_echo_server` and `http_echo_client` examples.
- HTTP client: callback registration (`on_sampling`, `on_roots_list`, `on_elicitation`), timeout with cancellation notification, capability advertisement during initialize.
- Pagination: `?cursor` parameter on all `list_tools`, `list_resources`, `list_prompts` APIs (stdio + HTTP clients).
- Resource subscription: `subscribe_resource` and `unsubscribe_resource` on both clients; server handler tracks subscribed URIs and advertises `subscribe` capability.
- `resources.mli` interface file for backward compat module.
- 79 new tests (327 total, up from 294 in v0.8.0).

### Fixed
- `protocol.ml`: narrowed 5 bare `with _` exception handlers to `Type_error _` / `Json_error _` (prevented swallowing `Out_of_memory`/`Stack_overflow`).
- `eio/client.ml`: added `Out_of_memory`/`Stack_overflow` guard to notification handler exception path.
- HTTP server: CORS headers now included on all responses (POST/GET/DELETE), not just OPTIONS preflight.
- HTTP server: request body size limit returns 413 instead of unhandled exception.

## [0.8.0] - 2026-03-08

### Added
- `Mcp_error` module: typed error hierarchy with `Protocol` (wire) and `Sdk` (local) variants.
- Request timeout: `Client.create` and `Server.run` support `?clock` parameter for 60s timeout (matches TypeScript SDK).
  - Sends `notifications/cancelled` on client request timeout.
- `notifications/cancelled` handling: Client aborts in-flight request; Server logs cancellation.
- Transport `max_size` is now configurable via `Stdio_transport.create ~max_size`.

### Fixed
- `jsonrpc.ml` `message_of_yojson`: narrowed bare `with _ ->` to specific exception types.
- `Stdio_transport.write`: catches `Eio.Io` exceptions, marks transport closed.
- `Stdio_transport.read`: catches `Eio.Io` exceptions (broken pipe), marks transport closed.

### Changed
- `Client.create` signature: now accepts optional `?clock` and trailing `()`.
- `Server.run` signature: now accepts optional `?clock` and trailing `()`.
- `Stdio_transport.create` signature: now accepts optional `?max_size` and trailing `()`.

## [0.7.0] - 2026-03-07

### Added
- **Bidirectional Server→Client requests**: Server handlers can now send requests
  to the connected Client and receive typed responses.
  - `context.request_sampling`: send `sampling/createMessage` to the Client.
  - `context.request_roots_list`: send `roots/list` to the Client.
  - `context.request_elicitation`: send `elicitation/create` to the Client.
- **Client callback registration**: builder functions for handling Server→Client requests.
  - `Client.on_sampling`: register a `sampling/createMessage` handler.
  - `Client.on_roots_list`: register a `roots/list` handler.
  - `Client.on_elicitation`: register an `elicitation/create` handler.
  - `Client.on_notification`: register a generic notification handler.
- Client `read_response` now dispatches incoming `Jsonrpc.Request` to registered
  handlers while waiting for its own response (interleaved dispatch).
- Client `initialize` advertises capabilities (`sampling`, `roots`, `elicitation`)
  based on which handlers are registered.
- 10 new client callback unit tests.
- 5 new bidirectional integration tests (Server↔Client via Eio pipes).
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

[0.12.1]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.12.0...v0.12.1
[0.12.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.11.0...v0.12.0
[0.11.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.10.0...v0.11.0
[0.10.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.9.1...v0.10.0
[0.9.1]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.9.0...v0.9.1
[0.9.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.8.0...v0.9.0
[0.8.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.2.2...v0.3.0
[0.2.2]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.2.0...v0.2.2
[0.2.0]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/jeong-sik/mcp-protocol-sdk/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/jeong-sik/mcp-protocol-sdk/releases/tag/v0.1.0
