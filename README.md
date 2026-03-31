# MCP Protocol SDK for OCaml

![CI](https://github.com/jeong-sik/mcp-protocol-sdk/actions/workflows/ci.yml/badge.svg)

A pure OCaml 5.x implementation of the [Model Context Protocol](https://modelcontextprotocol.io/) (MCP), covering specification versions 2024-11-05 through 2025-11-25.

## Package

Single opam package `mcp_protocol` with three sub-libraries:

| Library | Public Name | Key Modules |
|---|---|---|
| Core | `mcp_protocol` | `Mcp_types`, `Jsonrpc`, `Sampling`, `Logging`, `Auth`, `Pagination` |
| Eio | `mcp_protocol.eio` | `Server`, `Generic_server`, `Generic_client`, `Stdio_transport`, `Memory_transport`, `Middleware` |
| HTTP | `mcp_protocol.http` | `Http_server`, `Http_client`, `Oauth_client`, `Auth_middleware` |

## Project Status

- Current version: `1.3.0`
- Roadmap: [ROADMAP.md](ROADMAP.md)
- Release policy: [docs/RELEASE-POLICY.md](docs/RELEASE-POLICY.md)
- Contribution guide and issue labels: [CONTRIBUTING.md](CONTRIBUTING.md)
- Conformance guide: [conformance/README.md](conformance/README.md)

## Feature Matrix

### Protocol Versions

| Version | Added Capabilities |
|---|---|
| 2024-11-05 | Tools, Resources, Prompts (initial stable) |
| 2025-03-26 | Streamable HTTP, OAuth 2.1, tool annotations, audio content |
| 2025-06-18 | Structured output, elicitation, resource links, title fields |
| 2025-11-25 | Sampling, tasks (experimental), icons, extensions |

Version negotiation selects the highest mutually-supported version during the initialize handshake. Use `Version.negotiate`, `Version.is_supported`, and `Version.features_of_version` to query capabilities at runtime.

### Transports

| Transport | Library | Description |
|---|---|---|
| Stdio (NDJSON) | `mcp_protocol.eio` | Newline-delimited JSON over stdin/stdout |
| Streamable HTTP | `mcp_protocol.http` | HTTP + SSE via `cohttp-eio`, with session management |
| In-memory | `mcp_protocol.eio` | Paired channels for zero-IO testing (`Memory_transport`) |

### Server Primitives

| Primitive | Fields |
|---|---|
| Tools | `name`, `description`, `title`, `input_schema`, `output_schema`, `annotations` (read_only, destructive, idempotent, open_world hints), `icon`, `execution` (task support) |
| Resources | `uri`, `name`, `title`, `description`, `mime_type`, `icon`. Templates via `resource_template`. Subscriptions via `subscribe`/`unsubscribe`. |
| Prompts | `name`, `title`, `description`, `arguments`, `icon` |

### Client Features

| Feature | Module | Description |
|---|---|---|
| Sampling | `Sampling` | `createMessage` with `model_preferences`, `sampling_tool` list, `sampling_tool_choice` (Auto/None\_/Tool), `include_context` (None\_/ThisServer/AllServers) |
| Elicitation | `Mcp_types_elicitation` | Form mode (schema-driven) and URL mode. Actions: Accept, Decline, Cancel |
| Tasks | `Mcp_types_tasks` | Experimental (2025-11-25). Status types: Working, Input_required, Completed, Failed, Cancelled. Classified into `terminal_status` and `active_status` at the type level |
| Logging | `Logging` | RFC 5424 levels: Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency. `setLevel` request and `notifications/message` |
| Roots | `Mcp_types` | Client file system roots with `list_changed` notification support |
| Completion | `Mcp_types_completion` | Prompt and resource reference completion with `completion_context` for multi-argument awareness |
| Progress | `Mcp_result` | Progress notifications with `progress_token`, `progress`, `total`, and `message` fields |
| Cancellation | `Mcp_result` | Cancel in-flight requests by `request_id` with optional `reason` |

### Async Tasks

Async tasks are substrate-owned MCP primitives. The SDK provides the wire types (`Mcp_types_tasks`) and an in-memory lifecycle store (`Task_store`) for servers.

**What the substrate guarantees:**
- Task state machine: Working -> Completed | Failed | Cancelled | Input_required. Invalid transitions return errors.
- Wire handlers for `tasks/get`, `tasks/list`, `tasks/cancel` via `add_task_handlers`.
- GC of terminal tasks via `Task_store.gc_terminal`.
- Thread-safe store using `Stdlib.Mutex` (works in both Eio and non-Eio contexts).

**What downstream runtimes adapt:**
- When to create tasks (e.g., long-running tool calls).
- How to poll or push task progress to clients.
- Persistence beyond process lifetime (the store is in-memory by default).

**Server recipe:**

```ocaml
let store = Task_store.create () in
let server =
  Server.create ~name:"my-server" ~version:"1.0.0" ()
  |> Server.add_task_handlers (Task_store.to_task_handlers store)
in
(* In a tool handler: create a task, do work, update status *)
let task = Task_store.create_task store () in
(* ... run work in a fiber ... *)
Task_store.update_status store task.task_id Completed
  ~updated_at:(string_of_float (Unix.gettimeofday ())) |> ignore
```

### Content Types

| Type | Variant | Fields |
|---|---|---|
| Text | `TextContent` | `text`, `annotations` |
| Image | `ImageContent` | `data` (base64), `mime_type`, `annotations` |
| Audio | `AudioContent` | `data` (base64), `mime_type`, `annotations` |
| Embedded Resource | `ResourceContent` | `resource` (uri + text/blob) |
| Resource Link | `ResourceLinkContent` | `uri`, `name`, `description`, `mime_type`, `annotations` |

### Structured Output

Tools can declare an `output_schema` (JSON Schema). When present, `tool_result.structured_content` carries the typed JSON output alongside the human-readable `content` list.

### OAuth 2.1 (`mcp_protocol.http`)

| Capability | RFC | Module |
|---|---|---|
| PKCE (S256) | RFC 7636 | `Oauth_client.generate_pkce` |
| Authorization code exchange | RFC 6749 | `Oauth_client.exchange_code` |
| Token refresh | RFC 6749 | `Oauth_client.refresh_token` |
| Discovery | RFC 8414 | `Oauth_client.discover` |
| Dynamic client registration | RFC 7591 | `Oauth_client.register_client` |
| Bearer token injection | RFC 6750 | `Oauth_client.inject_bearer_token` |
| Server-side bearer validation | RFC 6750 S3 | `Auth_middleware.check_auth` |
| Protected resource metadata | RFC 9728 | `Auth_middleware.resource_metadata` |
| CSRF state parameter | | `Oauth_client.generate_state`, `validate_state` |
| Credential store | | `Oauth_client.credential_store` (pluggable) |
| Incremental consent | | `build_authorization_url ~scopes` |

HTTPS is enabled automatically via `tls-eio` + system CA certificates.

### Extension Data

All initialize params/results, tool results, prompt results, and sampling messages carry an optional `_meta` field (`Yojson.Safe.t option`) for passing extension data through the protocol without schema changes.

### Extensions and Experimental

Both `server_capabilities` and `client_capabilities` include `extensions` and `experimental` fields (`Yojson.Safe.t option`) for capability negotiation of non-standard features.

## Architecture

### Functor-based Transport Abstraction

`Generic_server.Make(T)` and `Generic_client.Make(T)` produce a server or client for any module satisfying the `Transport.S` signature. This means the same application logic runs over stdio, HTTP, or in-memory transports with no code changes.

```ocaml
(* Stdio *)
module Stdio_server = Mcp_protocol_eio.Generic_server.Make(Stdio_transport)

(* In-memory for tests *)
module Test_server = Mcp_protocol_eio.Generic_server.Make(Memory_transport)

(* With logging middleware *)
module Logged = Mcp_protocol_eio.Middleware.Logging(Stdio_transport)
module Debug_server = Mcp_protocol_eio.Generic_server.Make(Logged)
```

### Middleware

`Middleware.Logging(T)` wraps any transport with stderr message logging. Additional middleware can be composed by chaining functors.

### Tool_arg -- Type-safe Argument Extraction

Extract tool arguments without manual JSON pattern matching:

```ocaml
let handler _ctx _name args =
  let open Tool_arg in
  let* text = required args "text" string in
  let count = optional args "count" int ~default:1 in
  Ok (Mcp_types.tool_result_of_text (String.concat "" (List.init count (fun _ -> text))))
```

Available extractors: `string`, `int`, `float`, `bool`, `json`, `list_of`.
Field access: `required` (returns `Error` on missing/parse failure), `optional` (returns default), `optional_opt` (returns `'a option`).

## Quickstart

```bash
opam pin add mcp_protocol git+https://github.com/jeong-sik/mcp-protocol-sdk.git
```

Or add to your `dune-project`:

```lisp
(depends
 (mcp_protocol (>= 1.3.0)))
```

Then use sub-libraries in your `dune` file:

```lisp
(libraries mcp_protocol mcp_protocol.eio mcp_protocol.http)
```

## Usage

### Ergonomic Server API

Register tools, resources, and prompts in one call:

```ocaml
let server =
  Server.create ~name:"my-server" ~version:"1.0" ()
  |> Server.tool "echo" ~description:"Echo input"
       (fun _ctx _name args ->
         let open Tool_arg in
         let* text = required args "text" string in
         Ok (Mcp_types.tool_result_of_text text))
  |> Server.resource ~uri:"mcp://info" "info" ~mime_type:"application/json"
       (fun _ctx _uri ->
         Ok [Mcp_types.{ uri = "mcp://info"; mime_type = Some "application/json";
                         text = Some {|{"status":"ok"}|}; blob = None }])
  |> Server.prompt "greet" ~description:"Greeting"
       (fun _ctx _name args ->
         let who = match List.assoc_opt "name" args with Some n -> n | None -> "world" in
         Ok Mcp_types.{ description = None;
           messages = [{ role = User;
             content = PromptText { type_ = "text"; text = "Hello, " ^ who } }];
           _meta = None })
```

### In-memory Testing

```ocaml
module Mt = Mcp_protocol_eio.Memory_transport
module Test_server = Mcp_protocol_eio.Generic_server.Make(Mt)
module Test_client = Mcp_protocol_eio.Generic_client.Make(Mt)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client_t, server_t = Mt.create_pair () in
  let server = Test_server.create ~name:"test" ~version:"1.0" ()
    |> Test_server.tool "ping" (fun _ _ _ -> Ok (Mcp_types.tool_result_of_text "pong"))
  in
  Eio.Fiber.fork ~sw (fun () ->
    Test_server.run server ~transport:server_t ~clock:(Eio.Stdenv.clock env) ());
  let client = Test_client.create ~transport:client_t ~clock:(Eio.Stdenv.clock env) () in
  match Test_client.initialize client ~client_name:"test" ~client_version:"1.0" with
  | Ok _ -> ignore (Test_client.call_tool client ~name:"ping" ())
  | Error e -> Printf.eprintf "Failed: %s\n" e
```

### HTTP Server (Streamable HTTP)

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server =
    Http_server.create ~name:"my-server" ~version:"1.0.0" ()
    |> Http_server.add_tool
         (Mcp_types.make_tool ~name:"echo" ~description:"Echo" ())
         (fun _ctx _name args ->
           let open Tool_arg in
           let* text = required args "text" string in
           Ok (Mcp_types.tool_result_of_text ("Echo: " ^ text)))
  in
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let socket = Eio.Net.listen ~sw net addr ~backlog:128 in
  Cohttp_eio.Server.run socket
    (Cohttp_eio.Server.make
       ~callback:(Http_server.callback server) ())
    ~on_error:(fun exn -> Printf.eprintf "%s\n" (Printexc.to_string exn))
```

### HTTP Client

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let client = Http_client.create ~endpoint:"http://127.0.0.1:8080/mcp" ~net ~sw () in
  match Http_client.initialize client ~client_name:"my-client" ~client_version:"1.0" with
  | Ok result ->
    Printf.printf "Connected to %s\n" result.server_info.name;
    ignore (Http_client.close client)
  | Error e -> Printf.eprintf "Failed: %s\n" e
```

### OAuth Discovery + HTTPS

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  match Oauth_client.discover ~net ~sw ~issuer:"https://auth.example.com" with
  | Error e -> Printf.eprintf "Discovery failed: %s\n" e
  | Ok metadata ->
    let verifier, challenge = Oauth_client.generate_pkce () in
    let state = Oauth_client.generate_state () in
    let auth_url = Oauth_client.build_authorization_url
      ~authorization_endpoint:metadata.authorization_endpoint
      ~client_id:"my-client" ~redirect_uri:"http://localhost:9999/callback"
      ~scopes:["read"] ~state ~code_challenge:challenge () in
    Printf.printf "Visit: %s\n" auth_url;
    ignore (Oauth_client.exchange_code ~net ~sw
      ~token_endpoint:metadata.token_endpoint
      ~client_id:"my-client" ~code:"AUTH_CODE"
      ~redirect_uri:"http://localhost:9999/callback"
      ~code_verifier:verifier)
```

## Error Codes

```ocaml
(* JSON-RPC standard *)
Error_codes.parse_error        (* -32700 *)
Error_codes.invalid_request    (* -32600 *)
Error_codes.method_not_found   (* -32601 *)
Error_codes.invalid_params     (* -32602 *)
Error_codes.internal_error     (* -32603 *)

(* MCP-specific *)
Error_codes.connection_closed      (* -32001 *)
Error_codes.request_timeout        (* -32002 *)
Error_codes.resource_not_found     (* -32003 *)
Error_codes.tool_execution_error   (* -32004 *)
```

## Testing

33 test suites (565 tests) covering types, serialization, transports, server/client lifecycle, OAuth, SSE, and integration scenarios. `Memory_transport` enables deterministic testing with zero IO.

```bash
dune runtest
bash scripts/check-release-metadata.sh
```

Official conformance can be exercised against the bundled example server:

```bash
bash scripts/run-conformance.sh
```

## Building from Source

```bash
git clone https://github.com/jeong-sik/mcp-protocol-sdk.git
cd mcp-protocol-sdk
opam install . --deps-only
dune build
```

## Docs

- [Install Checklist](docs/INSTALL-CHECKLIST.md) -- post-install verification
- [MCP Config Template](docs/MCP-TEMPLATE.md) -- `~/.mcp.json` template for servers
- [Conformance Guide](conformance/README.md) -- official conformance harness flow
- [Contributing](CONTRIBUTING.md) -- issue labels, local checks, release checklist
- [Release Policy](docs/RELEASE-POLICY.md) -- versioning and release metadata rules
- [Roadmap](ROADMAP.md) -- Tier 1 quality work and follow-up milestones
- [Setup Guide](docs/SETUP.md) -- development environment setup

## License

MIT License

## References

- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
- [TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk)
- [Python SDK](https://github.com/modelcontextprotocol/python-sdk)
