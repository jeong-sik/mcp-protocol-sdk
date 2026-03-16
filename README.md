# MCP Protocol SDK for OCaml

A pure OCaml implementation of the [Model Context Protocol](https://modelcontextprotocol.io/) (MCP) types and utilities.

## Overview

MCP enables LLMs to interact with external tools, resources, and prompts through a standardized protocol. This SDK provides:

- **JSON-RPC 2.0** types and utilities for wire protocol
- **MCP Primitives** (Tools, Resources, Prompts)
- **Stdio Transport** (`mcp_protocol_eio`) — NDJSON over stdin/stdout
- **HTTP Transport** (`mcp_protocol_http`) — Streamable HTTP with SSE, sessions, cohttp-eio
- **Protocol Versioning** with negotiation support

Setup 가이드: `docs/SETUP.md`  
Install Checklist: `docs/INSTALL-CHECKLIST.md`

## Quickstart

```bash
opam pin add mcp_protocol git+https://github.com/jeong-sik/mcp-protocol-sdk.git
```
## Installation

```bash
opam pin add mcp_protocol git+https://github.com/jeong-sik/mcp-protocol-sdk.git
```

Or add to your `dune-project`:

```lisp
(depends
 (mcp_protocol (>= 0.12.0))
 (mcp_protocol_eio (>= 0.12.0))   ;; for stdio transport
 (mcp_protocol_http (>= 0.12.0))) ;; for HTTP transport
```

## Docs

- [Install Checklist](docs/INSTALL-CHECKLIST.md) - Post-install checks
- [MCP Config Template](docs/MCP-TEMPLATE.md) - `~/.mcp.json` template (for servers)

## Usage

### Basic JSON-RPC Messages

```ocaml
open Mcp_protocol

(* Create a JSON-RPC request *)
let req = Jsonrpc.make_request
  ~id:(Jsonrpc.Int 1)
  ~method_:"tools/list"
  ()

(* Create a notification (no id, no response expected) *)
let notif = Jsonrpc.make_notification
  ~method_:"notifications/initialized"
  ()

(* Create a success response *)
let resp = Jsonrpc.make_response
  ~id:(Jsonrpc.Int 1)
  ~result:(`Assoc [("tools", `List [])])

(* Create an error response *)
let err = Jsonrpc.make_error
  ~id:(Jsonrpc.Int 1)
  ~code:Error_codes.method_not_found
  ~message:"Method not found"
  ()
```

### MCP Types

```ocaml
open Mcp_protocol

(* Define a tool (v0.2.2+: title and annotations fields) *)
let my_tool : Mcp_types.tool = {
  name = "calculate";
  description = Some "Perform mathematical calculations";
  title = Some "Calculator";
  annotations = Some {
    title = None;
    read_only_hint = Some true;
    destructive_hint = Some false;
    idempotent_hint = Some true;
    open_world_hint = None;
  };
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("expression", `Assoc [
        ("type", `String "string");
        ("description", `String "Math expression to evaluate")
      ])
    ]);
    ("required", `List [`String "expression"])
  ];
}

(* Or use the convenience constructor *)
let my_tool2 = Mcp_types.make_tool
  ~name:"calculate"
  ~description:"Perform mathematical calculations"
  ~title:"Calculator"
  ()

(* Define a resource *)
let my_resource : Mcp_types.resource = {
  uri = "file:///workspace/README.md";
  name = "README";
  description = Some "Project documentation";
  mime_type = Some "text/markdown";
}

(* Define a prompt *)
let my_prompt : Mcp_types.prompt = {
  name = "code_review";
  description = Some "Review code for issues";
  arguments = Some [
    { name = "code"; description = Some "Code to review"; required = Some true };
    { name = "language"; description = Some "Programming language"; required = Some false };
  ];
}
```

### Automatic JSON Schema with ppx (v0.11.0+)

Use `ppx_deriving_jsonschema` to generate `input_schema` from OCaml record types:

```ocaml
(* ppx generates: echo_input_jsonschema : Yojson.Safe.t *)
type echo_input = {
  text: string;
  count: int option;
} [@@deriving yojson, jsonschema]

(* Use directly with make_tool -- no manual JSON Schema needed *)
let tool = Mcp_types.make_tool
  ~name:"echo"
  ~description:"Echoes back the input text"
  ~input_schema:echo_input_jsonschema
  ()

(* Type-safe argument parsing via ppx_deriving_yojson *)
let handler _ctx _name arguments =
  match arguments with
  | Some json ->
    begin match echo_input_of_yojson json with
    | Ok input -> Ok (Mcp_types.tool_result_of_text input.text)
    | Error e -> Error e
    end
  | None -> Error "missing arguments"
```

Add to your `dune` file:
```lisp
(preprocess (pps ppx_deriving_yojson ppx_deriving_jsonschema))
```

### HTTP Server (Streamable HTTP)

```ocaml
open Mcp_protocol
open Mcp_protocol_http

type echo_input = {
  text: string;
} [@@deriving yojson, jsonschema]

let echo_tool = Mcp_types.make_tool
  ~name:"echo"
  ~description:"Echoes back the input text"
  ~input_schema:echo_input_jsonschema ()

let echo_handler _ctx _name arguments =
  let text = match arguments with
    | Some json ->
      (match echo_input_of_yojson json with
       | Ok input -> input.text
       | Error _ -> "(invalid input)")
    | None -> "(no arguments)"
  in
  Ok (Mcp_types.tool_result_of_text (Printf.sprintf "Echo: %s" text))

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server =
    Http_server.create ~name:"my-server" ~version:"1.0.0" ()
    |> Http_server.add_tool echo_tool echo_handler
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
open Mcp_protocol_http

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let client = Http_client.create ~endpoint:"http://127.0.0.1:8080/mcp" ~net ~sw () in
  match Http_client.initialize client ~client_name:"my-client" ~client_version:"1.0" with
  | Ok result ->
    Printf.printf "Connected to %s\n" result.server_info.name;
    (* Use list_tools, call_tool, list_resources, etc. *)
    ignore (Http_client.close client)
  | Error e -> Printf.eprintf "Failed: %s\n" e
```

### OAuth Discovery + HTTPS (v0.12.0+)

Auto-discover OAuth endpoints and connect over HTTPS:

```ocaml
open Mcp_protocol_http

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in

  (* 1. Discover OAuth server metadata from issuer URL *)
  match Oauth_client.discover ~net ~sw ~issuer:"https://auth.example.com" with
  | Error e -> Printf.eprintf "Discovery failed: %s\n" e
  | Ok metadata ->
    Printf.printf "Auth endpoint: %s\n" metadata.authorization_endpoint;
    Printf.printf "Token endpoint: %s\n" metadata.token_endpoint;

    (* 2. Optional: Dynamic client registration *)
    let client_id = match metadata.registration_endpoint with
      | Some ep ->
        let req = Oauth_client.{
          client_name = "my-mcp-client";
          redirect_uris = ["http://localhost:9999/callback"];
          grant_types = ["authorization_code"];
          response_types = ["code"];
          token_endpoint_auth_method = "none";
        } in
        (match Oauth_client.register_client ~net ~sw
           ~registration_endpoint:ep ~request:req with
         | Ok cid -> cid
         | Error _ -> "pre-registered-client-id")
      | None -> "pre-registered-client-id"
    in

    (* 3. Generate PKCE and build authorization URL *)
    let _verifier, challenge = Oauth_client.generate_pkce () in
    let auth_url = Oauth_client.build_authorization_url
      ~authorization_endpoint:metadata.authorization_endpoint
      ~client_id ~redirect_uri:"http://localhost:9999/callback"
      ~scopes:["read"] ~state:"random" ~code_challenge:challenge () in
    Printf.printf "Visit: %s\n" auth_url
```

HTTPS is enabled automatically via `tls-eio` + system CA certificates.

### HTTP Content Negotiation

```ocaml
open Mcp_protocol

(* Check client capabilities from Accept header *)
let accept_header = "application/json, text/event-stream" in

if Http_negotiation.accepts_sse accept_header then
  (* Client supports Server-Sent Events *)
  print_endline "SSE supported"
else
  (* Fall back to regular HTTP *)
  print_endline "Using stateless HTTP"

(* Negotiate transport mode *)
let transport = Http_negotiation.negotiate_transport ~accept_header in
match transport with
| Http_negotiation.Streamable_http -> "Modern MCP with SSE"
| Http_negotiation.Sse_only -> "Legacy SSE mode"
| Http_negotiation.Stateless_http -> "Stateless HTTP"
```

### Protocol Version Handling

```ocaml
open Mcp_protocol

(* Check supported versions *)
let () =
  assert (Version.is_supported "2025-11-25");
  assert (Version.is_supported "2024-11-05");
  assert (not (Version.is_supported "2020-01-01"))

(* Negotiate version *)
let negotiated = Version.negotiate ~requested:"2025-11-25" in
(* Returns: Some "2025-11-25" *)

(* Check version features *)
let features = Version.features_of_version "2025-11-25" in
(* features.has_sampling = true *)
(* features.has_elicitation = true *)
(* features.has_streamable_http = true *)
```

### Initialize Handshake

```ocaml
open Mcp_protocol

(* Client sends initialize request *)
let init_params : Mcp_types.initialize_params = {
  protocol_version = "2025-11-25";
  capabilities = {
    roots = Some (`Assoc [("listChanged", `Bool true)]);
    sampling = None;
    elicitation = None;
    experimental = None;
  };
  client_info = { name = "my-client"; version = "1.0.0" };
}

(* Server responds with capabilities *)
let init_result : Mcp_types.initialize_result = {
  protocol_version = "2025-11-25";
  capabilities = {
    tools = Some (`Assoc [("listChanged", `Bool true)]);
    resources = Some (`Assoc [("subscribe", `Bool true)]);
    prompts = Some (`Assoc []);
    logging = None;
    experimental = None;
  };
  server_info = { name = "my-server"; version = "1.0.0" };
  instructions = Some "This server provides code analysis tools.";
}

(* Serialize to JSON *)
let json = Mcp_types.initialize_result_to_yojson init_result
```

## Supported Protocol Versions

| Version | Features |
|---------|----------|
| 2024-11-05 | Tools, Resources, Prompts (initial stable) |
| 2025-03-26 | + Elicitation, Streamable HTTP |
| 2025-11-25 | + Sampling, Enhanced capabilities (latest) |

## Error Codes

The SDK provides standard JSON-RPC and MCP-specific error codes:

```ocaml
open Mcp_protocol

(* JSON-RPC standard errors *)
Error_codes.parse_error        (* -32700 *)
Error_codes.invalid_request    (* -32600 *)
Error_codes.method_not_found   (* -32601 *)
Error_codes.invalid_params     (* -32602 *)
Error_codes.internal_error     (* -32603 *)

(* MCP-specific errors *)
Error_codes.connection_closed  (* -32001 *)
Error_codes.request_timeout    (* -32002 *)
Error_codes.resource_not_found (* -32003 *)
Error_codes.tool_execution_error (* -32004 *)
```

## Building from Source

```bash
git clone https://github.com/jeong-sik/mcp-protocol-sdk.git
cd mcp-protocol-sdk
opam install . --deps-only
dune build
```

## Running Tests

```bash
dune runtest
```

## License

MIT License

## References

- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
- [TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk)
- [Python SDK](https://github.com/modelcontextprotocol/python-sdk)
