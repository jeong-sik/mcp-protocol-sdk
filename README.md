# MCP Protocol SDK for OCaml

A pure OCaml implementation of the [Model Context Protocol](https://modelcontextprotocol.io/) (MCP) types and utilities.

## Overview

MCP enables LLMs to interact with external tools, resources, and prompts through a standardized protocol. This SDK provides:

- **JSON-RPC 2.0** types and utilities for wire protocol
- **MCP Primitives** (Tools, Resources, Prompts)
- **HTTP Negotiation** for content type handling
- **Protocol Versioning** with negotiation support

Setup 가이드: `docs/SETUP.md`

## Installation

```bash
opam pin add mcp_protocol git+https://github.com/jeong-sik/mcp-protocol-sdk.git
```

Or add to your `dune-project`:

```lisp
(depends
 (mcp_protocol (>= 0.1.0)))
```

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

(* Define a tool *)
let my_tool : Mcp_types.tool = {
  name = "calculate";
  description = Some "Perform mathematical calculations";
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
Error_codes.request_timeout    (* -32001 *)
Error_codes.resource_not_found (* -32002 *)
Error_codes.request_failed     (* -32003 *)
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
