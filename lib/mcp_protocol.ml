(** Model Context Protocol SDK for OCaml.

    This is the main entry point for the MCP protocol library.
    It re-exports all submodules for convenient access.

    Usage:
    {[
      open Mcp_protocol

      (* JSON-RPC messages *)
      let req = Jsonrpc.make_request
        ~id:(Jsonrpc.Int 1)
        ~method_:"tools/list"
        ()

      (* MCP types *)
      let tool : Mcp_types.tool = {
        name = "my_tool";
        description = Some "A useful tool";
        input_schema = `Assoc [];
      }

      (* HTTP negotiation *)
      let accepts_sse = Http_negotiation.accepts_sse accept_header
    ]}

    @see <https://modelcontextprotocol.io/docs> MCP Documentation
    @see <https://github.com/jeong-sik/mcp-protocol-sdk> GitHub Repository
*)

(** {1 Re-exported Modules} *)

(** JSON-RPC 2.0 types and utilities *)
module Jsonrpc = Jsonrpc

(** MCP primitive types (Tool, Resource, Prompt) *)
module Mcp_types = Mcp_types

(** Error codes for JSON-RPC and MCP *)
module Error_codes = Error_codes

(** HTTP content negotiation *)
module Http_negotiation = Http_negotiation

(** Protocol version handling *)
module Version = Version

(** Result types and progress tracking (Go SDK pattern) *)
module Mcp_result = Mcp_result

(** Session management and request tracking (Python SDK pattern) *)
module Session = Session

(** {1 Backward Compatibility Modules} *)

(** Legacy Protocol module for existing code *)
module Protocol = Protocol

(** Legacy Resources module for existing code *)
module Resources = Resources

(** {1 Convenience Re-exports} *)

(** Protocol version string *)
let protocol_version = Version.latest

(** Check if a version is supported *)
let is_version_supported = Version.is_supported

(** Make a JSON-RPC request *)
let make_request = Jsonrpc.make_request

(** Make a JSON-RPC notification *)
let make_notification = Jsonrpc.make_notification

(** Make a JSON-RPC success response *)
let make_response = Jsonrpc.make_response

(** Make a JSON-RPC error response *)
let make_error = Jsonrpc.make_error
