(** MCP and JSON-RPC error codes.

    JSON-RPC 2.0 standard error codes: -32700 to -32600
    Server errors: -32000 to -32099
    MCP-specific errors: defined by the protocol
*)

(** {2 JSON-RPC Standard Error Codes} *)

(** Parse error: Invalid JSON was received *)
let parse_error = -32700

(** Invalid Request: The JSON sent is not a valid Request object *)
let invalid_request = -32600

(** Method not found: The method does not exist / is not available *)
let method_not_found = -32601

(** Invalid params: Invalid method parameter(s) *)
let invalid_params = -32602

(** Internal error: Internal JSON-RPC error *)
let internal_error = -32603

(** {2 Server Error Codes} *)

(** Server error range start: -32000 *)
let server_error_start = -32000

(** Server error range end: -32099 *)
let server_error_end = -32099

(** Check if error code is a server error *)
let is_server_error code =
  code >= server_error_end && code <= server_error_start

(** {2 MCP-Specific Error Codes} *)

(** Connection closed by peer *)
let connection_closed = -32001

(** Request timeout *)
let request_timeout = -32002

(** Resource not found *)
let resource_not_found = -32003

(** Tool execution error *)
let tool_execution_error = -32004

(** Prompt not found *)
let prompt_not_found = -32005

(** URL elicitation required *)
let url_elicitation_required = -32006

(** Stateless mode not supported *)
let stateless_mode_not_supported = -32007

(** {2 Error Code Descriptions} *)

(** Get a human-readable description for an error code *)
let describe code =
  match code with
  | -32700 -> "Parse error: Invalid JSON"
  | -32600 -> "Invalid Request: Not a valid JSON-RPC request"
  | -32601 -> "Method not found"
  | -32602 -> "Invalid params"
  | -32603 -> "Internal error"
  | -32001 -> "Connection closed"
  | -32002 -> "Request timeout"
  | -32003 -> "Resource not found"
  | -32004 -> "Tool execution error"
  | -32005 -> "Prompt not found"
  | -32006 -> "URL elicitation required"
  | -32007 -> "Stateless mode not supported"
  | c when is_server_error c -> Printf.sprintf "Server error (%d)" c
  | c -> Printf.sprintf "Unknown error (%d)" c
