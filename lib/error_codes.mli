(** MCP and JSON-RPC error codes. *)

(** {2 JSON-RPC Standard Error Codes} *)

val parse_error : int
val invalid_request : int
val method_not_found : int
val invalid_params : int
val internal_error : int

(** {2 Server Error Codes} *)

val server_error_start : int
val server_error_end : int
val is_server_error : int -> bool

(** {2 MCP-Specific Error Codes} *)

val connection_closed : int
val request_timeout : int
val resource_not_found : int
val tool_execution_error : int
val prompt_not_found : int
val url_elicitation_required : int
val stateless_mode_not_supported : int

(** {2 Descriptions} *)

val describe : int -> string
