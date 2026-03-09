(** MCP Protocol - Backward compatibility module.

    Re-exports from new modules for existing code compatibility.
    New code should use Mcp_types, Jsonrpc, Error_codes directly. *)

val protocol_version : string

type tool = Mcp_types.tool
type resource = Mcp_types.resource
type server_info = Mcp_types.server_info

type capabilities = {
  tools: bool;
  resources: bool;
}

val tool_to_json : tool -> Yojson.Safe.t
val resource_to_json : resource -> Yojson.Safe.t
val server_info_to_json : server_info -> Yojson.Safe.t
val capabilities_to_json : capabilities -> Yojson.Safe.t

val make_response : id:Jsonrpc.id -> Yojson.Safe.t -> Yojson.Safe.t
val make_error : id:Jsonrpc.id -> int -> string -> Yojson.Safe.t

module ErrorCode : sig
  val parse_error : int
  val invalid_request : int
  val method_not_found : int
  val invalid_params : int
  val internal_error : int
end

val get_string : string -> Yojson.Safe.t -> string option
val get_int : string -> Yojson.Safe.t -> int option
val get_bool : string -> Yojson.Safe.t -> bool option
val get_string_list : string -> Yojson.Safe.t -> string list option

val text_content : string -> Yojson.Safe.t
val tool_result : Yojson.Safe.t list -> Yojson.Safe.t

val make_initialize_response :
  id:Jsonrpc.id ->
  server_info:server_info ->
  capabilities:capabilities ->
  Yojson.Safe.t

val make_tools_list_response :
  id:Jsonrpc.id -> tool list -> Yojson.Safe.t

val make_resources_list_response :
  id:Jsonrpc.id -> resource list -> Yojson.Safe.t

val make_tool_call_response :
  id:Jsonrpc.id -> string -> Yojson.Safe.t
