(** MCP Protocol - Backward compatibility module.

    @deprecated Use [Mcp_types], [Jsonrpc], [Error_codes] directly.
    This module will be removed in v0.4.0.
*)

(** Current MCP protocol version *)
let protocol_version = Version.latest

(** Tool definition type — re-export from Mcp_types *)
type tool = Mcp_types.tool

(** Resource definition type — re-export from Mcp_types *)
type resource = Mcp_types.resource

(** Server info — re-export from Mcp_types *)
type server_info = Mcp_types.server_info

(** Server capabilities *)
type capabilities = {
  tools: bool;
  resources: bool;
}

(** Convert tool to JSON *)
let tool_to_json = Mcp_types.tool_to_yojson

(** Convert resource to JSON *)
let resource_to_json = Mcp_types.resource_to_yojson

(** Convert server info to JSON *)
let server_info_to_json = Mcp_types.server_info_to_yojson

(** Convert capabilities to JSON *)
let capabilities_to_json (c : capabilities) =
  let items = [] in
  let items = if c.tools then ("tools", `Assoc []) :: items else items in
  let items = if c.resources then ("resources", `Assoc []) :: items else items in
  `Assoc items

(** Make JSON-RPC success response *)
let make_response ~id result =
  let msg = Jsonrpc.make_response ~id ~result in
  Jsonrpc.message_to_yojson msg

(** Make JSON-RPC error response *)
let make_error ~id code message =
  let msg = Jsonrpc.make_error ~id ~code ~message () in
  Jsonrpc.message_to_yojson msg

(** Standard error codes *)
module ErrorCode = struct
  let parse_error = Error_codes.parse_error
  let invalid_request = Error_codes.invalid_request
  let method_not_found = Error_codes.method_not_found
  let invalid_params = Error_codes.invalid_params
  let internal_error = Error_codes.internal_error
end

(** Extract string from JSON *)
let get_string key json =
  let open Yojson.Safe.Util in
  try Some (json |> member key |> to_string) with _ -> None

(** Extract int from JSON *)
let get_int key json =
  let open Yojson.Safe.Util in
  try Some (json |> member key |> to_int) with _ -> None

(** Extract bool from JSON *)
let get_bool key json =
  let open Yojson.Safe.Util in
  try Some (json |> member key |> to_bool) with _ -> None

(** Extract string list from JSON *)
let get_string_list key json =
  let open Yojson.Safe.Util in
  try
    Some (json |> member key |> to_list |> List.map to_string)
  with _ ->
    match get_string key json with
    | Some s -> (try Some (Yojson.Safe.from_string s |> to_list |> List.map to_string) with _ -> None)
    | None -> None

(** Tool call result content *)
let text_content text =
  `Assoc [
    ("type", `String "text");
    ("text", `String text);
  ]

(** Wrap tool result in MCP response format *)
let tool_result contents =
  `Assoc [("content", `List contents)]

(** Make initialize response *)
let make_initialize_response ~id ~server_info ~capabilities =
  make_response ~id (`Assoc [
    ("protocolVersion", `String protocol_version);
    ("serverInfo", server_info_to_json server_info);
    ("capabilities", capabilities_to_json capabilities);
  ])

(** Make tools/list response *)
let make_tools_list_response ~id tools =
  make_response ~id (`Assoc [
    ("tools", `List (List.map tool_to_json tools));
  ])

(** Make resources/list response *)
let make_resources_list_response ~id resources =
  make_response ~id (`Assoc [
    ("resources", `List (List.map resource_to_json resources));
  ])

(** Make tools/call response *)
let make_tool_call_response ~id text =
  make_response ~id (tool_result [text_content text])
