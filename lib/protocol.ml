(** MCP Protocol - Backward compatibility module.

    Re-exports from new modules for existing code compatibility.
    New code should use Mcp_types, Jsonrpc, Error_codes directly.
*)

let protocol_version = Version.latest

type tool = Mcp_types.tool
type resource = Mcp_types.resource
type server_info = Mcp_types.server_info

type capabilities = {
  tools: bool;
  resources: bool;
}

let tool_to_json = Mcp_types.tool_to_yojson
let resource_to_json = Mcp_types.resource_to_yojson
let server_info_to_json = Mcp_types.server_info_to_yojson

let capabilities_to_json (c : capabilities) =
  let items = [] in
  let items = if c.tools then ("tools", `Assoc []) :: items else items in
  let items = if c.resources then ("resources", `Assoc []) :: items else items in
  `Assoc items

let make_response ~id result =
  Jsonrpc.make_response_json ~id ~result

let make_error ~id code message =
  Jsonrpc.make_error_json ~id ~code ~message ()

module ErrorCode = struct
  let parse_error = Error_codes.parse_error
  let invalid_request = Error_codes.invalid_request
  let method_not_found = Error_codes.method_not_found
  let invalid_params = Error_codes.invalid_params
  let internal_error = Error_codes.internal_error
end

let get_string key json =
  let open Yojson.Safe.Util in
  try Some (json |> member key |> to_string) with _ -> None

let get_int key json =
  let open Yojson.Safe.Util in
  try Some (json |> member key |> to_int) with _ -> None

let get_bool key json =
  let open Yojson.Safe.Util in
  try Some (json |> member key |> to_bool) with _ -> None

let get_string_list key json =
  let open Yojson.Safe.Util in
  try
    Some (json |> member key |> to_list |> List.map to_string)
  with _ ->
    match get_string key json with
    | Some s -> (try Some (Yojson.Safe.from_string s |> to_list |> List.map to_string) with _ -> None)
    | None -> None

let text_content text =
  `Assoc [
    ("type", `String "text");
    ("text", `String text);
  ]

let tool_result contents =
  `Assoc [("content", `List contents)]

let make_initialize_response ~id ~server_info ~capabilities =
  make_response ~id (`Assoc [
    ("protocolVersion", `String protocol_version);
    ("serverInfo", server_info_to_json server_info);
    ("capabilities", capabilities_to_json capabilities);
  ])

let make_tools_list_response ~id tools =
  make_response ~id (`Assoc [
    ("tools", `List (List.map tool_to_json tools));
  ])

let make_resources_list_response ~id resources =
  make_response ~id (`Assoc [
    ("resources", `List (List.map resource_to_json resources));
  ])

let make_tool_call_response ~id text =
  make_response ~id (tool_result [text_content text])
