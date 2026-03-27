open Alcotest

module Protocol = Mcp_protocol.Protocol
module Resources = Mcp_protocol.Resources
module Types = Mcp_protocol.Mcp_types
module Jsonrpc = Mcp_protocol.Jsonrpc

let test_protocol_capabilities_to_json () =
  let json = Protocol.capabilities_to_json { tools = true; resources = true } in
  let open Yojson.Safe.Util in
  check bool "tools present" true (json |> member "tools" <> `Null);
  check bool "resources present" true (json |> member "resources" <> `Null)

let test_protocol_initialize_response () =
  let server_info : Protocol.server_info = {
    name = "compat-test";
    version = "1.0.0";
  } in
  let json =
    Protocol.make_initialize_response
      ~id:(Jsonrpc.Int 1)
      ~server_info
      ~capabilities:{ tools = true; resources = false }
  in
  let open Yojson.Safe.Util in
  check string "jsonrpc" "2.0" (json |> member "jsonrpc" |> to_string);
  check string "protocolVersion" Mcp_protocol.protocol_version
    (json |> member "result" |> member "protocolVersion" |> to_string);
  check string "server name" "compat-test"
    (json |> member "result" |> member "serverInfo" |> member "name" |> to_string)

let test_resources_helpers () =
  let resource : Resources.resource = {
    uri = "memory://x";
    name = "X";
    title = None;
    description = Some "desc";
    mime_type = Some "text/plain";
    icon = None;
  } in
  let template : Resources.resource_template = {
    uri_template = "memory://{id}";
    name = "T";
    title = None;
    description = Some "desc";
    mime_type = Some "text/plain";
    icon = None;
  } in
  let content : Resources.content = {
    uri = "memory://x";
    mime_type = Some "text/plain";
    text = Some "hello";
    blob = None;
  } in
  let open Yojson.Safe.Util in
  check int "resources count" 1
    (Resources.list_result [resource] |> member "resources" |> to_list |> List.length);
  check int "templates count" 1
    (Resources.templates_list_result [template] |> member "resourceTemplates" |> to_list |> List.length);
  check int "contents count" 1
    (Resources.read_result [content] |> member "contents" |> to_list |> List.length)

let () =
  run "backward_compat" [
    ("protocol", [
      test_case "capabilities_to_json" `Quick test_protocol_capabilities_to_json;
      test_case "initialize_response" `Quick test_protocol_initialize_response;
    ]);
    ("resources", [
      test_case "helper results" `Quick test_resources_helpers;
    ]);
  ]
