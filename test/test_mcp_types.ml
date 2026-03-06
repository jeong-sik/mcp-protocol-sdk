open Mcp_protocol

let json = Alcotest.testable
  (fun fmt j -> Format.pp_print_string fmt (Yojson.Safe.to_string j))
  (fun a b -> Yojson.Safe.equal a b)

(* --- protocol_version --- *)

let test_protocol_version_to_string () =
  Alcotest.(check string) "2024-11-05"
    "2024-11-05"
    (Mcp_types.protocol_version_to_string V_2024_11_05);
  Alcotest.(check string) "2025-03-26"
    "2025-03-26"
    (Mcp_types.protocol_version_to_string V_2025_03_26);
  Alcotest.(check string) "2025-11-25"
    "2025-11-25"
    (Mcp_types.protocol_version_to_string V_2025_11_25)

let test_protocol_version_of_string () =
  Alcotest.(check (option pass)) "valid"
    (Some Mcp_types.V_2024_11_05)
    (Mcp_types.protocol_version_of_string "2024-11-05");
  Alcotest.(check (option pass)) "unknown"
    None
    (Mcp_types.protocol_version_of_string "1999-01-01")

(* --- tool --- *)

let test_tool_roundtrip () =
  let tool : Mcp_types.tool = {
    name = "my_tool";
    description = Some "A useful tool";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  } in
  let j = Mcp_types.tool_to_yojson tool in
  match Mcp_types.tool_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "name" "my_tool" decoded.name;
    Alcotest.(check (option string)) "description" (Some "A useful tool") decoded.description
  | Error e -> Alcotest.fail e

let test_tool_no_description () =
  let tool : Mcp_types.tool = {
    name = "minimal";
    description = None;
    input_schema = `Assoc [("type", `String "object")];
  } in
  let j = Mcp_types.tool_to_yojson tool in
  match Mcp_types.tool_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option string)) "no desc" None decoded.description
  | Error e -> Alcotest.fail e

(* --- tool_def alias --- *)

let test_tool_def_alias () =
  let td : Mcp_types.tool_def = {
    name = "alias_test";
    description = Some "via alias";
    input_schema = `Assoc [];
  } in
  let j = Mcp_types.tool_to_yojson td in
  match Mcp_types.tool_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "name" "alias_test" decoded.name
  | Error e -> Alcotest.fail e

(* --- resource --- *)

let test_resource_roundtrip () =
  let res : Mcp_types.resource = {
    uri = "file:///test.txt";
    name = "test";
    description = Some "A test file";
    mime_type = Some "text/plain";
  } in
  let j = Mcp_types.resource_to_yojson res in
  match Mcp_types.resource_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "uri" "file:///test.txt" decoded.uri;
    Alcotest.(check (option string)) "mime" (Some "text/plain") decoded.mime_type
  | Error e -> Alcotest.fail e

(* --- prompt --- *)

let test_prompt_roundtrip () =
  let p : Mcp_types.prompt = {
    name = "code_review";
    description = Some "Review code";
    arguments = Some [{
      name = "code";
      description = Some "The code to review";
      required = Some true;
    }];
  } in
  let j = Mcp_types.prompt_to_yojson p in
  match Mcp_types.prompt_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "name" "code_review" decoded.name;
    Alcotest.(check int) "args count" 1
      (List.length (Option.value ~default:[] decoded.arguments))
  | Error e -> Alcotest.fail e

(* --- role --- *)

let test_role_roundtrip () =
  let check_role role expected_str =
    let j = Mcp_types.role_to_yojson role in
    Alcotest.(check json) ("role " ^ expected_str)
      (`String expected_str) j;
    Alcotest.(check (result pass string)) "parse back"
      (Ok role) (Mcp_types.role_of_yojson j)
  in
  check_role User "user";
  check_role Assistant "assistant"

(* --- server_capabilities --- *)

let test_capabilities_roundtrip () =
  let caps : Mcp_types.server_capabilities = {
    tools = Some (`Assoc []);
    resources = Some (`Assoc []);
    prompts = None;
    logging = None;
    experimental = None;
  } in
  let j = Mcp_types.server_capabilities_to_yojson caps in
  match Mcp_types.server_capabilities_of_yojson j with
  | Ok decoded ->
    Alcotest.(check bool) "tools present" true (Option.is_some decoded.tools);
    Alcotest.(check bool) "prompts absent" true (Option.is_none decoded.prompts)
  | Error e -> Alcotest.fail e

(* --- initialize --- *)

let test_initialize_params_roundtrip () =
  let params : Mcp_types.initialize_params = {
    protocol_version = "2025-03-26";
    capabilities = {
      roots = None;
      sampling = None;
      elicitation = None;
      experimental = None;
    };
    client_info = { name = "test-client"; version = "1.0" };
  } in
  let j = Mcp_types.initialize_params_to_yojson params in
  match Mcp_types.initialize_params_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "version" "2025-03-26" decoded.protocol_version;
    Alcotest.(check string) "client name" "test-client" decoded.client_info.name
  | Error e -> Alcotest.fail e

let test_initialize_result_roundtrip () =
  let result : Mcp_types.initialize_result = {
    protocol_version = "2025-03-26";
    capabilities = {
      tools = Some (`Assoc []);
      resources = None;
      prompts = None;
      logging = None;
      experimental = None;
    };
    server_info = { name = "test-server"; version = "0.1.0" };
    instructions = Some "Use these tools wisely";
  } in
  let j = Mcp_types.initialize_result_to_yojson result in
  match Mcp_types.initialize_result_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "server name" "test-server" decoded.server_info.name;
    Alcotest.(check (option string)) "instructions"
      (Some "Use these tools wisely") decoded.instructions
  | Error e -> Alcotest.fail e

(* --- pagination --- *)

let test_paginated_result_roundtrip () =
  let pr : string Mcp_types.paginated_result = {
    items = ["a"; "b"; "c"];
    next_cursor = Some "cursor-xyz";
  } in
  let j = Mcp_types.paginated_result_to_yojson
    (fun s -> `String s) pr in
  match Mcp_types.paginated_result_of_yojson
    (function `String s -> Ok s | _ -> Error "not string") j with
  | Ok decoded ->
    Alcotest.(check int) "items count" 3 (List.length decoded.items);
    Alcotest.(check (option string)) "cursor"
      (Some "cursor-xyz") decoded.next_cursor
  | Error e -> Alcotest.fail e

let test_paginated_result_no_cursor () =
  let pr : string Mcp_types.paginated_result = {
    items = ["x"];
    next_cursor = None;
  } in
  let j = Mcp_types.paginated_result_to_yojson
    (fun s -> `String s) pr in
  match Mcp_types.paginated_result_of_yojson
    (function `String s -> Ok s | _ -> Error "not string") j with
  | Ok decoded ->
    Alcotest.(check (option string)) "no cursor" None decoded.next_cursor
  | Error e -> Alcotest.fail e

(* --- make_tool helper --- *)

let test_make_tool () =
  let t = Mcp_types.make_tool ~name:"test" ~description:"desc"
    ~input_schema:(`Assoc [("type", `String "object")]) () in
  Alcotest.(check string) "name" "test" t.name;
  Alcotest.(check (option string)) "desc" (Some "desc") t.description

let test_make_tool_minimal () =
  let t = Mcp_types.make_tool ~name:"minimal" () in
  Alcotest.(check string) "name" "minimal" t.name;
  Alcotest.(check (option string)) "no desc" None t.description;
  Alcotest.(check json) "default schema"
    (`Assoc [("type", `String "object")])
    t.input_schema

(* --- make_resource helper --- *)

let test_make_resource () =
  let r = Mcp_types.make_resource ~uri:"file:///a" ~name:"a"
    ~description:"desc" ~mime_type:"text/plain" () in
  Alcotest.(check string) "uri" "file:///a" r.uri;
  Alcotest.(check (option string)) "mime" (Some "text/plain") r.mime_type

(* --- make_prompt helper --- *)

let test_make_prompt () =
  let p = Mcp_types.make_prompt ~name:"review"
    ~description:"Review code" () in
  Alcotest.(check string) "name" "review" p.name;
  Alcotest.(check (option string)) "desc" (Some "Review code") p.description

(* --- tool_result_of_text / tool_result_of_error --- *)

let test_tool_result_of_text () =
  let tr = Mcp_types.tool_result_of_text "hello" in
  Alcotest.(check int) "content count" 1 (List.length tr.content);
  Alcotest.(check (option bool)) "not error" None tr.is_error

let test_tool_result_of_error () =
  let tr = Mcp_types.tool_result_of_error "something failed" in
  Alcotest.(check (option bool)) "is error" (Some true) tr.is_error

(* --- Suite --- *)

let () =
  Alcotest.run "Mcp_types" [
    "protocol_version", [
      Alcotest.test_case "to_string" `Quick test_protocol_version_to_string;
      Alcotest.test_case "of_string" `Quick test_protocol_version_of_string;
    ];
    "tool", [
      Alcotest.test_case "round-trip" `Quick test_tool_roundtrip;
      Alcotest.test_case "no description" `Quick test_tool_no_description;
      Alcotest.test_case "tool_def alias" `Quick test_tool_def_alias;
    ];
    "resource", [
      Alcotest.test_case "round-trip" `Quick test_resource_roundtrip;
    ];
    "prompt", [
      Alcotest.test_case "round-trip" `Quick test_prompt_roundtrip;
    ];
    "role", [
      Alcotest.test_case "round-trip" `Quick test_role_roundtrip;
    ];
    "capabilities", [
      Alcotest.test_case "round-trip" `Quick test_capabilities_roundtrip;
    ];
    "initialize", [
      Alcotest.test_case "params round-trip" `Quick test_initialize_params_roundtrip;
      Alcotest.test_case "result round-trip" `Quick test_initialize_result_roundtrip;
    ];
    "pagination", [
      Alcotest.test_case "round-trip" `Quick test_paginated_result_roundtrip;
      Alcotest.test_case "no cursor" `Quick test_paginated_result_no_cursor;
    ];
    "make_helpers", [
      Alcotest.test_case "make_tool" `Quick test_make_tool;
      Alcotest.test_case "make_tool minimal" `Quick test_make_tool_minimal;
      Alcotest.test_case "make_resource" `Quick test_make_resource;
      Alcotest.test_case "make_prompt" `Quick test_make_prompt;
    ];
    "tool_result_helpers", [
      Alcotest.test_case "of_text" `Quick test_tool_result_of_text;
      Alcotest.test_case "of_error" `Quick test_tool_result_of_error;
    ];
  ]
