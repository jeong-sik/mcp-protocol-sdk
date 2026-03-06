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

(* --- root --- *)

let test_root_roundtrip () =
  let r : Mcp_types.root = {
    uri = "file:///home/user/project";
    name = Some "My Project";
  } in
  let j = Mcp_types.root_to_yojson r in
  match Mcp_types.root_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "uri" "file:///home/user/project" decoded.uri;
    Alcotest.(check (option string)) "name" (Some "My Project") decoded.name
  | Error e -> Alcotest.fail e

let test_root_no_name () =
  let r : Mcp_types.root = {
    uri = "file:///tmp";
    name = None;
  } in
  let j = Mcp_types.root_to_yojson r in
  match Mcp_types.root_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "uri" "file:///tmp" decoded.uri;
    Alcotest.(check (option string)) "no name" None decoded.name
  | Error e -> Alcotest.fail e

let test_make_root () =
  let r = Mcp_types.make_root ~uri:"file:///a" ~name:"a" () in
  Alcotest.(check string) "uri" "file:///a" r.uri;
  Alcotest.(check (option string)) "name" (Some "a") r.name

let test_make_root_minimal () =
  let r = Mcp_types.make_root ~uri:"file:///b" () in
  Alcotest.(check (option string)) "no name" None r.name

(* --- roots_capability --- *)

let test_roots_capability_roundtrip () =
  let cap : Mcp_types.roots_capability = {
    list_changed = Some true;
  } in
  let j = Mcp_types.roots_capability_to_yojson cap in
  Alcotest.(check json) "listChanged key"
    (`Assoc [("listChanged", `Bool true)]) j;
  match Mcp_types.roots_capability_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option bool)) "list_changed" (Some true) decoded.list_changed
  | Error e -> Alcotest.fail e

let test_roots_capability_none () =
  let cap : Mcp_types.roots_capability = { list_changed = None } in
  let j = Mcp_types.roots_capability_to_yojson cap in
  match Mcp_types.roots_capability_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option bool)) "none" None decoded.list_changed
  | Error e -> Alcotest.fail e

(* --- completion_reference --- *)

let test_completion_ref_prompt_roundtrip () =
  let ref_ = Mcp_types.Prompt_ref { name = "code_review" } in
  let j = Mcp_types.completion_reference_to_yojson ref_ in
  Alcotest.(check json) "type field"
    (`String "ref/prompt")
    (match j with `Assoc f -> List.assoc "type" f | _ -> `Null);
  match Mcp_types.completion_reference_of_yojson j with
  | Ok (Prompt_ref { name }) ->
    Alcotest.(check string) "name" "code_review" name
  | Ok (Resource_ref _) -> Alcotest.fail "expected Prompt_ref"
  | Error e -> Alcotest.fail e

let test_completion_ref_resource_roundtrip () =
  let ref_ = Mcp_types.Resource_ref { uri = "file:///doc.md" } in
  let j = Mcp_types.completion_reference_to_yojson ref_ in
  match Mcp_types.completion_reference_of_yojson j with
  | Ok (Resource_ref { uri }) ->
    Alcotest.(check string) "uri" "file:///doc.md" uri
  | Ok (Prompt_ref _) -> Alcotest.fail "expected Resource_ref"
  | Error e -> Alcotest.fail e

let test_completion_ref_unknown_type () =
  let j = `Assoc [("type", `String "ref/unknown"); ("x", `String "y")] in
  match Mcp_types.completion_reference_of_yojson j with
  | Error msg ->
    Alcotest.(check bool) "contains 'unknown type'"
      true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error for unknown type"

let test_completion_ref_missing_type () =
  let j = `Assoc [("name", `String "test")] in
  match Mcp_types.completion_reference_of_yojson j with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for missing type"

let test_completion_ref_not_object () =
  match Mcp_types.completion_reference_of_yojson (`String "bad") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-object"

(* --- completion_argument --- *)

let test_completion_argument_roundtrip () =
  let arg : Mcp_types.completion_argument = {
    name = "language";
    value = "oc";
  } in
  let j = Mcp_types.completion_argument_to_yojson arg in
  match Mcp_types.completion_argument_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "name" "language" decoded.name;
    Alcotest.(check string) "value" "oc" decoded.value
  | Error e -> Alcotest.fail e

let test_make_completion_argument () =
  let arg = Mcp_types.make_completion_argument ~name:"x" ~value:"y" in
  Alcotest.(check string) "name" "x" arg.name;
  Alcotest.(check string) "value" "y" arg.value

(* --- completion_result --- *)

let test_completion_result_roundtrip () =
  let cr : Mcp_types.completion_result = {
    values = ["ocaml"; "ocamlformat"];
    total = Some 5;
    has_more = Some true;
  } in
  let j = Mcp_types.completion_result_to_yojson cr in
  match Mcp_types.completion_result_of_yojson j with
  | Ok decoded ->
    Alcotest.(check int) "values count" 2 (List.length decoded.values);
    Alcotest.(check (option int)) "total" (Some 5) decoded.total;
    Alcotest.(check (option bool)) "has_more" (Some true) decoded.has_more
  | Error e -> Alcotest.fail e

let test_completion_result_minimal () =
  let cr : Mcp_types.completion_result = {
    values = [];
    total = None;
    has_more = None;
  } in
  let j = Mcp_types.completion_result_to_yojson cr in
  match Mcp_types.completion_result_of_yojson j with
  | Ok decoded ->
    Alcotest.(check int) "empty values" 0 (List.length decoded.values);
    Alcotest.(check (option int)) "no total" None decoded.total;
    Alcotest.(check (option bool)) "no has_more" None decoded.has_more
  | Error e -> Alcotest.fail e

let test_make_completion_result () =
  let cr = Mcp_types.make_completion_result
    ~values:["a"; "b"] ~total:10 ~has_more:true () in
  Alcotest.(check int) "values" 2 (List.length cr.values);
  Alcotest.(check (option int)) "total" (Some 10) cr.total;
  Alcotest.(check (option bool)) "has_more" (Some true) cr.has_more

let test_make_completion_result_minimal () =
  let cr = Mcp_types.make_completion_result ~values:[] () in
  Alcotest.(check (option int)) "no total" None cr.total;
  Alcotest.(check (option bool)) "no has_more" None cr.has_more

let test_completion_result_has_more_key () =
  let cr : Mcp_types.completion_result = {
    values = ["x"];
    total = None;
    has_more = Some false;
  } in
  let j = Mcp_types.completion_result_to_yojson cr in
  match j with
  | `Assoc fields ->
    Alcotest.(check bool) "hasMore key exists"
      true (List.mem_assoc "hasMore" fields)
  | _ -> Alcotest.fail "expected object"

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
    "root", [
      Alcotest.test_case "round-trip" `Quick test_root_roundtrip;
      Alcotest.test_case "no name" `Quick test_root_no_name;
      Alcotest.test_case "make_root" `Quick test_make_root;
      Alcotest.test_case "make_root minimal" `Quick test_make_root_minimal;
    ];
    "roots_capability", [
      Alcotest.test_case "round-trip" `Quick test_roots_capability_roundtrip;
      Alcotest.test_case "none" `Quick test_roots_capability_none;
    ];
    "completion_reference", [
      Alcotest.test_case "prompt round-trip" `Quick test_completion_ref_prompt_roundtrip;
      Alcotest.test_case "resource round-trip" `Quick test_completion_ref_resource_roundtrip;
      Alcotest.test_case "unknown type" `Quick test_completion_ref_unknown_type;
      Alcotest.test_case "missing type" `Quick test_completion_ref_missing_type;
      Alcotest.test_case "not object" `Quick test_completion_ref_not_object;
    ];
    "completion_argument", [
      Alcotest.test_case "round-trip" `Quick test_completion_argument_roundtrip;
      Alcotest.test_case "make helper" `Quick test_make_completion_argument;
    ];
    "completion_result", [
      Alcotest.test_case "round-trip" `Quick test_completion_result_roundtrip;
      Alcotest.test_case "minimal" `Quick test_completion_result_minimal;
      Alcotest.test_case "make helper" `Quick test_make_completion_result;
      Alcotest.test_case "make minimal" `Quick test_make_completion_result_minimal;
      Alcotest.test_case "hasMore key" `Quick test_completion_result_has_more_key;
    ];
  ]
