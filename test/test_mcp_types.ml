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
    title = None;
    annotations = None;
    icon = None;
    output_schema = None;
    execution = None;
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
    title = None;
    annotations = None;
    icon = None;
    output_schema = None;
    execution = None;
  } in
  let j = Mcp_types.tool_to_yojson tool in
  match Mcp_types.tool_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option string)) "no desc" None decoded.description
  | Error e -> Alcotest.fail e

let test_tool_input_schema_snake_case () =
  let tool_json =
    `Assoc [
      ("name", `String "compat_tool");
      ("description", `String "snake_case schema");
      ("input_schema", `Assoc [("type", `String "object")]);
    ]
  in
  match Mcp_types.tool_of_yojson tool_json with
  | Ok decoded ->
    Alcotest.(check string) "name" "compat_tool" decoded.name;
    Alcotest.check json "schema"
      (`Assoc [("type", `String "object")]) decoded.input_schema
  | Error e -> Alcotest.fail e

(* --- tool_def alias --- *)

let test_tool_def_alias () =
  let td : Mcp_types.tool_def = {
    name = "alias_test";
    description = Some "via alias";
    input_schema = `Assoc [];
    title = None;
    annotations = None;
    icon = None;
    output_schema = None;
    execution = None;
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
    icon = None;
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
    icon = None;
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
    tools = Some { list_changed = Some true };
    resources = Some { subscribe = Some true; list_changed = None };
    prompts = None;
    logging = Some ();
    completions = None;
    experimental = None;
  } in
  let j = Mcp_types.server_capabilities_to_yojson caps in
  match Mcp_types.server_capabilities_of_yojson j with
  | Ok decoded ->
    Alcotest.(check bool) "tools present" true (Option.is_some decoded.tools);
    Alcotest.(check bool) "prompts absent" true (Option.is_none decoded.prompts);
    Alcotest.(check bool) "logging present" true (Option.is_some decoded.logging)
  | Error e -> Alcotest.fail e

(* --- initialize --- *)

let test_initialize_params_roundtrip () =
  let params : Mcp_types.initialize_params = {
    protocol_version = "2025-03-26";
    capabilities = {
      roots = Some { list_changed = Some false };
      sampling = Some ();
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
      tools = Some { list_changed = Some false };
      resources = None;
      prompts = None;
      logging = Some ();
      completions = None;
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

(* --- content_annotations --- *)

let test_content_annotations_roundtrip () =
  let ann : Mcp_types.content_annotations = {
    audience = Some [User; Assistant];
    priority = Some 0.8;
  } in
  let j = Mcp_types.content_annotations_to_yojson ann in
  match Mcp_types.content_annotations_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option (float 0.001))) "priority" (Some 0.8) decoded.priority;
    Alcotest.(check bool) "audience present" true (Option.is_some decoded.audience)
  | Error e -> Alcotest.fail e

let test_content_annotations_empty () =
  let ann : Mcp_types.content_annotations = {
    audience = None;
    priority = None;
  } in
  let j = Mcp_types.content_annotations_to_yojson ann in
  match Mcp_types.content_annotations_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option (float 0.001))) "no priority" None decoded.priority;
    Alcotest.(check bool) "no audience" true (Option.is_none decoded.audience)
  | Error e -> Alcotest.fail e

(* --- tool_annotations --- *)

let test_tool_annotations_full () =
  let ann : Mcp_types.tool_annotations = {
    title = Some "My Tool";
    read_only_hint = Some true;
    destructive_hint = Some false;
    idempotent_hint = Some true;
    open_world_hint = Some false;
  } in
  let j = Mcp_types.tool_annotations_to_yojson ann in
  (* Check JSON key names follow camelCase *)
  (match j with
   | `Assoc fields ->
     Alcotest.(check bool) "readOnlyHint key"
       true (List.mem_assoc "readOnlyHint" fields);
     Alcotest.(check bool) "destructiveHint key"
       true (List.mem_assoc "destructiveHint" fields)
   | _ -> Alcotest.fail "expected object");
  match Mcp_types.tool_annotations_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option string)) "title" (Some "My Tool") decoded.title;
    Alcotest.(check (option bool)) "readOnly" (Some true) decoded.read_only_hint;
    Alcotest.(check (option bool)) "destructive" (Some false) decoded.destructive_hint
  | Error e -> Alcotest.fail e

let test_tool_annotations_empty () =
  let ann : Mcp_types.tool_annotations = {
    title = None;
    read_only_hint = None;
    destructive_hint = None;
    idempotent_hint = None;
    open_world_hint = None;
  } in
  let j = Mcp_types.tool_annotations_to_yojson ann in
  match Mcp_types.tool_annotations_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option string)) "no title" None decoded.title;
    Alcotest.(check (option bool)) "no readOnly" None decoded.read_only_hint
  | Error e -> Alcotest.fail e

let test_tool_annotations_partial () =
  let ann : Mcp_types.tool_annotations = {
    title = None;
    read_only_hint = Some true;
    destructive_hint = None;
    idempotent_hint = None;
    open_world_hint = None;
  } in
  let j = Mcp_types.tool_annotations_to_yojson ann in
  match Mcp_types.tool_annotations_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option bool)) "readOnly" (Some true) decoded.read_only_hint
  | Error e -> Alcotest.fail e

(* --- tool with annotations --- *)

let test_tool_with_annotations () =
  let ann : Mcp_types.tool_annotations = {
    title = Some "Display Title";
    read_only_hint = Some true;
    destructive_hint = None;
    idempotent_hint = None;
    open_world_hint = None;
  } in
  let t = Mcp_types.make_tool ~name:"annotated"
    ~description:"A tool with annotations"
    ~title:"Display Title"
    ~annotations:ann () in
  Alcotest.(check (option string)) "title" (Some "Display Title") t.title;
  Alcotest.(check bool) "annotations present" true (Option.is_some t.annotations);
  let j = Mcp_types.tool_to_yojson t in
  match Mcp_types.tool_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option string)) "decoded title" (Some "Display Title") decoded.title;
    Alcotest.(check bool) "decoded annotations" true (Option.is_some decoded.annotations)
  | Error e -> Alcotest.fail e

let test_tool_without_annotations () =
  let t = Mcp_types.make_tool ~name:"simple" () in
  Alcotest.(check (option string)) "no title" None t.title;
  Alcotest.(check bool) "no annotations" true (Option.is_none t.annotations);
  let j = Mcp_types.tool_to_yojson t in
  match Mcp_types.tool_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option string)) "decoded no title" None decoded.title;
    Alcotest.(check bool) "decoded no ann" true (Option.is_none decoded.annotations)
  | Error e -> Alcotest.fail e

(* --- audio content --- *)

let test_audio_content_roundtrip () =
  let content = Mcp_types.AudioContent {
    type_ = "audio";
    data = "base64audiodata";
    mime_type = "audio/wav";
    annotations = None;
  } in
  let j = Mcp_types.tool_content_to_yojson content in
  match Mcp_types.tool_content_of_yojson j with
  | Ok (AudioContent { data; mime_type; _ }) ->
    Alcotest.(check string) "data" "base64audiodata" data;
    Alcotest.(check string) "mime_type" "audio/wav" mime_type
  | Ok _ -> Alcotest.fail "expected AudioContent"
  | Error e -> Alcotest.fail e

let test_audio_content_with_annotations () =
  let ann : Mcp_types.content_annotations = {
    audience = Some [User];
    priority = Some 0.5;
  } in
  let content = Mcp_types.AudioContent {
    type_ = "audio";
    data = "data";
    mime_type = "audio/mp3";
    annotations = Some ann;
  } in
  let j = Mcp_types.tool_content_to_yojson content in
  match Mcp_types.tool_content_of_yojson j with
  | Ok (AudioContent { annotations = Some decoded_ann; _ }) ->
    Alcotest.(check (option (float 0.001))) "priority" (Some 0.5) decoded_ann.priority
  | Ok (AudioContent { annotations = None; _ }) ->
    Alcotest.fail "expected annotations"
  | Ok _ -> Alcotest.fail "expected AudioContent"
  | Error e -> Alcotest.fail e

(* --- resource_link content --- *)

let test_resource_link_content_roundtrip () =
  let content = Mcp_types.ResourceLinkContent {
    type_ = "resource_link";
    uri = "file:///doc.md";
    name = Some "doc.md";
    description = Some "A document";
    mime_type = Some "text/markdown";
    annotations = None;
  } in
  let j = Mcp_types.tool_content_to_yojson content in
  match Mcp_types.tool_content_of_yojson j with
  | Ok (ResourceLinkContent { uri; name; description; mime_type; _ }) ->
    Alcotest.(check string) "uri" "file:///doc.md" uri;
    Alcotest.(check (option string)) "name" (Some "doc.md") name;
    Alcotest.(check (option string)) "description" (Some "A document") description;
    Alcotest.(check (option string)) "mime_type" (Some "text/markdown") mime_type
  | Ok _ -> Alcotest.fail "expected ResourceLinkContent"
  | Error e -> Alcotest.fail e

let test_resource_link_content_minimal () =
  let content = Mcp_types.ResourceLinkContent {
    type_ = "resource_link";
    uri = "file:///x";
    name = None;
    description = None;
    mime_type = None;
    annotations = None;
  } in
  let j = Mcp_types.tool_content_to_yojson content in
  match Mcp_types.tool_content_of_yojson j with
  | Ok (ResourceLinkContent { uri; name; _ }) ->
    Alcotest.(check string) "uri" "file:///x" uri;
    Alcotest.(check (option string)) "no name" None name
  | Ok _ -> Alcotest.fail "expected ResourceLinkContent"
  | Error e -> Alcotest.fail e

(* --- tool_result with structured_content --- *)

let test_tool_result_structured_content () =
  let tr : Mcp_types.tool_result = {
    content = [Mcp_types.TextContent { type_ = "text"; text = "ok"; annotations = None }];
    is_error = None;
    structured_content = Some (`Assoc [("key", `String "value")]);
  } in
  let j = Mcp_types.tool_result_to_yojson tr in
  match Mcp_types.tool_result_of_yojson j with
  | Ok decoded ->
    Alcotest.(check bool) "structured present"
      true (Option.is_some decoded.structured_content)
  | Error e -> Alcotest.fail e

let test_tool_result_no_structured_content () =
  let tr = Mcp_types.tool_result_of_text "hello" in
  let j = Mcp_types.tool_result_to_yojson tr in
  match Mcp_types.tool_result_of_yojson j with
  | Ok decoded ->
    Alcotest.(check bool) "no structured"
      true (Option.is_none decoded.structured_content)
  | Error e -> Alcotest.fail e

(* --- text content with annotations --- *)

let test_text_content_with_annotations () =
  let ann : Mcp_types.content_annotations = {
    audience = Some [Assistant];
    priority = Some 1.0;
  } in
  let content = Mcp_types.TextContent {
    type_ = "text";
    text = "annotated text";
    annotations = Some ann;
  } in
  let j = Mcp_types.tool_content_to_yojson content in
  match Mcp_types.tool_content_of_yojson j with
  | Ok (TextContent { text; annotations = Some decoded_ann; _ }) ->
    Alcotest.(check string) "text" "annotated text" text;
    Alcotest.(check (option (float 0.001))) "priority" (Some 1.0) decoded_ann.priority
  | Ok (TextContent { annotations = None; _ }) ->
    Alcotest.fail "expected annotations on text content"
  | Ok _ -> Alcotest.fail "expected TextContent"
  | Error e -> Alcotest.fail e

(* --- elicitation --- *)

let test_elicitation_schema_roundtrip () =
  let schema : Mcp_types.elicitation_schema = {
    type_ = "object";
    properties = [
      ("name", `Assoc [("type", `String "string")]);
      ("age", `Assoc [("type", `String "number")]);
    ];
    required = Some ["name"];
  } in
  let j = Mcp_types.elicitation_schema_to_yojson schema in
  match Mcp_types.elicitation_schema_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "type" "object" decoded.type_;
    Alcotest.(check int) "properties count" 2 (List.length decoded.properties);
    Alcotest.(check (option (list string))) "required" (Some ["name"]) decoded.required
  | Error e -> Alcotest.fail e

let test_elicitation_params_roundtrip () =
  let schema : Mcp_types.elicitation_schema = {
    type_ = "object";
    properties = [("q", `Assoc [("type", `String "string")])];
    required = None;
  } in
  let params : Mcp_types.elicitation_params = {
    message = "Please enter your name";
    requested_schema = Some schema;
    mode = None;
    url = None;
  } in
  let j = Mcp_types.elicitation_params_to_yojson params in
  (* Check requestedSchema key (camelCase) *)
  (match j with
   | `Assoc fields ->
     Alcotest.(check bool) "requestedSchema key"
       true (List.mem_assoc "requestedSchema" fields)
   | _ -> Alcotest.fail "expected object");
  match Mcp_types.elicitation_params_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "message" "Please enter your name" decoded.message;
    Alcotest.(check bool) "schema present" true (Option.is_some decoded.requested_schema)
  | Error e -> Alcotest.fail e

let test_elicitation_params_no_schema () =
  let params : Mcp_types.elicitation_params = {
    message = "Confirm?";
    requested_schema = None;
    mode = None;
    url = None;
  } in
  let j = Mcp_types.elicitation_params_to_yojson params in
  match Mcp_types.elicitation_params_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "message" "Confirm?" decoded.message;
    Alcotest.(check bool) "no schema" true (Option.is_none decoded.requested_schema)
  | Error e -> Alcotest.fail e

let test_elicitation_action_roundtrip () =
  let check_action action expected_str =
    let j = Mcp_types.elicitation_action_to_yojson action in
    Alcotest.(check json) ("action " ^ expected_str)
      (`String expected_str) j;
    Alcotest.(check (result pass string)) "parse back"
      (Ok action) (Mcp_types.elicitation_action_of_yojson j)
  in
  check_action Accept "accept";
  check_action Decline "decline";
  check_action Cancel "cancel"

let test_elicitation_result_accept () =
  let result : Mcp_types.elicitation_result = {
    action = Accept;
    content = Some [("name", `String "Vincent"); ("age", `Int 30)];
  } in
  let j = Mcp_types.elicitation_result_to_yojson result in
  match Mcp_types.elicitation_result_of_yojson j with
  | Ok decoded ->
    (match decoded.action with Accept -> () | _ -> Alcotest.fail "expected Accept");
    Alcotest.(check bool) "content present" true (Option.is_some decoded.content);
    let pairs = Option.get decoded.content in
    Alcotest.(check int) "content pairs" 2 (List.length pairs)
  | Error e -> Alcotest.fail e

let test_elicitation_result_decline () =
  let result : Mcp_types.elicitation_result = {
    action = Decline;
    content = None;
  } in
  let j = Mcp_types.elicitation_result_to_yojson result in
  match Mcp_types.elicitation_result_of_yojson j with
  | Ok decoded ->
    (match decoded.action with Decline -> () | _ -> Alcotest.fail "expected Decline");
    Alcotest.(check bool) "no content" true (Option.is_none decoded.content)
  | Error e -> Alcotest.fail e

(* --- completion_context --- *)

let test_completion_context_roundtrip () =
  let ctx : Mcp_types.completion_context = {
    arguments = Some [("lang", "ocaml"); ("version", "5.4")];
  } in
  let j = Mcp_types.completion_context_to_yojson ctx in
  match Mcp_types.completion_context_of_yojson j with
  | Ok decoded ->
    Alcotest.(check bool) "args present" true (Option.is_some decoded.arguments);
    let args = Option.get decoded.arguments in
    Alcotest.(check int) "arg count" 2 (List.length args);
    Alcotest.(check string) "first key" "lang" (fst (List.hd args))
  | Error e -> Alcotest.fail e

let test_completion_context_empty () =
  let ctx : Mcp_types.completion_context = { arguments = None } in
  let j = Mcp_types.completion_context_to_yojson ctx in
  match Mcp_types.completion_context_of_yojson j with
  | Ok decoded ->
    Alcotest.(check bool) "no args" true (Option.is_none decoded.arguments)
  | Error e -> Alcotest.fail e

(* --- unknown content type error --- *)

let test_unknown_content_type () =
  let j = `Assoc [("type", `String "video"); ("data", `String "x")] in
  match Mcp_types.tool_content_of_yojson j with
  | Error msg ->
    Alcotest.(check bool) "mentions unknown type"
      true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error for unknown content type"

(* --- Icons --- *)

let test_tool_with_icon () =
  let t : Mcp_types.tool = {
    name = "icon_tool";
    description = None;
    input_schema = `Assoc [("type", `String "object")];
    title = None;
    annotations = None;
    icon = Some "https://example.com/icon.png";
    output_schema = None;
    execution = None;
  } in
  let j = Mcp_types.tool_to_yojson t in
  match Mcp_types.tool_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option string)) "icon" (Some "https://example.com/icon.png") decoded.icon
  | Error e -> Alcotest.fail e

let test_resource_with_icon () =
  let r : Mcp_types.resource = {
    uri = "file:///x";
    name = "icon_res";
    description = None;
    mime_type = None;
    icon = Some "data:image/svg+xml,<svg/>";
  } in
  let j = Mcp_types.resource_to_yojson r in
  match Mcp_types.resource_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option string)) "icon" (Some "data:image/svg+xml,<svg/>") decoded.icon
  | Error e -> Alcotest.fail e

let test_resource_template_with_icon () =
  let t : Mcp_types.resource_template = {
    uri_template = "file:///{path}";
    name = "icon_tmpl";
    description = None;
    mime_type = None;
    icon = Some "https://example.com/template.png";
  } in
  let j = Mcp_types.resource_template_to_yojson t in
  match Mcp_types.resource_template_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option string)) "icon" (Some "https://example.com/template.png") decoded.icon
  | Error e -> Alcotest.fail e

let test_prompt_with_icon () =
  let p : Mcp_types.prompt = {
    name = "icon_prompt";
    description = None;
    arguments = None;
    icon = Some "https://example.com/prompt.svg";
  } in
  let j = Mcp_types.prompt_to_yojson p in
  match Mcp_types.prompt_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option string)) "icon" (Some "https://example.com/prompt.svg") decoded.icon
  | Error e -> Alcotest.fail e

(* --- Elicitation Mode Variant --- *)

let test_elicitation_mode_roundtrip () =
  (* Form -> "form" -> Form *)
  let j_form = Mcp_types.elicitation_mode_to_yojson Form in
  Alcotest.(check json) "Form serializes to form" (`String "form") j_form;
  (match Mcp_types.elicitation_mode_of_yojson j_form with
   | Ok Form -> ()
   | Ok Url -> Alcotest.fail "expected Form, got Url"
   | Error e -> Alcotest.fail e);
  (* Url -> "url" -> Url *)
  let j_url = Mcp_types.elicitation_mode_to_yojson Url in
  Alcotest.(check json) "Url serializes to url" (`String "url") j_url;
  (match Mcp_types.elicitation_mode_of_yojson j_url with
   | Ok Url -> ()
   | Ok Form -> Alcotest.fail "expected Url, got Form"
   | Error e -> Alcotest.fail e)

let test_elicitation_mode_invalid () =
  match Mcp_types.elicitation_mode_of_yojson (`String "unknown") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for invalid mode"

let test_elicitation_params_form_mode () =
  let schema : Mcp_types.elicitation_schema = {
    type_ = "object";
    properties = [("q", `Assoc [("type", `String "string")])];
    required = None;
  } in
  let params : Mcp_types.elicitation_params = {
    message = "Please fill the form";
    requested_schema = Some schema;
    mode = Some Form;
    url = None;
  } in
  let j = Mcp_types.elicitation_params_to_yojson params in
  (match j with
   | `Assoc fields ->
     Alcotest.(check json) "mode is form" (`String "form") (List.assoc "mode" fields)
   | _ -> Alcotest.fail "expected object");
  match Mcp_types.elicitation_params_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "message" "Please fill the form" decoded.message;
    Alcotest.(check bool) "schema present" true (Option.is_some decoded.requested_schema);
    (match decoded.mode with
     | Some Form -> ()
     | Some Url -> Alcotest.fail "expected Form mode"
     | None -> Alcotest.fail "expected Some mode")
  | Error e -> Alcotest.fail e

let test_elicitation_params_url_mode () =
  let params : Mcp_types.elicitation_params = {
    message = "Visit the URL to authenticate";
    requested_schema = None;
    mode = Some Url;
    url = Some "https://example.com/auth";
  } in
  let j = Mcp_types.elicitation_params_to_yojson params in
  (match j with
   | `Assoc fields ->
     Alcotest.(check json) "mode is url" (`String "url") (List.assoc "mode" fields);
     Alcotest.(check json) "url field" (`String "https://example.com/auth") (List.assoc "url" fields)
   | _ -> Alcotest.fail "expected object");
  match Mcp_types.elicitation_params_of_yojson j with
  | Ok decoded ->
    (match decoded.mode with
     | Some Url -> ()
     | Some Form -> Alcotest.fail "expected Url mode"
     | None -> Alcotest.fail "expected Some mode");
    Alcotest.(check (option string)) "url" (Some "https://example.com/auth") decoded.url
  | Error e -> Alcotest.fail e

(* --- tool with outputSchema roundtrip --- *)

let test_tool_output_schema_roundtrip () =
  let schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("result", `Assoc [("type", `String "string")])
    ])
  ] in
  let tool = Mcp_types.make_tool ~name:"calc" ~description:"Calculate"
    ~output_schema:schema () in
  Alcotest.(check bool) "output_schema set" true (Option.is_some tool.output_schema);
  let j = Mcp_types.tool_to_yojson tool in
  (* Verify outputSchema key in JSON *)
  (match j with
   | `Assoc fields ->
     Alcotest.(check bool) "outputSchema key in JSON" true
       (List.mem_assoc "outputSchema" fields)
   | _ -> Alcotest.fail "expected object");
  match Mcp_types.tool_of_yojson j with
  | Ok decoded ->
    Alcotest.(check bool) "has output_schema" true
      (Option.is_some decoded.output_schema);
    Alcotest.(check json) "output_schema value" schema
      (Option.get decoded.output_schema)
  | Error e -> Alcotest.fail e

(* --- tool with execution.taskSupport roundtrip --- *)

let test_tool_execution_roundtrip () =
  let check_variant variant expected_str =
    let exec : Mcp_types.tool_execution = { task_support = variant } in
    let tool = Mcp_types.make_tool ~name:"task_tool"
      ~description:"Tool with task support" ~execution:exec () in
    let j = Mcp_types.tool_to_yojson tool in
    (* Verify execution.taskSupport in JSON *)
    (match j with
     | `Assoc fields ->
       (match List.assoc_opt "execution" fields with
        | Some (`Assoc exec_fields) ->
          (match List.assoc_opt "taskSupport" exec_fields with
           | Some (`String s) ->
             Alcotest.(check string) ("taskSupport " ^ expected_str) expected_str s
           | _ -> Alcotest.fail "missing taskSupport field")
        | _ -> Alcotest.fail "missing execution field")
     | _ -> Alcotest.fail "expected object");
    match Mcp_types.tool_of_yojson j with
    | Ok decoded ->
      Alcotest.(check bool) "execution present" true
        (Option.is_some decoded.execution);
      let exec_decoded = Option.get decoded.execution in
      let got = Mcp_types.task_execution_support_to_yojson exec_decoded.task_support in
      Alcotest.(check json) ("roundtrip " ^ expected_str)
        (`String expected_str) got
    | Error e -> Alcotest.fail e
  in
  check_variant Mcp_types.Task_required "required";
  check_variant Mcp_types.Task_optional "optional";
  check_variant Mcp_types.Task_forbidden "forbidden"

(* --- server_capabilities full roundtrip --- *)

let test_capabilities_full_roundtrip () =
  let caps : Mcp_types.server_capabilities = {
    tools = Some { list_changed = Some true };
    resources = Some { subscribe = Some true; list_changed = Some false };
    prompts = Some { list_changed = Some true };
    logging = Some ();
    completions = Some ();
    experimental = Some (`Assoc [("custom", `Bool true)]);
  } in
  let j = Mcp_types.server_capabilities_to_yojson caps in
  match Mcp_types.server_capabilities_of_yojson j with
  | Ok decoded ->
    Alcotest.(check bool) "tools present" true (Option.is_some decoded.tools);
    Alcotest.(check bool) "resources present" true (Option.is_some decoded.resources);
    Alcotest.(check bool) "prompts present" true (Option.is_some decoded.prompts);
    Alcotest.(check bool) "logging present" true (Option.is_some decoded.logging);
    Alcotest.(check bool) "completions present" true (Option.is_some decoded.completions);
    Alcotest.(check bool) "experimental present" true (Option.is_some decoded.experimental);
    (* Check sub-fields *)
    let resources = Option.get decoded.resources in
    Alcotest.(check (option bool)) "subscribe" (Some true) resources.subscribe;
    Alcotest.(check (option bool)) "resources listChanged" (Some false) resources.list_changed;
    let prompts = Option.get decoded.prompts in
    Alcotest.(check (option bool)) "prompts listChanged" (Some true) prompts.list_changed
  | Error e -> Alcotest.fail e

(* --- client_capabilities full roundtrip --- *)

let test_client_capabilities_full_roundtrip () =
  let caps : Mcp_types.client_capabilities = {
    roots = Some { list_changed = Some true };
    sampling = Some ();
    elicitation = Some ();
    experimental = Some (`Assoc [("beta", `String "feature")]);
  } in
  let j = Mcp_types.client_capabilities_to_yojson caps in
  match Mcp_types.client_capabilities_of_yojson j with
  | Ok decoded ->
    Alcotest.(check bool) "roots present" true (Option.is_some decoded.roots);
    Alcotest.(check bool) "sampling present" true (Option.is_some decoded.sampling);
    Alcotest.(check bool) "elicitation present" true (Option.is_some decoded.elicitation);
    Alcotest.(check bool) "experimental present" true (Option.is_some decoded.experimental);
    let roots = Option.get decoded.roots in
    Alcotest.(check (option bool)) "roots listChanged" (Some true) roots.list_changed
  | Error e -> Alcotest.fail e

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
      Alcotest.test_case "input_schema compatibility" `Quick test_tool_input_schema_snake_case;
      Alcotest.test_case "tool_def alias" `Quick test_tool_def_alias;
      Alcotest.test_case "output_schema roundtrip" `Quick test_tool_output_schema_roundtrip;
      Alcotest.test_case "execution roundtrip" `Quick test_tool_execution_roundtrip;
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
      Alcotest.test_case "full roundtrip" `Quick test_capabilities_full_roundtrip;
    ];
    "client_capabilities", [
      Alcotest.test_case "full roundtrip" `Quick test_client_capabilities_full_roundtrip;
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
    "content_annotations", [
      Alcotest.test_case "round-trip" `Quick test_content_annotations_roundtrip;
      Alcotest.test_case "empty" `Quick test_content_annotations_empty;
    ];
    "tool_annotations", [
      Alcotest.test_case "full" `Quick test_tool_annotations_full;
      Alcotest.test_case "empty" `Quick test_tool_annotations_empty;
      Alcotest.test_case "partial" `Quick test_tool_annotations_partial;
    ];
    "tool_with_annotations", [
      Alcotest.test_case "with annotations" `Quick test_tool_with_annotations;
      Alcotest.test_case "without annotations" `Quick test_tool_without_annotations;
    ];
    "audio_content", [
      Alcotest.test_case "round-trip" `Quick test_audio_content_roundtrip;
      Alcotest.test_case "with annotations" `Quick test_audio_content_with_annotations;
    ];
    "resource_link_content", [
      Alcotest.test_case "round-trip" `Quick test_resource_link_content_roundtrip;
      Alcotest.test_case "minimal" `Quick test_resource_link_content_minimal;
    ];
    "tool_result_structured", [
      Alcotest.test_case "with structured" `Quick test_tool_result_structured_content;
      Alcotest.test_case "without structured" `Quick test_tool_result_no_structured_content;
    ];
    "text_content_annotations", [
      Alcotest.test_case "with annotations" `Quick test_text_content_with_annotations;
    ];
    "elicitation", [
      Alcotest.test_case "schema round-trip" `Quick test_elicitation_schema_roundtrip;
      Alcotest.test_case "params round-trip" `Quick test_elicitation_params_roundtrip;
      Alcotest.test_case "params no schema" `Quick test_elicitation_params_no_schema;
      Alcotest.test_case "action round-trip" `Quick test_elicitation_action_roundtrip;
      Alcotest.test_case "result accept" `Quick test_elicitation_result_accept;
      Alcotest.test_case "result decline" `Quick test_elicitation_result_decline;
    ];
    "elicitation_mode", [
      Alcotest.test_case "mode roundtrip" `Quick test_elicitation_mode_roundtrip;
      Alcotest.test_case "mode invalid" `Quick test_elicitation_mode_invalid;
      Alcotest.test_case "params form mode" `Quick test_elicitation_params_form_mode;
      Alcotest.test_case "params url mode" `Quick test_elicitation_params_url_mode;
    ];
    "icons", [
      Alcotest.test_case "tool with icon" `Quick test_tool_with_icon;
      Alcotest.test_case "resource with icon" `Quick test_resource_with_icon;
      Alcotest.test_case "resource_template with icon" `Quick test_resource_template_with_icon;
      Alcotest.test_case "prompt with icon" `Quick test_prompt_with_icon;
    ];
    "completion_context", [
      Alcotest.test_case "round-trip" `Quick test_completion_context_roundtrip;
      Alcotest.test_case "empty" `Quick test_completion_context_empty;
    ];
    "content_type_errors", [
      Alcotest.test_case "unknown type" `Quick test_unknown_content_type;
    ];
  ]
