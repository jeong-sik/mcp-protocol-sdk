(** Tests for ppx_deriving_jsonschema integration.

    Verifies that [@@deriving jsonschema] generates correct JSON Schema
    for various OCaml types used in MCP tool definitions. *)

open Mcp_protocol

(* ── test types ─────────────────────────────── *)

type simple_record = {
  name: string;
  count: int;
} [@@deriving yojson, jsonschema]

type with_option = {
  required_field: string;
  optional_field: string option;
} [@@deriving yojson, jsonschema]

type nested_inner = {
  value: int;
} [@@deriving yojson, jsonschema]

type nested_outer = {
  label: string;
  inner: nested_inner;
} [@@deriving yojson, jsonschema]

type color = Red | Green | Blue
[@@deriving yojson, jsonschema]

type with_variant = {
  name: string;
  color: color;
} [@@deriving yojson, jsonschema]

type with_list = {
  tags: string list;
} [@@deriving yojson, jsonschema]

type with_float = {
  score: float;
  enabled: bool;
} [@@deriving yojson, jsonschema]

(* ── helpers ────────────────────────────────── *)

(** Pipeline-friendly bind: [opt >>= f] = [Option.bind opt f] *)
let ( >>= ) opt f = Option.bind opt f

let get_field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let get_string = function
  | `String s -> Some s
  | _ -> None

let get_list = function
  | `List l -> Some l
  | _ -> None

let get_assoc = function
  | `Assoc l -> Some l
  | _ -> None

(* ── tests ──────────────────────────────────── *)

let test_simple_record () =
  let schema = simple_record_jsonschema in
  (* type = "object" *)
  Alcotest.(check (option string)) "type is object"
    (Some "object")
    (get_field "type" schema >>= get_string);
  (* properties has name and count *)
  let props = get_field "properties" schema in
  Alcotest.(check bool) "has properties" true (Option.is_some props);
  let props_assoc = props >>= get_assoc in
  Alcotest.(check bool) "name in properties" true
    (Option.is_some (props_assoc >>= fun p -> List.assoc_opt "name" p));
  Alcotest.(check bool) "count in properties" true
    (Option.is_some (props_assoc >>= fun p -> List.assoc_opt "count" p));
  (* both are required *)
  let required = get_field "required" schema >>= get_list in
  let required_strs =
    Option.value ~default:[] required
    |> List.filter_map get_string
    |> List.sort String.compare
  in
  Alcotest.(check (list string)) "required fields"
    ["count"; "name"] required_strs

let test_option_field () =
  let schema = with_option_jsonschema in
  let required = get_field "required" schema >>= get_list in
  let required_strs =
    Option.value ~default:[] required
    |> List.filter_map get_string
  in
  (* required_field should be required, optional_field should not *)
  Alcotest.(check bool) "required_field is required" true
    (List.mem "required_field" required_strs);
  Alcotest.(check bool) "optional_field is not required" false
    (List.mem "optional_field" required_strs)

let test_nested_record () =
  let schema = nested_outer_jsonschema in
  let props = get_field "properties" schema >>= get_assoc in
  (* inner field should exist and be an object *)
  let inner_schema = props >>= fun p -> List.assoc_opt "inner" p in
  Alcotest.(check bool) "inner field exists" true (Option.is_some inner_schema);
  let inner_type = inner_schema >>= get_field "type" >>= get_string in
  Alcotest.(check (option string)) "inner is object"
    (Some "object") inner_type

let test_variant_type () =
  (* color_jsonschema should represent the enum *)
  let schema = color_jsonschema in
  Alcotest.(check bool) "variant schema is not null" true
    (schema <> `Null)

let test_list_field () =
  let schema = with_list_jsonschema in
  let props = get_field "properties" schema >>= get_assoc in
  let tags_type =
    (props >>= fun p -> List.assoc_opt "tags" p)
    >>= get_field "type" >>= get_string
  in
  Alcotest.(check (option string)) "tags is array"
    (Some "array") tags_type

let test_float_and_bool () =
  let schema = with_float_jsonschema in
  let props = get_field "properties" schema >>= get_assoc in
  let score_type =
    (props >>= fun p -> List.assoc_opt "score" p)
    >>= get_field "type" >>= get_string
  in
  let enabled_type =
    (props >>= fun p -> List.assoc_opt "enabled" p)
    >>= get_field "type" >>= get_string
  in
  Alcotest.(check (option string)) "score is number"
    (Some "number") score_type;
  Alcotest.(check (option string)) "enabled is boolean"
    (Some "boolean") enabled_type

let test_make_tool_integration () =
  (* Verify ppx schema works with make_tool *)
  let tool = Mcp_types.make_tool
    ~name:"test-tool"
    ~description:"A test tool"
    ~input_schema:simple_record_jsonschema
    ()
  in
  Alcotest.(check string) "tool name" "test-tool" tool.name;
  Alcotest.(check (option string)) "tool description"
    (Some "A test tool") tool.description;
  let schema_type = get_field "type" tool.input_schema >>= get_string in
  Alcotest.(check (option string)) "input_schema type is object"
    (Some "object") schema_type

(* ── suite ──────────────────────────────────── *)

let () =
  Alcotest.run "ppx_jsonschema" [
    "record", [
      Alcotest.test_case "simple record" `Quick test_simple_record;
      Alcotest.test_case "option field" `Quick test_option_field;
      Alcotest.test_case "nested record" `Quick test_nested_record;
      Alcotest.test_case "list field" `Quick test_list_field;
      Alcotest.test_case "float and bool" `Quick test_float_and_bool;
    ];
    "variant", [
      Alcotest.test_case "variant type" `Quick test_variant_type;
    ];
    "integration", [
      Alcotest.test_case "make_tool with ppx schema" `Quick test_make_tool_integration;
    ];
  ]
