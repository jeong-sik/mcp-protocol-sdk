(** Tests for Tool_arg — type-safe argument extraction. *)

open Mcp_protocol

(* ── extractors ────────────────────────────── *)

let test_string_extractor () =
  Alcotest.(check (result string string)) "string ok"
    (Ok "hello") (Tool_arg.string (`String "hello"));
  Alcotest.(check bool) "string err"
    true (Result.is_error (Tool_arg.string (`Int 42)))

let test_int_extractor () =
  Alcotest.(check (result int string)) "int ok"
    (Ok 42) (Tool_arg.int (`Int 42));
  Alcotest.(check (result int string)) "float to int"
    (Ok 3) (Tool_arg.int (`Float 3.0));
  Alcotest.(check bool) "int err"
    true (Result.is_error (Tool_arg.int (`String "x")))

let test_float_extractor () =
  Alcotest.(check (result (float 0.001) string)) "float ok"
    (Ok 3.14) (Tool_arg.float (`Float 3.14));
  Alcotest.(check (result (float 0.001) string)) "int to float"
    (Ok 5.0) (Tool_arg.float (`Int 5))

let test_bool_extractor () =
  Alcotest.(check (result bool string)) "bool ok"
    (Ok true) (Tool_arg.bool (`Bool true))

let test_list_of () =
  let json = `List [`String "a"; `String "b"; `String "c"] in
  Alcotest.(check (result (list string) string)) "list_of string"
    (Ok ["a"; "b"; "c"]) (Tool_arg.list_of Tool_arg.string json);
  let bad = `List [`String "a"; `Int 1] in
  Alcotest.(check bool) "list_of mixed err"
    true (Result.is_error (Tool_arg.list_of Tool_arg.string bad))

(* ── field access ──────────────────────────── *)

let args = Some (`Assoc [
  ("name", `String "test");
  ("count", `Int 5);
  ("verbose", `Bool true);
  ("tags", `List [`String "a"; `String "b"]);
])

let test_required () =
  Alcotest.(check (result string string)) "required string"
    (Ok "test") (Tool_arg.required args "name" Tool_arg.string);
  Alcotest.(check (result int string)) "required int"
    (Ok 5) (Tool_arg.required args "count" Tool_arg.int);
  Alcotest.(check bool) "required missing"
    true (Result.is_error (Tool_arg.required args "missing" Tool_arg.string));
  Alcotest.(check bool) "required no args"
    true (Result.is_error (Tool_arg.required None "x" Tool_arg.string))

let test_optional () =
  Alcotest.(check int) "optional present"
    5 (Tool_arg.optional args "count" Tool_arg.int ~default:0);
  Alcotest.(check int) "optional missing"
    99 (Tool_arg.optional args "missing" Tool_arg.int ~default:99);
  Alcotest.(check int) "optional no args"
    0 (Tool_arg.optional None "x" Tool_arg.int ~default:0)

let test_optional_opt () =
  Alcotest.(check (option string)) "opt present"
    (Some "test") (Tool_arg.optional_opt args "name" Tool_arg.string);
  Alcotest.(check (option string)) "opt missing"
    None (Tool_arg.optional_opt args "missing" Tool_arg.string)

(* ── monadic chaining ──────────────────────── *)

let test_let_bind () =
  let handler args =
    let open Tool_arg in
    let* name = required args "name" string in
    let* count = required args "count" int in
    Ok (Printf.sprintf "%s:%d" name count)
  in
  Alcotest.(check (result string string)) "let* chain ok"
    (Ok "test:5") (handler args);
  Alcotest.(check bool) "let* chain fail on missing"
    true (Result.is_error (handler (Some (`Assoc [("name", `String "x")]))))

(* ── ergonomic server API ──────────────────── *)

let test_ergonomic_tool () =
  let module GS = Mcp_protocol_eio.Generic_server.Make(Mcp_protocol_eio.Memory_transport) in
  let server =
    GS.create ~name:"ergo" ~version:"1" ()
    |> GS.tool "add" ~description:"Add two numbers" (fun _ctx _name args ->
      let open Tool_arg in
      let* x = required args "x" int in
      let* y = required args "y" int in
      Ok (Mcp_types.tool_result_of_text (string_of_int (x + y))))
    |> GS.tool "greet" (fun _ctx _name args ->
      let open Tool_arg in
      let* name = required args "name" string in
      let greeting = optional args "greeting" string ~default:"Hello" in
      Ok (Mcp_types.tool_result_of_text (greeting ^ ", " ^ name)))
  in
  ignore server

(* ── E2E with Tool_arg ─────────────────────── *)

let test_tool_arg_e2e () =
  Eio_main.run @@ fun env ->
  let module Mt = Mcp_protocol_eio.Memory_transport in
  let module GS = Mcp_protocol_eio.Generic_server.Make(Mt) in
  let module GC = Mcp_protocol_eio.Generic_client.Make(Mt) in
  Eio.Switch.run @@ fun sw ->
  let client_t, server_t = Mt.create_pair () in
  let server =
    GS.create ~name:"calc" ~version:"1" ()
    |> GS.tool "add" ~description:"Add numbers" (fun _ctx _name args ->
      let open Tool_arg in
      let* x = required args "x" int in
      let* y = required args "y" int in
      Ok (Mcp_types.tool_result_of_text (string_of_int (x + y))))
  in
  let clock = Eio.Stdenv.clock env in
  Eio.Fiber.fork ~sw (fun () -> GS.run server ~transport:server_t ~clock ());
  let client = GC.create ~transport:client_t ~clock () in

  (match GC.initialize client ~client_name:"t" ~client_version:"1" with
   | Ok _ -> () | Error e -> Alcotest.fail e);

  (match GC.call_tool client ~name:"add"
           ~arguments:(`Assoc [("x", `Int 17); ("y", `Int 25)]) () with
   | Ok result ->
     (match result.content with
      | [Mcp_types.TextContent { text; _ }] ->
        Alcotest.(check string) "17+25" "42" text
      | _ -> Alcotest.fail "expected TextContent")
   | Error e -> Alcotest.fail ("add failed: " ^ e));

  Mt.close client_t

(* ── test suite ────────────────────────────── *)

let () =
  Alcotest.run "Tool_arg" [
    "extractors", [
      Alcotest.test_case "string" `Quick test_string_extractor;
      Alcotest.test_case "int" `Quick test_int_extractor;
      Alcotest.test_case "float" `Quick test_float_extractor;
      Alcotest.test_case "bool" `Quick test_bool_extractor;
      Alcotest.test_case "list_of" `Quick test_list_of;
    ];
    "fields", [
      Alcotest.test_case "required" `Quick test_required;
      Alcotest.test_case "optional" `Quick test_optional;
      Alcotest.test_case "optional_opt" `Quick test_optional_opt;
    ];
    "monadic", [
      Alcotest.test_case "let* binding" `Quick test_let_bind;
    ];
    "ergonomic", [
      Alcotest.test_case "tool convenience" `Quick test_ergonomic_tool;
      Alcotest.test_case "E2E with Tool_arg" `Quick test_tool_arg_e2e;
    ];
  ]
