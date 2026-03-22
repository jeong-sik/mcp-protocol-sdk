(** Unit tests for Handler module.

    Tests registration, accessors, capabilities, and dispatch logic
    without requiring a real transport. *)

open Mcp_protocol

module Handler = Mcp_protocol_eio.Handler

let json = Alcotest.testable
  (fun fmt j -> Format.pp_print_string fmt (Yojson.Safe.to_string j))
  (fun a b -> Yojson.Safe.equal a b)

(* ── dummy context ────────────────────────────── *)

let dummy_ctx : Handler.context = {
  send_notification = (fun ~method_:_ ~params:_ -> Ok ());
  send_log = (fun _ _ -> Ok ());
  send_progress = (fun ~token:_ ~progress:_ ~total:_ -> Ok ());
  request_sampling = (fun _ -> Error "not supported");
  request_roots_list = (fun () -> Error "not supported");
  request_elicitation = (fun _ -> Error "not supported");
}

(* ── create + accessors ──────────────────────── *)

let test_create_basic () =
  let h = Handler.create ~name:"test" ~version:"1.0" () in
  Alcotest.(check string) "name" "test" (Handler.name h);
  Alcotest.(check string) "version" "1.0" (Handler.version h);
  Alcotest.(check (option string)) "no instructions" None (Handler.instructions h)

let test_create_with_instructions () =
  let h = Handler.create ~name:"s" ~version:"1" ~instructions:"help text" () in
  Alcotest.(check (option string)) "instructions"
    (Some "help text") (Handler.instructions h)

let test_empty_collections () =
  let h = Handler.create ~name:"s" ~version:"1" () in
  Alcotest.(check int) "no tools" 0 (List.length (Handler.tools h));
  Alcotest.(check int) "no resources" 0 (List.length (Handler.resources h));
  Alcotest.(check int) "no prompts" 0 (List.length (Handler.prompts h));
  Alcotest.(check (list string)) "no subscribed" [] (Handler.subscribed_uris h)

(* ── tool registration ───────────────────────── *)

let echo_handler _ctx _name _args = Ok (Mcp_types.tool_result_of_text "echo")

let test_add_tool () =
  let tool = Mcp_types.make_tool ~name:"echo" ~description:"Echo tool" () in
  let h = Handler.create ~name:"s" ~version:"1" ()
    |> Handler.add_tool tool echo_handler in
  Alcotest.(check int) "one tool" 1 (List.length (Handler.tools h));
  let t = List.hd (Handler.tools h) in
  Alcotest.(check string) "tool name" "echo" t.name

let test_add_multiple_tools () =
  let t1 = Mcp_types.make_tool ~name:"a" () in
  let t2 = Mcp_types.make_tool ~name:"b" () in
  let h = Handler.create ~name:"s" ~version:"1" ()
    |> Handler.add_tool t1 echo_handler
    |> Handler.add_tool t2 echo_handler in
  Alcotest.(check int) "two tools" 2 (List.length (Handler.tools h))

let test_duplicate_tool_replaces () =
  let t1 = Mcp_types.make_tool ~name:"dup" ~description:"first" () in
  let t2 = Mcp_types.make_tool ~name:"dup" ~description:"second" () in
  let h = Handler.create ~name:"s" ~version:"1" ()
    |> Handler.add_tool t1 echo_handler
    |> Handler.add_tool t2 echo_handler in
  Alcotest.(check int) "still one" 1 (List.length (Handler.tools h));
  Alcotest.(check (option string)) "second wins"
    (Some "second") (List.hd (Handler.tools h)).description

(* ── resource registration ───────────────────── *)

let test_add_resource () =
  let res = Mcp_types.make_resource ~uri:"file:///test" ~name:"test" () in
  let handler _ctx _uri = Ok [{ Mcp_types.uri = "file:///test"; mime_type = None; text = Some "data"; blob = None }] in
  let h = Handler.create ~name:"s" ~version:"1" ()
    |> Handler.add_resource res handler in
  Alcotest.(check int) "one resource" 1 (List.length (Handler.resources h))

(* ── prompt registration ─────────────────────── *)

let test_add_prompt () =
  let prompt = Mcp_types.make_prompt ~name:"greet" () in
  let handler _ctx _name _args = Ok Mcp_types.{ description = None; messages = [] } in
  let h = Handler.create ~name:"s" ~version:"1" ()
    |> Handler.add_prompt prompt handler in
  Alcotest.(check int) "one prompt" 1 (List.length (Handler.prompts h))

(* ── capabilities ────────────────────────────── *)

let test_capabilities_empty () =
  let h = Handler.create ~name:"s" ~version:"1" () in
  let caps = Handler.server_capabilities h in
  Alcotest.(check bool) "no tools cap" true (Option.is_none caps.tools);
  Alcotest.(check bool) "no resources cap" true (Option.is_none caps.resources);
  Alcotest.(check bool) "no prompts cap" true (Option.is_none caps.prompts);
  (* logging is always present *)
  Alcotest.(check bool) "has logging" true (Option.is_some caps.logging)

let test_capabilities_with_tools () =
  let tool = Mcp_types.make_tool ~name:"t" () in
  let h = Handler.create ~name:"s" ~version:"1" ()
    |> Handler.add_tool tool echo_handler in
  let caps = Handler.server_capabilities h in
  Alcotest.(check bool) "tools cap present" true (Option.is_some caps.tools)

(* ── dispatch: initialize ────────────────────── *)

let dispatch_request h method_ ?params () =
  let req = Jsonrpc.make_request ~id:(Jsonrpc.Int 1) ~method_ ?params () in
  let log_ref = ref Logging.Warning in
  Handler.dispatch h dummy_ctx log_ref req

let test_dispatch_initialize () =
  let h = Handler.create ~name:"test-server" ~version:"2.0" () in
  let params = `Assoc [
    ("protocolVersion", `String "2025-11-25");
    ("capabilities", `Assoc []);
    ("clientInfo", `Assoc [("name", `String "test"); ("version", `String "1.0")]);
  ] in
  match dispatch_request h "initialize" ~params () with
  | Some (Response r) ->
    (match r.result with
     | `Assoc fields ->
       (match List.assoc_opt "serverInfo" fields with
        | Some (`Assoc info) ->
          Alcotest.(check (option (testable
            (fun fmt j -> Format.pp_print_string fmt (Yojson.Safe.to_string j))
            (fun a b -> a = b))))
            "server name" (Some (`String "test-server"))
            (List.assoc_opt "name" info)
        | _ -> Alcotest.fail "missing serverInfo")
     | _ -> Alcotest.fail "expected assoc result")
  | _ -> Alcotest.fail "expected Response"

(* ── dispatch: ping ──────────────────────────── *)

let test_dispatch_ping () =
  let h = Handler.create ~name:"s" ~version:"1" () in
  match dispatch_request h "ping" () with
  | Some (Response r) ->
    Alcotest.(check json) "empty result" (`Assoc []) r.result
  | _ -> Alcotest.fail "expected Response"

(* ── dispatch: tools/list ────────────────────── *)

let test_dispatch_tools_list () =
  let tool = Mcp_types.make_tool ~name:"echo" ~description:"desc" () in
  let h = Handler.create ~name:"s" ~version:"1" ()
    |> Handler.add_tool tool echo_handler in
  match dispatch_request h "tools/list" () with
  | Some (Response r) ->
    (match r.result with
     | `Assoc fields ->
       (match List.assoc_opt "tools" fields with
        | Some (`List tools) ->
          Alcotest.(check int) "one tool" 1 (List.length tools)
        | _ -> Alcotest.fail "missing tools array")
     | _ -> Alcotest.fail "expected assoc")
  | _ -> Alcotest.fail "expected Response"

(* ── dispatch: tools/call ────────────────────── *)

let test_dispatch_tools_call () =
  let tool = Mcp_types.make_tool ~name:"echo" () in
  let handler _ctx _name args =
    let text = match args with
      | Some (`Assoc fields) ->
        (match List.assoc_opt "input" fields with
         | Some (`String s) -> s | _ -> "no input")
      | _ -> "no args"
    in
    Ok (Mcp_types.tool_result_of_text text)
  in
  let h = Handler.create ~name:"s" ~version:"1" ()
    |> Handler.add_tool tool handler in
  let params = `Assoc [
    ("name", `String "echo");
    ("arguments", `Assoc [("input", `String "hello")]);
  ] in
  match dispatch_request h "tools/call" ~params () with
  | Some (Response r) ->
    (match r.result with
     | `Assoc fields ->
       (match List.assoc_opt "content" fields with
        | Some (`List items) ->
          Alcotest.(check int) "one content" 1 (List.length items)
        | _ -> Alcotest.fail "missing content")
     | _ -> Alcotest.fail "expected assoc")
  | _ -> Alcotest.fail "expected Response"

let test_dispatch_tools_call_unknown () =
  let h = Handler.create ~name:"s" ~version:"1" () in
  let params = `Assoc [("name", `String "nonexistent")] in
  match dispatch_request h "tools/call" ~params () with
  | Some (Error _) -> ()
  | _ -> Alcotest.fail "expected Error for unknown tool"

(* ── dispatch: unknown method ────────────────── *)

let test_dispatch_unknown_method () =
  let h = Handler.create ~name:"s" ~version:"1" () in
  match dispatch_request h "totally/unknown" () with
  | Some (Error e) ->
    Alcotest.(check int) "method not found code"
      Error_codes.method_not_found e.error.code
  | _ -> Alcotest.fail "expected Error"

(* ── dispatch: notification returns None ─────── *)

let test_dispatch_notification_returns_none () =
  let h = Handler.create ~name:"s" ~version:"1" () in
  let notif = Jsonrpc.make_notification ~method_:"notifications/initialized" () in
  let log_ref = ref Logging.Warning in
  match Handler.dispatch h dummy_ctx log_ref notif with
  | None -> ()
  | Some _ -> Alcotest.fail "notification should return None"

(* ── parse_list_field ────────────────────────── *)

let test_parse_list_field_success () =
  let json = `Assoc [("items", `List [`String "a"; `String "b"])] in
  let parser = function `String s -> Ok s | _ -> Error "not string" in
  match Handler.parse_list_field "items" parser json with
  | Ok items ->
    Alcotest.(check (list string)) "parsed" ["a"; "b"] items
  | Error e -> Alcotest.fail e

let test_parse_list_field_missing () =
  let json = `Assoc [("other", `Int 1)] in
  let parser = function _ -> Ok () in
  Alcotest.(check bool) "missing field"
    true (Result.is_error (Handler.parse_list_field "items" parser json))

let test_parse_list_field_drops_failures () =
  let json = `Assoc [("items", `List [`String "ok"; `Int 42; `String "also"])] in
  let parser = function `String s -> Ok s | _ -> Error "not string" in
  match Handler.parse_list_field "items" parser json with
  | Ok items ->
    (* Int 42 silently dropped *)
    Alcotest.(check (list string)) "filtered" ["ok"; "also"] items
  | Error e -> Alcotest.fail e

(* ── build_initialize_params ─────────────────── *)

let test_build_init_params () =
  let params = Handler.build_initialize_params
    ~has_sampling:true ~has_roots:false ~has_elicitation:true
    ~client_name:"test" ~client_version:"1.0" in
  match params with
  | `Assoc fields ->
    (match List.assoc_opt "capabilities" fields with
     | Some (`Assoc caps) ->
       Alcotest.(check bool) "has sampling" true (List.assoc_opt "sampling" caps <> None);
       Alcotest.(check bool) "no roots" true (List.assoc_opt "roots" caps = None);
       Alcotest.(check bool) "has elicitation" true (List.assoc_opt "elicitation" caps <> None)
     | _ -> Alcotest.fail "missing capabilities")
  | _ -> Alcotest.fail "expected assoc"

(* ── Suite ────────────────────────────────────── *)

(* ── resource templates ──────────────────────── *)

let test_resource_template_register () =
  let h = Handler.create ~name:"s" ~version:"1" ()
    |> Handler.resource_template ~uri_template:"file:///{path}" "files"
         ~description:"File access" ~mime_type:"text/plain"
         (fun _ctx _uri -> Ok [Mcp_types.{
           uri = "file:///test.txt"; mime_type = Some "text/plain";
           text = Some "content"; blob = None;
         }])
  in
  Alcotest.(check int) "one template" 1 (List.length (Handler.resource_templates h))

let test_resource_templates_list_dispatch () =
  let h = Handler.create ~name:"s" ~version:"1" ()
    |> Handler.resource_template ~uri_template:"db:///{table}" "database"
         (fun _ctx _uri -> Ok [])
  in
  match dispatch_request h "resources/templates/list" () with
  | Some (Jsonrpc.Response resp) ->
    (match resp.result with
     | `Assoc fields ->
       (match List.assoc_opt "resourceTemplates" fields with
        | Some (`List templates) ->
          Alcotest.(check int) "one template" 1 (List.length templates)
        | _ -> Alcotest.fail "missing resourceTemplates")
     | _ -> Alcotest.fail "expected object")
  | _ -> Alcotest.fail "expected response"

let test_capabilities_with_templates () =
  let h = Handler.create ~name:"s" ~version:"1" ()
    |> Handler.resource_template ~uri_template:"x:///{id}" "x"
         (fun _ctx _uri -> Ok [])
  in
  let caps = Handler.server_capabilities h in
  Alcotest.(check bool) "resources cap from template" true (Option.is_some caps.resources)

let () =
  Alcotest.run "Handler" [
    "create", [
      Alcotest.test_case "basic" `Quick test_create_basic;
      Alcotest.test_case "with instructions" `Quick test_create_with_instructions;
      Alcotest.test_case "empty collections" `Quick test_empty_collections;
    ];
    "registration", [
      Alcotest.test_case "add tool" `Quick test_add_tool;
      Alcotest.test_case "multiple tools" `Quick test_add_multiple_tools;
      Alcotest.test_case "duplicate replaces" `Quick test_duplicate_tool_replaces;
      Alcotest.test_case "add resource" `Quick test_add_resource;
      Alcotest.test_case "add prompt" `Quick test_add_prompt;
    ];
    "capabilities", [
      Alcotest.test_case "empty" `Quick test_capabilities_empty;
      Alcotest.test_case "with tools" `Quick test_capabilities_with_tools;
    ];
    "dispatch", [
      Alcotest.test_case "initialize" `Quick test_dispatch_initialize;
      Alcotest.test_case "ping" `Quick test_dispatch_ping;
      Alcotest.test_case "tools/list" `Quick test_dispatch_tools_list;
      Alcotest.test_case "tools/call" `Quick test_dispatch_tools_call;
      Alcotest.test_case "tools/call unknown" `Quick test_dispatch_tools_call_unknown;
      Alcotest.test_case "unknown method" `Quick test_dispatch_unknown_method;
      Alcotest.test_case "notification returns None" `Quick test_dispatch_notification_returns_none;
    ];
    "resource_templates", [
      Alcotest.test_case "register and list" `Quick test_resource_template_register;
      Alcotest.test_case "templates/list dispatch" `Quick test_resource_templates_list_dispatch;
      Alcotest.test_case "capabilities with templates" `Quick test_capabilities_with_templates;
    ];
    "helpers", [
      Alcotest.test_case "parse_list_field" `Quick test_parse_list_field_success;
      Alcotest.test_case "parse_list_field missing" `Quick test_parse_list_field_missing;
      Alcotest.test_case "parse_list_field drops" `Quick test_parse_list_field_drops_failures;
      Alcotest.test_case "build_init_params" `Quick test_build_init_params;
    ];
  ]
