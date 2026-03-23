(** End-to-end tests: Generic_client ↔ Generic_server over Memory_transport.

    Proves the complete MCP lifecycle works with zero IO —
    both client and server use the functor API over in-memory transport. *)

open Mcp_protocol
module Mt = Mcp_protocol_eio.Memory_transport
module Server = Mcp_protocol_eio.Generic_server.Make(Mt)
module Client = Mcp_protocol_eio.Generic_client.Make(Mt)

let () = Eio_main.run @@ fun env ->
let clock = Eio.Stdenv.clock env in

(* ── helper: create connected client+server pair ── *)

let make_pair ~sw ?instructions tools =
  let client_t, server_t = Mt.create_pair () in
  let server =
    let s = Server.create ~name:"e2e-server" ~version:"1.0.0" ?instructions () in
    List.fold_left (fun s (tool, handler) -> Server.add_tool tool handler s) s tools
  in
  Eio.Fiber.fork ~sw (fun () ->
    Server.run server ~transport:server_t ~clock ());
  let client = Client.create ~transport:client_t ~clock () in
  (client, client_t)
in

(* ── test: full lifecycle ─────────────────────── *)

let test_full_lifecycle () =
  Eio.Switch.run @@ fun sw ->
  let echo_tool = Mcp_types.make_tool
    ~name:"echo" ~description:"Echo input" () in
  let upper_tool = Mcp_types.make_tool
    ~name:"upper" ~description:"Uppercase" () in
  let tools = [
    (echo_tool, (fun _ctx _name args ->
      let text = match args with
        | Some (`Assoc f) ->
          (match List.assoc_opt "text" f with
           | Some (`String s) -> s | _ -> "?")
        | _ -> "?"
      in
      Ok (Mcp_types.tool_result_of_text ("echo: " ^ text))));
    (upper_tool, (fun _ctx _name args ->
      let text = match args with
        | Some (`Assoc f) ->
          (match List.assoc_opt "text" f with
           | Some (`String s) -> s | _ -> "?")
        | _ -> "?"
      in
      Ok (Mcp_types.tool_result_of_text (String.uppercase_ascii text))));
  ] in
  let client, client_t = make_pair ~sw ~instructions:"E2E test server" tools in

  (* 1. Initialize *)
  (match Client.initialize client ~client_name:"e2e-client" ~client_version:"1.0" with
   | Ok result ->
     Alcotest.(check string) "server name" "e2e-server" result.server_info.name;
     Alcotest.(check string) "server version" "1.0.0" result.server_info.version;
     Alcotest.(check (option string)) "instructions" (Some "E2E test server") result.instructions;
     Alcotest.(check bool) "has tools cap" true (Option.is_some result.capabilities.tools)
   | Error e -> Alcotest.fail ("init failed: " ^ e));

  (* 2. Ping *)
  (match Client.ping client with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("ping failed: " ^ e));

  (* 3. List tools *)
  (match Client.list_tools client with
   | Ok tools ->
     Alcotest.(check int) "two tools" 2 (List.length tools);
     let names = List.map (fun (t : Mcp_types.tool) -> t.name) tools in
     Alcotest.(check bool) "has echo" true (List.mem "echo" names);
     Alcotest.(check bool) "has upper" true (List.mem "upper" names)
   | Error e -> Alcotest.fail ("list_tools failed: " ^ e));

  (* 4. Call echo tool *)
  (match Client.call_tool client ~name:"echo"
           ~arguments:(`Assoc [("text", `String "hello")]) () with
   | Ok result ->
     (match result.content with
      | [Mcp_types.TextContent { text; _ }] ->
        Alcotest.(check string) "echo result" "echo: hello" text
      | _ -> Alcotest.fail "expected single TextContent")
   | Error e -> Alcotest.fail ("call echo failed: " ^ e));

  (* 5. Call upper tool *)
  (match Client.call_tool client ~name:"upper"
           ~arguments:(`Assoc [("text", `String "ocaml")]) () with
   | Ok result ->
     (match result.content with
      | [Mcp_types.TextContent { text; _ }] ->
        Alcotest.(check string) "upper result" "OCAML" text
      | _ -> Alcotest.fail "expected single TextContent")
   | Error e -> Alcotest.fail ("call upper failed: " ^ e));

  (* 6. Call unknown tool *)
  (match Client.call_tool client ~name:"nonexistent" () with
   | Ok _ -> Alcotest.fail "expected error for unknown tool"
   | Error _ -> ());

  Mt.close client_t
in

(* ── test: prompts ────────────────────────────── *)

let test_prompts () =
  Eio.Switch.run @@ fun sw ->
  let client_t, server_t = Mt.create_pair () in
  let greet_prompt = Mcp_types.make_prompt
    ~name:"greet" ~description:"Greeting prompt"
    ~arguments:[{ name = "name"; description = Some "Who to greet"; required = Some true }]
    () in
  let server =
    Server.create ~name:"prompt-server" ~version:"1.0" ()
    |> Server.add_prompt greet_prompt (fun _ctx _name args ->
      let name = match List.assoc_opt "name" args with
        | Some n -> n | None -> "world"
      in
      Ok Mcp_types.{
        description = Some "A greeting";
        messages = [{
          role = User;
          content = PromptText { type_ = "text"; text = "Hello, " ^ name };
        }];
      })
  in
  Eio.Fiber.fork ~sw (fun () ->
    Server.run server ~transport:server_t ~clock ());
  let client = Client.create ~transport:client_t ~clock () in

  (* Initialize *)
  (match Client.initialize client ~client_name:"t" ~client_version:"1" with
   | Ok _ -> () | Error e -> Alcotest.fail e);

  (* List prompts *)
  (match Client.list_prompts client with
   | Ok prompts ->
     Alcotest.(check int) "one prompt" 1 (List.length prompts);
     Alcotest.(check string) "prompt name" "greet" (List.hd prompts).name
   | Error e -> Alcotest.fail ("list_prompts: " ^ e));

  (* Get prompt *)
  (match Client.get_prompt client ~name:"greet"
           ~arguments:[("name", "OCaml")] () with
   | Ok result ->
     (match result.messages with
      | [msg] ->
        (match msg.content with
         | PromptText { text; _ } ->
           Alcotest.(check string) "greeting" "Hello, OCaml" text
         | _ -> Alcotest.fail "expected PromptText")
      | _ -> Alcotest.fail "expected one message")
   | Error e -> Alcotest.fail ("get_prompt: " ^ e));

  Mt.close client_t
in

(* ── test: resources ──────────────────────────── *)

let test_resources () =
  Eio.Switch.run @@ fun sw ->
  let client_t, server_t = Mt.create_pair () in
  let res = Mcp_types.make_resource
    ~uri:"file:///readme.md" ~name:"README"
    ~mime_type:"text/markdown" () in
  let server =
    Server.create ~name:"res-server" ~version:"1.0" ()
    |> Server.add_resource res (fun _ctx _uri ->
      Ok [Mcp_types.{
        uri = "file:///readme.md";
        mime_type = Some "text/markdown";
        text = Some "# Hello";
        blob = None;
      }])
  in
  Eio.Fiber.fork ~sw (fun () ->
    Server.run server ~transport:server_t ~clock ());
  let client = Client.create ~transport:client_t ~clock () in

  (match Client.initialize client ~client_name:"t" ~client_version:"1" with
   | Ok _ -> () | Error e -> Alcotest.fail e);

  (* List resources *)
  (match Client.list_resources client with
   | Ok resources ->
     Alcotest.(check int) "one resource" 1 (List.length resources);
     Alcotest.(check string) "resource name" "README" (List.hd resources).name
   | Error e -> Alcotest.fail ("list_resources: " ^ e));

  (* Read resource *)
  (match Client.read_resource client ~uri:"file:///readme.md" with
   | Ok contents ->
     Alcotest.(check int) "one content" 1 (List.length contents);
     Alcotest.(check (option string)) "text" (Some "# Hello") (List.hd contents).text
   | Error e -> Alcotest.fail ("read_resource: " ^ e));

  Mt.close client_t
in

(* ── test: resource templates ─────────────────── *)

let test_resource_templates () =
  Eio.Switch.run @@ fun sw ->
  let client_t, server_t = Mt.create_pair () in
  let server =
    Server.create ~name:"tmpl-server" ~version:"1.0" ()
    |> Server.resource_template ~uri_template:"db:///{table}" "database"
         ~description:"Database table access"
         (fun _ctx uri ->
           let table = match String.split_on_char '/' uri with
             | _ :: _ :: _ :: t :: _ -> t
             | _ -> "unknown"
           in
           Ok [Mcp_types.{
             uri;
             mime_type = Some "application/json";
             text = Some (Printf.sprintf {|{"table":"%s","rows":42}|} table);
             blob = None;
           }])
  in
  Eio.Fiber.fork ~sw (fun () ->
    Server.run server ~transport:server_t ~clock ());
  let client = Client.create ~transport:client_t ~clock () in

  (match Client.initialize client ~client_name:"t" ~client_version:"1" with
   | Ok result ->
     (* Resources capability should be present due to templates *)
     Alcotest.(check bool) "has resources cap" true
       (Option.is_some result.capabilities.resources)
   | Error e -> Alcotest.fail e);

  (* templates/list via typed API *)
  (match Client.list_resource_templates client with
   | Ok templates ->
     Alcotest.(check int) "one template" 1 (List.length templates);
     Alcotest.(check string) "template name" "database" (List.hd templates).name
   | Error e -> Alcotest.fail ("list_resource_templates: " ^ e));

  (* also verify via low-level request *)
  (match Client.send_request client ~method_:"resources/templates/list" () with
   | Ok result ->
     (match result with
      | `Assoc fields ->
        (match List.assoc_opt "resourceTemplates" fields with
         | Some (`List templates) ->
           Alcotest.(check int) "one template" 1 (List.length templates)
         | _ -> Alcotest.fail "missing resourceTemplates")
      | _ -> Alcotest.fail "expected object")
   | Error e -> Alcotest.fail ("templates/list: " ^ e));

  Mt.close client_t
in

(* ── test: resource subscribe/unsubscribe ───── *)

let test_resource_subscribe () =
  Eio.Switch.run @@ fun sw ->
  let client_t, server_t = Mt.create_pair () in
  let res = Mcp_types.make_resource ~uri:"file:///data.json" ~name:"data" () in
  let server =
    Server.create ~name:"sub-server" ~version:"1.0" ()
    |> Server.add_resource res (fun _ctx _uri ->
      Ok [Mcp_types.{ uri = "file:///data.json"; mime_type = None;
                       text = Some "{}"; blob = None }])
  in
  Eio.Fiber.fork ~sw (fun () ->
    Server.run server ~transport:server_t ~clock ());
  let client = Client.create ~transport:client_t ~clock () in

  (match Client.initialize client ~client_name:"t" ~client_version:"1" with
   | Ok _ -> () | Error e -> Alcotest.fail e);

  (* Subscribe *)
  (match Client.subscribe_resource client ~uri:"file:///data.json" with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("subscribe: " ^ e));

  (* Unsubscribe *)
  (match Client.unsubscribe_resource client ~uri:"file:///data.json" with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("unsubscribe: " ^ e));

  Mt.close client_t
in

(* ── test suite ──────────────────────────────── *)

Alcotest.run "E2E_memory" [
  "lifecycle", [
    Alcotest.test_case "full lifecycle" `Quick test_full_lifecycle;
  ];
  "primitives", [
    Alcotest.test_case "prompts" `Quick test_prompts;
    Alcotest.test_case "resources" `Quick test_resources;
    Alcotest.test_case "resource templates" `Quick test_resource_templates;
    Alcotest.test_case "resource subscribe" `Quick test_resource_subscribe;
  ];
]
