(** Integration tests for Generic_server + Memory_transport.

    Proves that Generic_server.Make(Memory_transport) produces a fully
    functional MCP server — initialize handshake, tool dispatch, and
    notifications all work without any IO. *)

open Mcp_protocol
module Mt = Mcp_protocol_eio.Memory_transport
module GS = Mcp_protocol_eio.Generic_server.Make(Mt)

let () = Eio_main.run @@ fun env ->

(* ── helper: run server in background fiber ───── *)

let run_server_fiber ~sw server server_t =
  Eio.Fiber.fork ~sw (fun () ->
    GS.run server ~transport:server_t
      ~clock:(Eio.Stdenv.clock env) ())
in

(* ── helper: send request and read response ───── *)

let send_request client_t id method_ ?params () =
  let msg = Jsonrpc.make_request ~id:(Jsonrpc.Int id) ~method_ ?params () in
  match Mt.write client_t msg with
  | Error e -> Error e
  | Ok () ->
    let rec loop () =
      match Mt.read client_t with
      | None -> Error "EOF"
      | Some (Error e) -> Error e
      | Some (Ok (Jsonrpc.Response resp)) when resp.id = Jsonrpc.Int id ->
        Ok resp.result
      | Some (Ok (Jsonrpc.Error err)) when err.id = Jsonrpc.Int id ->
        Error err.error.message
      | Some (Ok _) -> loop ()
    in
    loop ()
in

let send_notification client_t method_ =
  let msg = Jsonrpc.make_notification ~method_ () in
  Mt.write client_t msg
in

(* ── test: initialize handshake ───────────────── *)

let test_initialize () =
  Eio.Switch.run @@ fun sw ->
  let client_t, server_t = Mt.create_pair () in
  let server =
    GS.create ~name:"test-server" ~version:"1.0.0"
      ~instructions:"Test server" ()
  in
  run_server_fiber ~sw server server_t;

  (* Send initialize request *)
  let params = `Assoc [
    ("protocolVersion", `String "2025-11-25");
    ("capabilities", `Assoc []);
    ("clientInfo", `Assoc [
      ("name", `String "test-client");
      ("version", `String "1.0");
    ]);
  ] in
  (match send_request client_t 1 "initialize" ~params () with
   | Ok result ->
     (match result with
      | `Assoc fields ->
        let server_info = List.assoc_opt "serverInfo" fields in
        (match server_info with
         | Some (`Assoc info) ->
           let name = match List.assoc_opt "name" info with
             | Some (`String s) -> s | _ -> ""
           in
           Alcotest.(check string) "server name" "test-server" name
         | _ -> Alcotest.fail "missing serverInfo")
      | _ -> Alcotest.fail "expected object response")
   | Error e -> Alcotest.fail ("initialize failed: " ^ e));

  (* Send initialized notification *)
  (match send_notification client_t "notifications/initialized" with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("initialized notification failed: " ^ e));

  Mt.close client_t
in

(* ── test: tool call ──────────────────────────── *)

let test_tool_call () =
  Eio.Switch.run @@ fun sw ->
  let client_t, server_t = Mt.create_pair () in
  let echo_tool = Mcp_types.make_tool
    ~name:"echo" ~description:"Echo back the input" () in
  let server =
    GS.create ~name:"echo-server" ~version:"1.0.0" ()
    |> GS.add_tool echo_tool (fun _ctx _name args ->
      let text = match args with
        | Some (`Assoc fields) ->
          (match List.assoc_opt "text" fields with
           | Some (`String s) -> s
           | _ -> "no text")
        | _ -> "no args"
      in
      Ok (Mcp_types.tool_result_of_text ("echo: " ^ text)))
  in
  run_server_fiber ~sw server server_t;

  (* Initialize first *)
  let init_params = `Assoc [
    ("protocolVersion", `String "2025-11-25");
    ("capabilities", `Assoc []);
    ("clientInfo", `Assoc [("name", `String "test"); ("version", `String "1")]);
  ] in
  ignore (send_request client_t 1 "initialize" ~params:init_params ());
  ignore (send_notification client_t "notifications/initialized");

  (* Call the echo tool *)
  let call_params = `Assoc [
    ("name", `String "echo");
    ("arguments", `Assoc [("text", `String "hello world")]);
  ] in
  (match send_request client_t 2 "tools/call" ~params:call_params () with
   | Ok result ->
     let json_str = Yojson.Safe.to_string result in
     Alcotest.(check bool) "contains echo response"
       true (String.length json_str > 0);
     (* Parse the tool result *)
     (match Mcp_types.tool_result_of_yojson result with
      | Ok tr ->
        (match tr.content with
         | [Mcp_types.TextContent { text; _ }] ->
           Alcotest.(check string) "echo text" "echo: hello world" text
         | _ -> Alcotest.fail "expected single TextContent")
      | Error e -> Alcotest.fail ("parse tool_result: " ^ e))
   | Error e -> Alcotest.fail ("tool call failed: " ^ e));

  Mt.close client_t
in

(* ── test: tools/list ─────────────────────────── *)

let test_tools_list () =
  Eio.Switch.run @@ fun sw ->
  let client_t, server_t = Mt.create_pair () in
  let t1 = Mcp_types.make_tool ~name:"alpha" () in
  let t2 = Mcp_types.make_tool ~name:"beta" ~description:"second tool" () in
  let noop _ctx _name _args = Ok (Mcp_types.tool_result_of_text "ok") in
  let server =
    GS.create ~name:"s" ~version:"1" ()
    |> GS.add_tool t1 noop
    |> GS.add_tool t2 noop
  in
  run_server_fiber ~sw server server_t;

  (* Initialize *)
  let init = `Assoc [
    ("protocolVersion", `String "2025-11-25");
    ("capabilities", `Assoc []);
    ("clientInfo", `Assoc [("name", `String "t"); ("version", `String "1")]);
  ] in
  ignore (send_request client_t 1 "initialize" ~params:init ());
  ignore (send_notification client_t "notifications/initialized");

  (* List tools *)
  (match send_request client_t 2 "tools/list" () with
   | Ok result ->
     (match result with
      | `Assoc fields ->
        (match List.assoc_opt "tools" fields with
         | Some (`List tools) ->
           Alcotest.(check int) "two tools" 2 (List.length tools)
         | _ -> Alcotest.fail "missing tools array")
      | _ -> Alcotest.fail "expected object")
   | Error e -> Alcotest.fail ("tools/list failed: " ^ e));

  Mt.close client_t
in

(* ── test: ping ───────────────────────────────── *)

let test_ping () =
  Eio.Switch.run @@ fun sw ->
  let client_t, server_t = Mt.create_pair () in
  let server = GS.create ~name:"s" ~version:"1" () in
  run_server_fiber ~sw server server_t;

  (* Initialize *)
  let init = `Assoc [
    ("protocolVersion", `String "2025-11-25");
    ("capabilities", `Assoc []);
    ("clientInfo", `Assoc [("name", `String "t"); ("version", `String "1")]);
  ] in
  ignore (send_request client_t 1 "initialize" ~params:init ());
  ignore (send_notification client_t "notifications/initialized");

  (* Ping *)
  (match send_request client_t 2 "ping" () with
   | Ok _ -> ()
   | Error e -> Alcotest.fail ("ping failed: " ^ e));

  Mt.close client_t
in

(* ── test: middleware composition ─────────────── *)

let test_middleware_composition () =
  (* Verify that Logging middleware composes with Generic_server *)
  let module Logged_mt = Mcp_protocol_eio.Middleware.Logging(Mt) in
  let module Logged_server = Mcp_protocol_eio.Generic_server.Make(Logged_mt) in
  Eio.Switch.run @@ fun sw ->
  let client_raw, server_raw = Mt.create_pair () in
  let server_t = Logged_mt.wrap ~label:"test" server_raw in
  let server = Logged_server.create ~name:"logged" ~version:"1" () in
  Eio.Fiber.fork ~sw (fun () ->
    Logged_server.run server ~transport:server_t
      ~clock:(Eio.Stdenv.clock env) ());

  (* Initialize through the middleware *)
  let init = `Assoc [
    ("protocolVersion", `String "2025-11-25");
    ("capabilities", `Assoc []);
    ("clientInfo", `Assoc [("name", `String "t"); ("version", `String "1")]);
  ] in
  (match send_request client_raw 1 "initialize" ~params:init () with
   | Ok _ -> ()
   | Error e -> Alcotest.fail ("middleware init failed: " ^ e));

  Mt.close client_raw
in

(* ── test suite ──────────────────────────────── *)

Alcotest.run "Generic_server" [
  "lifecycle", [
    Alcotest.test_case "initialize handshake" `Quick test_initialize;
    Alcotest.test_case "ping" `Quick test_ping;
  ];
  "tools", [
    Alcotest.test_case "tool call" `Quick test_tool_call;
    Alcotest.test_case "tools/list" `Quick test_tools_list;
  ];
  "middleware", [
    Alcotest.test_case "logging middleware" `Quick test_middleware_composition;
  ];
]
