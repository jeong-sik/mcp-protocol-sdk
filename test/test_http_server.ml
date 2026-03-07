(** Tests for HTTP server via actual HTTP requests. *)

open Mcp_protocol
open Mcp_protocol_http

(* ── Helpers ─────────────────────────────────── *)

let read_body body =
  Eio.Buf_read.of_flow ~max_size:(1024 * 1024) body
  |> Eio.Buf_read.take_all

let json_of_body body =
  Yojson.Safe.from_string (read_body body)

let make_initialize_json () =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0.0"}}}|}

let make_ping_json () =
  {|{"jsonrpc":"2.0","id":2,"method":"ping"}|}

let make_tools_list_json () =
  {|{"jsonrpc":"2.0","id":3,"method":"tools/list"}|}

let make_notification_json () =
  {|{"jsonrpc":"2.0","method":"notifications/initialized"}|}

let echo_tool =
  Mcp_types.{
    name = "echo";
    description = Some "Echo tool";
    input_schema = `Assoc ["type", `String "object"];
    title = None;
    annotations = None;
  }

let echo_handler _ctx _name args =
  let text = match args with
    | Some (`Assoc fields) ->
      (match List.assoc_opt "text" fields with
       | Some (`String s) -> s
       | _ -> "no text")
    | _ -> "no args"
  in
  Ok (Mcp_types.tool_result_of_text text)

let make_server () =
  Http_server.create ~name:"test-server" ~version:"1.0.0" ()
  |> Http_server.add_tool echo_tool echo_handler

(** Run a test with a running HTTP server. [f] receives (client, port). *)
let with_server ~env f =
  Eio.Switch.run @@ fun sw ->
  let s = make_server () in
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0) in
  let socket = Eio.Net.listen ~sw net addr ~backlog:16 in
  let port = match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | _ -> failwith "unexpected address"
  in
  let stop, stop_resolver = Eio.Promise.create () in
  let cohttp_server = Cohttp_eio.Server.make
    ~callback:(fun conn request body ->
      Http_server.callback s conn request body)
    () in
  Eio.Fiber.both
    (fun () ->
      Cohttp_eio.Server.run ~stop socket cohttp_server
        ~on_error:(fun _exn -> ()))
    (fun () ->
      let client = Cohttp_eio.Client.make ~https:None net in
      Fun.protect
        (fun () -> f client port)
        ~finally:(fun () -> Eio.Promise.resolve stop_resolver ()))

let base_uri port path =
  Uri.of_string (Printf.sprintf "http://127.0.0.1:%d%s" port path)

let post_json client ~sw ~headers port path body_str =
  let body = Cohttp_eio.Body.of_string body_str in
  let headers = Http.Header.of_list
    (("Content-Type", "application/json") :: headers) in
  Cohttp_eio.Client.post client ~sw ~body ~headers (base_uri port path)

let get_req client ~sw ~headers port path =
  let headers = Http.Header.of_list headers in
  Cohttp_eio.Client.get client ~sw ~headers (base_uri port path)

let delete_req client ~sw ~headers port path =
  let headers = Http.Header.of_list headers in
  Cohttp_eio.Client.delete client ~sw ~headers (base_uri port path)

(* ── Construction tests ──────────────────────── *)

let test_create () =
  let _s = Http_server.create ~name:"test" ~version:"1.0" () in
  ()

let test_add_tool () =
  let _s = Http_server.create ~name:"test" ~version:"1.0" ()
    |> Http_server.add_tool echo_tool echo_handler in
  ()

(* ── POST /mcp tests (initialize) ───────────── *)

let test_initialize env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    let resp, body = post_json client ~sw ~headers:[] port "/mcp"
      (make_initialize_json ()) in
    let status = Http.Response.status resp in
    Alcotest.(check int) "status 200" 200 (Http.Status.to_int status);
    let json = json_of_body body in
    (* Check it's a valid JSON-RPC response *)
    (match json with
     | `Assoc fields ->
       Alcotest.(check bool) "has jsonrpc" true
         (List.mem_assoc "jsonrpc" fields);
       Alcotest.(check bool) "has result" true
         (List.mem_assoc "result" fields)
     | _ -> Alcotest.fail "expected JSON object");
    (* Check Mcp-Session-Id header *)
    let sid = Http.Header.get (Http.Response.headers resp) "mcp-session-id" in
    Alcotest.(check bool) "has session id" true (Option.is_some sid))

(* ── POST /mcp tests (ping after init) ──────── *)

let test_ping_after_init env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    (* Initialize first *)
    let resp, _body = post_json client ~sw ~headers:[] port "/mcp"
      (make_initialize_json ()) in
    let sid = Http.Header.get (Http.Response.headers resp) "mcp-session-id" in
    let sid = Option.get sid in
    (* Ping with session ID *)
    let resp2, body2 = post_json client ~sw
      ~headers:[("mcp-session-id", sid)] port "/mcp"
      (make_ping_json ()) in
    Alcotest.(check int) "status 200" 200
      (Http.Status.to_int (Http.Response.status resp2));
    let json = json_of_body body2 in
    match json with
    | `Assoc fields ->
      Alcotest.(check bool) "has result" true (List.mem_assoc "result" fields)
    | _ -> Alcotest.fail "expected JSON object")

(* ── POST without session ID ─────────────────── *)

let test_post_without_session_id env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    (* Initialize to create session *)
    let resp, _body = post_json client ~sw ~headers:[] port "/mcp"
      (make_initialize_json ()) in
    let _sid = Http.Header.get (Http.Response.headers resp) "mcp-session-id" in
    (* Try ping without session header *)
    let resp2, _body2 = post_json client ~sw ~headers:[] port "/mcp"
      (make_ping_json ()) in
    Alcotest.(check int) "status 400" 400
      (Http.Status.to_int (Http.Response.status resp2)))

(* ── POST with wrong session ID ──────────────── *)

let test_post_wrong_session_id env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    (* Initialize to create session *)
    let _resp, _body = post_json client ~sw ~headers:[] port "/mcp"
      (make_initialize_json ()) in
    (* Try ping with wrong session ID *)
    let resp2, _body2 = post_json client ~sw
      ~headers:[("mcp-session-id", "wrong-id")] port "/mcp"
      (make_ping_json ()) in
    Alcotest.(check int) "status 404" 404
      (Http.Status.to_int (Http.Response.status resp2)))

(* ── POST invalid JSON ───────────────────────── *)

let test_post_invalid_json env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    let _resp, body = post_json client ~sw ~headers:[] port "/mcp"
      "not json at all" in
    let json = json_of_body body in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "error" fields with
       | Some (`Assoc err_fields) ->
         let code = List.assoc_opt "code" err_fields in
         Alcotest.(check bool) "parse error code" true
           (code = Some (`Int Error_codes.parse_error))
       | _ -> Alcotest.fail "expected error object")
    | _ -> Alcotest.fail "expected JSON object")

(* ── POST notification (no response body) ────── *)

let test_post_notification env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    (* Initialize first *)
    let resp, _body = post_json client ~sw ~headers:[] port "/mcp"
      (make_initialize_json ()) in
    let sid = Option.get
      (Http.Header.get (Http.Response.headers resp) "mcp-session-id") in
    (* Send notification *)
    let resp2, _body2 = post_json client ~sw
      ~headers:[("mcp-session-id", sid)] port "/mcp"
      (make_notification_json ()) in
    Alcotest.(check int) "status 202" 202
      (Http.Status.to_int (Http.Response.status resp2)))

(* ── Tools list ──────────────────────────────── *)

let test_tools_list env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    (* Initialize *)
    let resp, _body = post_json client ~sw ~headers:[] port "/mcp"
      (make_initialize_json ()) in
    let sid = Option.get
      (Http.Header.get (Http.Response.headers resp) "mcp-session-id") in
    (* List tools *)
    let _resp2, body2 = post_json client ~sw
      ~headers:[("mcp-session-id", sid)] port "/mcp"
      (make_tools_list_json ()) in
    let json = json_of_body body2 in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`Assoc result) ->
         (match List.assoc_opt "tools" result with
          | Some (`List tools) ->
            Alcotest.(check int) "one tool" 1 (List.length tools)
          | _ -> Alcotest.fail "expected tools list")
       | _ -> Alcotest.fail "expected result")
    | _ -> Alcotest.fail "expected JSON object")

(* ── Tools call ──────────────────────────────── *)

let test_tools_call env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    (* Initialize *)
    let resp, _body = post_json client ~sw ~headers:[] port "/mcp"
      (make_initialize_json ()) in
    let sid = Option.get
      (Http.Header.get (Http.Response.headers resp) "mcp-session-id") in
    (* Call echo tool *)
    let call_json = Printf.sprintf
      {|{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"echo","arguments":{"text":"hello"}}}|} in
    let _resp2, body2 = post_json client ~sw
      ~headers:[("mcp-session-id", sid)] port "/mcp" call_json in
    let json = json_of_body body2 in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`Assoc result) ->
         (match List.assoc_opt "content" result with
          | Some (`List [`Assoc content_fields]) ->
            let text = List.assoc_opt "text" content_fields in
            Alcotest.(check (option string)) "echo text"
              (Some "hello")
              (match text with Some (`String s) -> Some s | _ -> None)
          | _ -> Alcotest.fail "expected content list")
       | _ -> Alcotest.fail "expected result")
    | _ -> Alcotest.fail "expected JSON object")

(* ── GET /mcp (SSE endpoint) ─────────────────── *)

let test_get_without_session env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    (* Initialize to create session *)
    let _resp, _body = post_json client ~sw ~headers:[] port "/mcp"
      (make_initialize_json ()) in
    (* GET without session ID *)
    let resp, _body = get_req client ~sw ~headers:[] port "/mcp" in
    Alcotest.(check int) "status 400" 400
      (Http.Status.to_int (Http.Response.status resp)))

let test_get_with_session env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    (* Initialize *)
    let resp, _body = post_json client ~sw ~headers:[] port "/mcp"
      (make_initialize_json ()) in
    let sid = Option.get
      (Http.Header.get (Http.Response.headers resp) "mcp-session-id") in
    (* GET with session ID *)
    let resp2, _body2 = get_req client ~sw
      ~headers:[("mcp-session-id", sid)] port "/mcp" in
    Alcotest.(check int) "status 200" 200
      (Http.Status.to_int (Http.Response.status resp2));
    let ct = Http.Header.get (Http.Response.headers resp2) "content-type" in
    Alcotest.(check (option string)) "SSE content type"
      (Some "text/event-stream") ct)

(* ── DELETE /mcp ─────────────────────────────── *)

let test_delete_session env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    (* Initialize *)
    let resp, _body = post_json client ~sw ~headers:[] port "/mcp"
      (make_initialize_json ()) in
    let sid = Option.get
      (Http.Header.get (Http.Response.headers resp) "mcp-session-id") in
    (* DELETE *)
    let resp2, _body2 = delete_req client ~sw
      ~headers:[("mcp-session-id", sid)] port "/mcp" in
    Alcotest.(check int) "status 200" 200
      (Http.Status.to_int (Http.Response.status resp2)))

let test_delete_without_session env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    (* Initialize to create session *)
    let _resp, _body = post_json client ~sw ~headers:[] port "/mcp"
      (make_initialize_json ()) in
    (* DELETE without session ID *)
    let resp, _body = delete_req client ~sw ~headers:[] port "/mcp" in
    Alcotest.(check int) "status 400" 400
      (Http.Status.to_int (Http.Response.status resp)))

(* ── Wrong path ──────────────────────────────── *)

let test_wrong_path env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    let resp, _body = post_json client ~sw ~headers:[] port "/wrong"
      (make_initialize_json ()) in
    Alcotest.(check int) "status 404" 404
      (Http.Status.to_int (Http.Response.status resp)))

(* ── Method not allowed ──────────────────────── *)

let test_method_not_allowed env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    let headers = Http.Header.of_list [] in
    let resp, _body = Cohttp_eio.Client.put client ~sw ~headers
      (base_uri port "/mcp") in
    Alcotest.(check int) "status 405" 405
      (Http.Status.to_int (Http.Response.status resp)))

(* ── OPTIONS /mcp (CORS) ────────────────────── *)

let test_options_cors env () =
  with_server ~env (fun client port ->
    Eio.Switch.run @@ fun sw ->
    let headers = Http.Header.of_list [] in
    let resp, _body = Cohttp_eio.Client.call client ~sw `OPTIONS ~headers
      (base_uri port "/mcp") in
    Alcotest.(check int) "status 200" 200
      (Http.Status.to_int (Http.Response.status resp));
    let hdrs = Http.Response.headers resp in
    Alcotest.(check (option string)) "allow-origin"
      (Some "*")
      (Http.Header.get hdrs "access-control-allow-origin");
    Alcotest.(check bool) "allow-methods present" true
      (Option.is_some (Http.Header.get hdrs "access-control-allow-methods")))

(* ── Test suite ──────────────────────────────── *)

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "HTTP_server" [
    "construction", [
      Alcotest.test_case "create" `Quick test_create;
      Alcotest.test_case "add_tool" `Quick test_add_tool;
    ];
    "post", [
      Alcotest.test_case "initialize" `Quick (test_initialize env);
      Alcotest.test_case "ping after init" `Quick (test_ping_after_init env);
      Alcotest.test_case "without session id" `Quick (test_post_without_session_id env);
      Alcotest.test_case "wrong session id" `Quick (test_post_wrong_session_id env);
      Alcotest.test_case "invalid json" `Quick (test_post_invalid_json env);
      Alcotest.test_case "notification" `Quick (test_post_notification env);
      Alcotest.test_case "tools list" `Quick (test_tools_list env);
      Alcotest.test_case "tools call" `Quick (test_tools_call env);
    ];
    "get", [
      Alcotest.test_case "without session" `Quick (test_get_without_session env);
      Alcotest.test_case "with session" `Quick (test_get_with_session env);
    ];
    "delete", [
      Alcotest.test_case "session" `Quick (test_delete_session env);
      Alcotest.test_case "without session" `Quick (test_delete_without_session env);
    ];
    "routing", [
      Alcotest.test_case "wrong path" `Quick (test_wrong_path env);
      Alcotest.test_case "method not allowed" `Quick (test_method_not_allowed env);
      Alcotest.test_case "options cors" `Quick (test_options_cors env);
    ];
  ]
