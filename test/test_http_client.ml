(** Tests for HTTP client via actual HTTP loopback. *)

open Mcp_protocol
open Mcp_protocol_http

(* ── Helpers ─────────────────────────────────── *)

let echo_tool =
  Mcp_types.{
    name = "echo";
    description = Some "Echo tool";
    input_schema = `Assoc ["type", `String "object"];
    title = None;
    annotations = None;
    icon = None;
    output_schema = None;
    execution = None;
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

let greet_resource =
  Mcp_types.{
    uri = "test://greet";
    name = "greet";
    description = Some "A greeting resource";
    mime_type = Some "text/plain";
    icon = None;
  }

let greet_handler _ctx uri =
  Ok [Mcp_types.{
    uri;
    mime_type = Some "text/plain";
    text = Some "hello world";
    blob = None;
  }]

let hello_prompt =
  Mcp_types.{
    name = "hello";
    description = Some "A hello prompt";
    arguments = Some [Mcp_types.{
      name = "name";
      description = Some "Your name";
      required = Some true;
    }];
    icon = None;
  }

let hello_prompt_handler _ctx _name _args =
  Ok Mcp_types.{
    description = Some "A greeting";
    messages = [{
      role = User;
      content = PromptText { type_ = "text"; text = "Hello!" };
    }];
  }

let make_server () =
  Http_server.create ~name:"test-server" ~version:"1.0.0" ()
  |> Http_server.add_tool echo_tool echo_handler
  |> Http_server.add_resource greet_resource greet_handler
  |> Http_server.add_prompt hello_prompt hello_prompt_handler

(** Run a test with a running HTTP server. [f] receives (endpoint, net, sw). *)
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
  let endpoint = Printf.sprintf "http://127.0.0.1:%d/mcp" port in
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
      Fun.protect
        (fun () -> f endpoint net sw)
        ~finally:(fun () -> Eio.Promise.resolve stop_resolver ()))

let with_callback_server ~env callback f =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0) in
  let socket = Eio.Net.listen ~sw net addr ~backlog:16 in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | _ -> failwith "unexpected address"
  in
  let endpoint = Printf.sprintf "http://127.0.0.1:%d/mcp" port in
  let stop, stop_resolver = Eio.Promise.create () in
  let cohttp_server = Cohttp_eio.Server.make ~callback () in
  Eio.Fiber.both
    (fun () ->
      Cohttp_eio.Server.run ~stop socket cohttp_server
        ~on_error:(fun _exn -> ()))
    (fun () ->
      Fun.protect
        (fun () -> f endpoint net sw)
        ~finally:(fun () -> Eio.Promise.resolve stop_resolver ()))

let body_to_string body =
  Eio.Buf_read.of_flow ~max_size:(1024 * 1024) body |> Eio.Buf_read.take_all

let contains_substring ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop idx =
    if idx + needle_len > haystack_len then
      false
    else if String.sub haystack idx needle_len = needle then
      true
    else
      loop (idx + 1)
  in
  if needle_len = 0 then true else loop 0

let request_id body_str =
  match Yojson.Safe.from_string body_str with
  | `Assoc fields -> Option.value (List.assoc_opt "id" fields) ~default:(`Int 1)
  | _ -> `Int 1

let json_headers extra =
  Cohttp.Header.of_list (("content-type", "application/json") :: extra)

let respond_json ?(headers = []) body =
  Cohttp_eio.Server.respond_string ~headers:(json_headers headers) ~status:`OK
    ~body:(Yojson.Safe.to_string body) ()

let init_client client =
  match
    Http_client.initialize client ~client_name:"test" ~client_version:"1.0"
  with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "init: %s" e)

(* ── Tests ───────────────────────────────────── *)

let test_create env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    ignore client)

let test_initialize env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    match Http_client.initialize client
            ~client_name:"test" ~client_version:"1.0" with
    | Ok result ->
      Alcotest.(check string) "server name"
        "test-server" result.server_info.name;
      Alcotest.(check string) "server version"
        "1.0.0" result.server_info.version
    | Error e -> Alcotest.fail e)

let test_ping env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    (match Http_client.initialize client
             ~client_name:"test" ~client_version:"1.0" with
     | Ok _ -> ()
     | Error e -> Alcotest.fail e);
    match Http_client.ping client with
    | Ok () -> ()
    | Error e -> Alcotest.fail e)

let test_list_tools env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    (match Http_client.initialize client
             ~client_name:"test" ~client_version:"1.0" with
     | Ok _ -> ()
     | Error e -> Alcotest.fail e);
    match Http_client.list_tools client with
    | Ok tools ->
      Alcotest.(check int) "tool count" 1 (List.length tools);
      Alcotest.(check string) "tool name" "echo" (List.hd tools).name
    | Error e -> Alcotest.fail e)

let test_call_tool env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    (match Http_client.initialize client
             ~client_name:"test" ~client_version:"1.0" with
     | Ok _ -> ()
     | Error e -> Alcotest.fail e);
    let args = `Assoc [("text", `String "hello")] in
    match Http_client.call_tool client ~name:"echo" ~arguments:args () with
    | Ok result ->
      Alcotest.(check bool) "not error" false (Option.value ~default:false result.is_error);
      (match result.content with
       | [Mcp_types.TextContent { text; _ }] ->
         Alcotest.(check string) "echo text" "hello" text
       | _ -> Alcotest.fail "expected text content")
    | Error e -> Alcotest.fail e)

let test_list_resources env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    (match Http_client.initialize client
             ~client_name:"test" ~client_version:"1.0" with
     | Ok _ -> ()
     | Error e -> Alcotest.fail e);
    match Http_client.list_resources client with
    | Ok resources ->
      Alcotest.(check int) "resource count" 1 (List.length resources);
      Alcotest.(check string) "resource uri"
        "test://greet" (List.hd resources).uri
    | Error e -> Alcotest.fail e)

let test_read_resource env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    (match Http_client.initialize client
             ~client_name:"test" ~client_version:"1.0" with
     | Ok _ -> ()
     | Error e -> Alcotest.fail e);
    match Http_client.read_resource client ~uri:"test://greet" with
    | Ok contents ->
      Alcotest.(check int) "content count" 1 (List.length contents);
      Alcotest.(check (option string)) "text"
        (Some "hello world") (List.hd contents).text
    | Error e -> Alcotest.fail e)

let test_list_prompts env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    (match Http_client.initialize client
             ~client_name:"test" ~client_version:"1.0" with
     | Ok _ -> ()
     | Error e -> Alcotest.fail e);
    match Http_client.list_prompts client with
    | Ok prompts ->
      Alcotest.(check int) "prompt count" 1 (List.length prompts);
      Alcotest.(check string) "prompt name"
        "hello" (List.hd prompts).name
    | Error e -> Alcotest.fail e)

let test_get_prompt env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    (match Http_client.initialize client
             ~client_name:"test" ~client_version:"1.0" with
     | Ok _ -> ()
     | Error e -> Alcotest.fail e);
    match Http_client.get_prompt client
            ~name:"hello" ~arguments:[("name", "world")] () with
    | Ok result ->
      Alcotest.(check (option string)) "description"
        (Some "A greeting") result.description;
      Alcotest.(check int) "message count" 1
        (List.length result.messages)
    | Error e -> Alcotest.fail e)

let test_close env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    (match Http_client.initialize client
             ~client_name:"test" ~client_version:"1.0" with
     | Ok _ -> ()
     | Error e -> Alcotest.fail e);
    match Http_client.close client with
    | Ok () -> ()
    | Error e -> Alcotest.fail e)

let test_close_without_session env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    match Http_client.close client with
    | Ok () -> ()
    | Error e -> Alcotest.fail e)

let test_request_without_session env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    match Http_client.ping client with
    | Error _ -> ()
    | Ok () -> Alcotest.fail "expected error without session")

let test_notification_handler env () =
  with_server ~env (fun endpoint net sw ->
    let received = ref false in
    let client =
      Http_client.create ~endpoint ~net ~sw ()
      |> Http_client.on_notification (fun _method_ _params ->
           received := true)
    in
    (* Just verify client creation with handler works *)
    match Http_client.initialize client
            ~client_name:"test" ~client_version:"1.0" with
    | Ok _ -> ()
    | Error e -> Alcotest.fail e)

let test_initialize_from_multiline_sse env () =
  with_callback_server ~env
    (fun _conn _request body ->
      let _id = request_id (body_to_string body) in
      let sse_body =
        String.concat "\n"
          [
            "data: {\"jsonrpc\":\"2.0\",\"id\":1,";
            "data: \"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"sse-server\",\"version\":\"1.0.0\"}}}";
            "";
          ]
      in
      let headers =
        Cohttp.Header.of_list
          [
            ("content-type", "text/event-stream");
            (Http_session.header_name, "sse-session");
          ]
      in
      Cohttp_eio.Server.respond_string ~headers ~status:`OK ~body:sse_body ())
    (fun endpoint net sw ->
      let client = Http_client.create ~endpoint ~net ~sw () in
      match
        Http_client.initialize client ~client_name:"test" ~client_version:"1.0"
      with
      | Ok result ->
          Alcotest.(check string) "server name" "sse-server"
            result.server_info.name
      | Error e -> Alcotest.fail e)

let test_list_tools_all_detects_cursor_cycle env () =
  let tool_json name description =
    `Assoc
      [
        ("name", `String name);
        ("description", `String description);
        ("inputSchema", `Assoc [ ("type", `String "object") ]);
      ]
  in
  let page = ref 0 in
  with_callback_server ~env
    (fun _conn _request body ->
      let body_str = body_to_string body in
      match Yojson.Safe.from_string body_str with
      | `Assoc fields -> (
          match List.assoc_opt "method" fields with
          | Some (`String "initialize") ->
              let id = Option.value (List.assoc_opt "id" fields) ~default:(`Int 1) in
              respond_json ~headers:[ (Http_session.header_name, "loop-session") ]
                (`Assoc
                  [
                    ("jsonrpc", `String "2.0");
                    ("id", id);
                    ( "result",
                      `Assoc
                        [
                          ("protocolVersion", `String "2025-11-25");
                          ("capabilities", `Assoc []);
                          ( "serverInfo",
                            `Assoc
                              [
                                ("name", `String "loop-server");
                                ("version", `String "1.0.0");
                              ] );
                        ] );
                  ])
          | Some (`String "notifications/initialized") ->
              Cohttp_eio.Server.respond_string ~status:`Accepted ~body:"" ()
          | Some (`String "tools/list") ->
              incr page;
              let id = Option.value (List.assoc_opt "id" fields) ~default:(`Int 2) in
              let tools =
                if !page = 1 then [ tool_json "one" "first" ]
                else [ tool_json "two" "second" ]
              in
              respond_json
                (`Assoc
                  [
                    ("jsonrpc", `String "2.0");
                    ("id", id);
                    ( "result",
                      `Assoc
                        [
                          ("tools", `List tools);
                          ("nextCursor", `String "loop");
                        ] );
                  ])
          | _ -> Cohttp_eio.Server.respond_string ~status:`Bad_request ~body:"bad request" ())
      | _ -> Cohttp_eio.Server.respond_string ~status:`Bad_request ~body:"bad request" ())
    (fun endpoint net sw ->
      let client = Http_client.create ~endpoint ~net ~sw () in
      init_client client;
      match Http_client.list_tools_all client with
      | Ok _ -> Alcotest.fail "expected cursor loop detection"
      | Error msg ->
          Alcotest.(check bool) "loop detected" true
            (contains_substring ~needle:"cursor loop detected" msg))

let test_full_lifecycle env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    (* Initialize *)
    (match Http_client.initialize client
             ~client_name:"test" ~client_version:"1.0" with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Printf.sprintf "init: %s" e));
    (* Ping *)
    (match Http_client.ping client with
     | Ok () -> ()
     | Error e -> Alcotest.fail (Printf.sprintf "ping: %s" e));
    (* List tools *)
    (match Http_client.list_tools client with
     | Ok tools ->
       Alcotest.(check int) "tools" 1 (List.length tools)
     | Error e -> Alcotest.fail (Printf.sprintf "list_tools: %s" e));
    (* Call tool *)
    let args = `Assoc [("text", `String "lifecycle")] in
    (match Http_client.call_tool client ~name:"echo" ~arguments:args () with
     | Ok result ->
       Alcotest.(check bool) "not error" false (Option.value ~default:false result.is_error)
     | Error e -> Alcotest.fail (Printf.sprintf "call_tool: %s" e));
    (* List resources *)
    (match Http_client.list_resources client with
     | Ok resources ->
       Alcotest.(check int) "resources" 1 (List.length resources)
     | Error e -> Alcotest.fail (Printf.sprintf "list_resources: %s" e));
    (* Read resource *)
    (match Http_client.read_resource client ~uri:"test://greet" with
     | Ok contents ->
       Alcotest.(check int) "contents" 1 (List.length contents)
     | Error e -> Alcotest.fail (Printf.sprintf "read_resource: %s" e));
    (* List prompts *)
    (match Http_client.list_prompts client with
     | Ok prompts ->
       Alcotest.(check int) "prompts" 1 (List.length prompts)
     | Error e -> Alcotest.fail (Printf.sprintf "list_prompts: %s" e));
    (* Get prompt *)
    (match Http_client.get_prompt client
             ~name:"hello" ~arguments:[("name", "test")] () with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Printf.sprintf "get_prompt: %s" e));
    (* Close *)
    (match Http_client.close client with
     | Ok () -> ()
     | Error e -> Alcotest.fail (Printf.sprintf "close: %s" e)))

(* ── Test runner ─────────────────────────────── *)

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "HTTP_client" [
    "construction", [
      Alcotest.test_case "create" `Quick (test_create env);
    ];
    "lifecycle", [
      Alcotest.test_case "initialize" `Quick (test_initialize env);
      Alcotest.test_case "initialize from multiline sse" `Quick
        (test_initialize_from_multiline_sse env);
      Alcotest.test_case "ping" `Quick (test_ping env);
      Alcotest.test_case "close" `Quick (test_close env);
      Alcotest.test_case "close without session" `Quick
        (test_close_without_session env);
      Alcotest.test_case "request without session" `Quick
        (test_request_without_session env);
      Alcotest.test_case "notification handler" `Quick
        (test_notification_handler env);
      Alcotest.test_case "full lifecycle" `Quick
        (test_full_lifecycle env);
    ];
    "tools", [
      Alcotest.test_case "list" `Quick (test_list_tools env);
      Alcotest.test_case "list all detects cursor cycle" `Quick
        (test_list_tools_all_detects_cursor_cycle env);
      Alcotest.test_case "call" `Quick (test_call_tool env);
    ];
    "resources", [
      Alcotest.test_case "list" `Quick (test_list_resources env);
      Alcotest.test_case "read" `Quick (test_read_resource env);
    ];
    "prompts", [
      Alcotest.test_case "list" `Quick (test_list_prompts env);
      Alcotest.test_case "get" `Quick (test_get_prompt env);
    ];
  ]
