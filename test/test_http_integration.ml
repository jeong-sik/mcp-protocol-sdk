(** Integration tests for HTTP client+server working together.
    Covers edge cases not in unit tests: bad sessions, error
    handling, multiple resources, concurrent patterns. *)

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

let upper_tool =
  Mcp_types.{
    name = "upper";
    description = Some "Uppercase tool";
    input_schema = `Assoc ["type", `String "object"];
    title = None;
    annotations = None;
    icon = None;
    output_schema = None;
    execution = None;
  }

let upper_handler _ctx _name args =
  let text = match args with
    | Some (`Assoc fields) ->
      (match List.assoc_opt "text" fields with
       | Some (`String s) -> String.uppercase_ascii s
       | _ -> "NO TEXT")
    | _ -> "NO ARGS"
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

let status_resource =
  Mcp_types.{
    uri = "test://status";
    name = "status";
    description = Some "A status resource";
    mime_type = Some "application/json";
    icon = None;
  }

let status_handler _ctx uri =
  Ok [Mcp_types.{
    uri;
    mime_type = Some "application/json";
    text = Some {|{"ok":true}|};
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
  Http_server.create ~name:"integration-server" ~version:"1.0.0" ()
  |> Http_server.add_tool echo_tool echo_handler
  |> Http_server.add_tool upper_tool upper_handler
  |> Http_server.add_resource greet_resource greet_handler
  |> Http_server.add_resource status_resource status_handler
  |> Http_server.add_prompt hello_prompt hello_prompt_handler

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

let init_client client =
  match Http_client.initialize client
          ~client_name:"test" ~client_version:"1.0" with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "init: %s" e)

(* ── Tests ───────────────────────────────────── *)

(** Multiple tools listed and callable *)
let test_multiple_tools env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    init_client client;
    match Http_client.list_tools client with
    | Ok tools ->
      Alcotest.(check int) "tool count" 2 (List.length tools);
      let names = List.map (fun (t : Mcp_types.tool) -> t.name) tools in
      Alcotest.(check bool) "has echo" true (List.mem "echo" names);
      Alcotest.(check bool) "has upper" true (List.mem "upper" names)
    | Error e -> Alcotest.fail e)

(** Call each tool and verify output *)
let test_call_each_tool env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    init_client client;
    (* echo *)
    let args = `Assoc [("text", `String "foo")] in
    (match Http_client.call_tool client ~name:"echo" ~arguments:args () with
     | Ok result ->
       (match result.content with
        | [Mcp_types.TextContent { text; _ }] ->
          Alcotest.(check string) "echo" "foo" text
        | _ -> Alcotest.fail "expected text content")
     | Error e -> Alcotest.fail e);
    (* upper *)
    let args = `Assoc [("text", `String "hello")] in
    (match Http_client.call_tool client ~name:"upper" ~arguments:args () with
     | Ok result ->
       (match result.content with
        | [Mcp_types.TextContent { text; _ }] ->
          Alcotest.(check string) "upper" "HELLO" text
        | _ -> Alcotest.fail "expected text content")
     | Error e -> Alcotest.fail e))

(** Multiple resources listed and readable *)
let test_multiple_resources env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    init_client client;
    match Http_client.list_resources client with
    | Ok resources ->
      Alcotest.(check int) "resource count" 2 (List.length resources);
      (* Read each *)
      (match Http_client.read_resource client ~uri:"test://greet" with
       | Ok contents ->
         Alcotest.(check (option string)) "greet text"
           (Some "hello world") (List.hd contents).text
       | Error e -> Alcotest.fail e);
      (match Http_client.read_resource client ~uri:"test://status" with
       | Ok contents ->
         Alcotest.(check (option string)) "status text"
           (Some {|{"ok":true}|}) (List.hd contents).text
       | Error e -> Alcotest.fail e)
    | Error e -> Alcotest.fail e)

(** Sequential lifecycle: init → work → close → cannot reuse *)
let test_close_prevents_reuse env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    init_client client;
    (* Work *)
    (match Http_client.ping client with
     | Ok () -> ()
     | Error e -> Alcotest.fail e);
    (* Close *)
    (match Http_client.close client with
     | Ok () -> ()
     | Error e -> Alcotest.fail e);
    (* After close, session_id is cleared → requests fail *)
    match Http_client.ping client with
    | Error _ -> ()
    | Ok () -> Alcotest.fail "expected error after close")

(** Large text payload roundtrip *)
let test_large_payload env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    init_client client;
    let big_text = String.make 100_000 'x' in
    let args = `Assoc [("text", `String big_text)] in
    match Http_client.call_tool client ~name:"echo" ~arguments:args () with
    | Ok result ->
      (match result.content with
       | [Mcp_types.TextContent { text; _ }] ->
         Alcotest.(check int) "large text length"
           100_000 (String.length text)
       | _ -> Alcotest.fail "expected text content")
    | Error e -> Alcotest.fail e)

(** Multiple sequential requests on same client *)
let test_sequential_requests env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    init_client client;
    for i = 1 to 10 do
      let text = Printf.sprintf "msg-%d" i in
      let args = `Assoc [("text", `String text)] in
      match Http_client.call_tool client ~name:"echo" ~arguments:args () with
      | Ok result ->
        (match result.content with
         | [Mcp_types.TextContent { text = t; _ }] ->
           Alcotest.(check string) (Printf.sprintf "seq %d" i) text t
         | _ -> Alcotest.fail "expected text content")
      | Error e -> Alcotest.fail e
    done)

(** Full end-to-end: init → tools → resources → prompts → close *)
let test_full_integration env () =
  with_server ~env (fun endpoint net sw ->
    let client = Http_client.create ~endpoint ~net ~sw () in
    (* Initialize *)
    (match Http_client.initialize client
             ~client_name:"integration" ~client_version:"1.0" with
     | Ok result ->
       Alcotest.(check string) "server name"
         "integration-server" result.server_info.name
     | Error e -> Alcotest.fail (Printf.sprintf "init: %s" e));
    (* Ping *)
    (match Http_client.ping client with
     | Ok () -> ()
     | Error e -> Alcotest.fail (Printf.sprintf "ping: %s" e));
    (* Tools *)
    (match Http_client.list_tools client with
     | Ok tools ->
       Alcotest.(check int) "tools" 2 (List.length tools)
     | Error e -> Alcotest.fail (Printf.sprintf "list_tools: %s" e));
    (* Call echo *)
    let args = `Assoc [("text", `String "integration")] in
    (match Http_client.call_tool client ~name:"echo" ~arguments:args () with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Printf.sprintf "call_tool: %s" e));
    (* Call upper *)
    let args = `Assoc [("text", `String "test")] in
    (match Http_client.call_tool client ~name:"upper" ~arguments:args () with
     | Ok result ->
       (match result.content with
        | [Mcp_types.TextContent { text; _ }] ->
          Alcotest.(check string) "upper result" "TEST" text
        | _ -> Alcotest.fail "expected text content")
     | Error e -> Alcotest.fail (Printf.sprintf "call_upper: %s" e));
    (* Resources *)
    (match Http_client.list_resources client with
     | Ok resources ->
       Alcotest.(check int) "resources" 2 (List.length resources)
     | Error e -> Alcotest.fail (Printf.sprintf "list_resources: %s" e));
    (match Http_client.read_resource client ~uri:"test://greet" with
     | Ok contents ->
       Alcotest.(check (option string)) "greet"
         (Some "hello world") (List.hd contents).text
     | Error e -> Alcotest.fail (Printf.sprintf "read_resource: %s" e));
    (* Prompts *)
    (match Http_client.list_prompts client with
     | Ok prompts ->
       Alcotest.(check int) "prompts" 1 (List.length prompts)
     | Error e -> Alcotest.fail (Printf.sprintf "list_prompts: %s" e));
    (match Http_client.get_prompt client
             ~name:"hello" ~arguments:[("name", "world")] () with
     | Ok result ->
       Alcotest.(check (option string)) "prompt desc"
         (Some "A greeting") result.description
     | Error e -> Alcotest.fail (Printf.sprintf "get_prompt: %s" e));
    (* Close *)
    (match Http_client.close client with
     | Ok () -> ()
     | Error e -> Alcotest.fail (Printf.sprintf "close: %s" e)))

(* ── Test runner ─────────────────────────────── *)

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "HTTP_integration" [
    "multi-capability", [
      Alcotest.test_case "multiple tools" `Quick
        (test_multiple_tools env);
      Alcotest.test_case "call each tool" `Quick
        (test_call_each_tool env);
      Alcotest.test_case "multiple resources" `Quick
        (test_multiple_resources env);
    ];
    "edge cases", [
      Alcotest.test_case "close prevents reuse" `Quick
        (test_close_prevents_reuse env);
      Alcotest.test_case "large payload" `Quick
        (test_large_payload env);
      Alcotest.test_case "sequential requests" `Quick
        (test_sequential_requests env);
    ];
    "end-to-end", [
      Alcotest.test_case "full integration" `Quick
        (test_full_integration env);
    ];
  ]
