(** Tests for MCP Server framework. *)

open Mcp_protocol

(* ── test helpers ─────────────────────────────────────── *)

(** Send JSON lines to a server, collect all output lines (responses + notifications). *)
let run_server_with server input_lines =
  Eio_main.run @@ fun _env ->
  let input_str = String.concat "\n" input_lines ^ "\n" in
  let source = Eio.Flow.string_source input_str in
  let buf = Buffer.create 1024 in
  let sink = Eio.Flow.buffer_sink buf in
  Mcp_protocol_eio.Server.run server ~stdin:source ~stdout:sink ();
  let output = Buffer.contents buf in
  String.split_on_char '\n' output
  |> List.filter (fun s -> String.length (String.trim s) > 0)
  |> List.map (fun line -> Yojson.Safe.from_string line)

let get_field key json =
  match json with
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let get_string key json =
  match get_field key json with
  | Some (`String s) -> Some s
  | _ -> None

let get_result json = get_field "result" json
let get_error json = get_field "error" json

let assert_has_result json =
  match get_result json with
  | Some _ -> ()
  | None -> Alcotest.fail (Printf.sprintf "Expected result in: %s" (Yojson.Safe.to_string json))

let assert_has_error json =
  match get_error json with
  | Some _ -> ()
  | None -> Alcotest.fail (Printf.sprintf "Expected error in: %s" (Yojson.Safe.to_string json))

(** Partition output lines into responses (has "id") and notifications (has "method", no "id"). *)
let partition_output lines =
  List.partition (fun json ->
    get_field "id" json <> None
  ) lines

(* ── echo tool setup ──────────────────────────────────── *)

let echo_tool =
  Mcp_types.make_tool
    ~name:"echo"
    ~description:"Echoes back the input text"
    ~input_schema:(`Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("text", `Assoc [("type", `String "string")])
      ]);
      ("required", `List [`String "text"])
    ])
    ()

let echo_handler _ctx _name arguments =
  let text = match arguments with
    | Some (`Assoc args) ->
      begin match List.assoc_opt "text" args with
      | Some (`String s) -> s
      | _ -> "(no text)"
      end
    | _ -> "(no args)"
  in
  Ok (Mcp_types.tool_result_of_text (Printf.sprintf "Echo: %s" text))

let fail_handler _ctx _name _arguments =
  Error "Something went wrong"

let fail_tool =
  Mcp_types.make_tool ~name:"fail" ~description:"Always fails" ()

(* Tool handler that uses context to send a log *)
let logging_tool =
  Mcp_types.make_tool ~name:"log-test" ~description:"Tests logging" ()

let logging_tool_handler (ctx : Mcp_protocol_eio.Server.context) _name _arguments =
  let _ = ctx.send_log Logging.Info "Tool executed" in
  Ok (Mcp_types.tool_result_of_text "logged")

(* ── resource setup ───────────────────────────────────── *)

let test_resource =
  Mcp_types.make_resource
    ~uri:"file:///test.txt"
    ~name:"test.txt"
    ~description:"A test file"
    ~mime_type:"text/plain"
    ()

let resource_handler _ctx _uri =
  Ok [Mcp_types.{ uri = "file:///test.txt"; mime_type = Some "text/plain";
                   text = Some "Hello from resource"; blob = None }]

(* ── prompt setup ─────────────────────────────────────── *)

let greeting_prompt =
  Mcp_types.make_prompt
    ~name:"greeting"
    ~description:"Generate a greeting"
    ~arguments:[Mcp_types.{ name = "name"; description = Some "Who to greet"; required = Some true }]
    ()

let prompt_handler _ctx _name args =
  let who = match List.assoc_opt "name" args with
    | Some n -> n
    | None -> "world"
  in
  Ok Mcp_types.{
    description = Some "A greeting";
    messages = [{
      role = User;
      content = PromptText { type_ = "text"; text = Printf.sprintf "Hello, %s" who };
    }];
    _meta = None;
  }

(* ── completion setup ─────────────────────────────────── *)

let test_completion_handler ref_ arg_name _arg_value ~context:_context =
  match ref_ with
  | Mcp_types.Prompt_ref { name } ->
    if name = "greeting" && arg_name = "name" then
      Mcp_types.make_completion_result ~values:["Vincent"; "Victor"; "Vivian"] ()
    else
      Mcp_types.make_completion_result ~values:[] ()
  | Mcp_types.Resource_ref _ ->
    Mcp_types.make_completion_result ~values:[] ()

(* ── server builders ──────────────────────────────────── *)

let basic_server () =
  Mcp_protocol_eio.Server.create ~name:"test-server" ~version:"1.0.0" ()
  |> Mcp_protocol_eio.Server.add_tool echo_tool echo_handler
  |> Mcp_protocol_eio.Server.add_tool fail_tool fail_handler

let full_server () =
  basic_server ()
  |> Mcp_protocol_eio.Server.add_resource test_resource resource_handler
  |> Mcp_protocol_eio.Server.add_prompt greeting_prompt prompt_handler

let logging_server () =
  Mcp_protocol_eio.Server.create ~name:"log-server" ~version:"1.0.0" ()
  |> Mcp_protocol_eio.Server.add_tool logging_tool logging_tool_handler

let completion_server () =
  full_server ()
  |> Mcp_protocol_eio.Server.add_completion_handler test_completion_handler

(* ── initialize tests ─────────────────────────────────── *)

let test_initialize () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}|}]
  in
  Alcotest.(check int) "one response" 1 (List.length responses);
  let json = List.hd responses in
  assert_has_result json;
  match get_result json with
  | Some result ->
    begin match get_field "serverInfo" result with
    | Some info ->
      Alcotest.(check (option string)) "server name"
        (Some "test-server") (get_string "name" info)
    | None -> Alcotest.fail "Missing serverInfo"
    end
  | None -> Alcotest.fail "unreachable"

let test_initialize_version_negotiation () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}|}]
  in
  let json = List.hd responses in
  match get_result json with
  | Some result ->
    begin match get_string "protocolVersion" result with
    | Some v ->
      Alcotest.(check bool) "valid version" true (Version.is_supported v)
    | None -> Alcotest.fail "Missing protocolVersion"
    end
  | None -> Alcotest.fail "Expected result"

let test_initialize_capabilities_reflect_tools () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}|}]
  in
  let json = List.hd responses in
  match get_result json with
  | Some result ->
    begin match get_field "capabilities" result with
    | Some caps ->
      Alcotest.(check bool) "has tools" true
        (get_field "tools" caps <> None);
      Alcotest.(check bool) "no resources" true
        (get_field "resources" caps = None)
    | None -> Alcotest.fail "Missing capabilities"
    end
  | None -> Alcotest.fail "Expected result"

let test_initialize_has_logging_cap () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}|}]
  in
  let json = List.hd responses in
  match get_result json with
  | Some result ->
    begin match get_field "capabilities" result with
    | Some caps ->
      Alcotest.(check bool) "has logging" true
        (get_field "logging" caps <> None)
    | None -> Alcotest.fail "Missing capabilities"
    end
  | None -> Alcotest.fail "Expected result"

let test_initialize_without_logging_cap () =
  let server =
    Mcp_protocol_eio.Server.create
      ~name:"no-log-server"
      ~version:"1.0.0"
      ~enable_logging:false
      ()
    |> Mcp_protocol_eio.Server.add_tool echo_tool echo_handler
  in
  let responses = run_server_with server
    [{|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}|}]
  in
  let json = List.hd responses in
  match get_result json with
  | Some result ->
    begin match get_field "capabilities" result with
    | Some caps ->
      Alcotest.(check bool) "logging omitted" true
        (get_field "logging" caps = None)
    | None -> Alcotest.fail "Missing capabilities"
    end
  | None -> Alcotest.fail "Expected result"

(* ── ping tests ───────────────────────────────────────── *)

let test_ping () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"ping"}|}]
  in
  Alcotest.(check int) "one response" 1 (List.length responses);
  assert_has_result (List.hd responses)

(* ── tools tests ──────────────────────────────────────── *)

let test_tools_list () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"tools/list"}|}]
  in
  let json = List.hd responses in
  match get_result json with
  | Some result ->
    begin match get_field "tools" result with
    | Some (`List tools) ->
      Alcotest.(check int) "two tools" 2 (List.length tools)
    | _ -> Alcotest.fail "Missing tools array"
    end
  | None -> Alcotest.fail "Expected result"

let test_tools_call_success () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"echo","arguments":{"text":"hello"}}}|}]
  in
  let json = List.hd responses in
  assert_has_result json;
  match get_result json with
  | Some result ->
    begin match get_field "content" result with
    | Some (`List (first :: _)) ->
      begin match get_string "text" first with
      | Some t ->
        Alcotest.(check bool) "contains echo" true
          (String.length t > 0 && String.sub t 0 5 = "Echo:")
      | None -> Alcotest.fail "No text field"
      end
    | _ -> Alcotest.fail "Missing content"
    end
  | None -> Alcotest.fail "Expected result"

let test_tools_call_unknown_tool () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"nonexistent","arguments":{}}}|}]
  in
  assert_has_error (List.hd responses)

let test_tools_call_handler_error () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"fail","arguments":{}}}|}]
  in
  let json = List.hd responses in
  assert_has_result json;
  match get_result json with
  | Some result ->
    begin match get_field "isError" result with
    | Some (`Bool true) -> ()
    | _ -> Alcotest.fail "Expected isError=true for failed handler"
    end
  | None -> Alcotest.fail "Expected result"

let test_tools_call_missing_name () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"arguments":{}}}|}]
  in
  assert_has_error (List.hd responses)

(* ── resources tests ──────────────────────────────────── *)

let test_resources_list () =
  let responses = run_server_with (full_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"resources/list"}|}]
  in
  let json = List.hd responses in
  match get_result json with
  | Some result ->
    begin match get_field "resources" result with
    | Some (`List resources) ->
      Alcotest.(check int) "one resource" 1 (List.length resources)
    | _ -> Alcotest.fail "Missing resources array"
    end
  | None -> Alcotest.fail "Expected result"

let test_resources_read () =
  let responses = run_server_with (full_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"file:///test.txt"}}|}]
  in
  let json = List.hd responses in
  assert_has_result json;
  match get_result json with
  | Some result ->
    begin match get_field "contents" result with
    | Some (`List (first :: _)) ->
      Alcotest.(check (option string)) "has text"
        (Some "Hello from resource") (get_string "text" first)
    | _ -> Alcotest.fail "Missing contents"
    end
  | None -> Alcotest.fail "Expected result"

let test_resources_read_unknown () =
  let responses = run_server_with (full_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"file:///nonexistent"}}|}]
  in
  assert_has_error (List.hd responses)

(* ── prompts tests ────────────────────────────────────── *)

let test_prompts_list () =
  let responses = run_server_with (full_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"prompts/list"}|}]
  in
  let json = List.hd responses in
  match get_result json with
  | Some result ->
    begin match get_field "prompts" result with
    | Some (`List prompts) ->
      Alcotest.(check int) "one prompt" 1 (List.length prompts)
    | _ -> Alcotest.fail "Missing prompts array"
    end
  | None -> Alcotest.fail "Expected result"

let test_prompts_get () =
  let responses = run_server_with (full_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"prompts/get","params":{"name":"greeting","arguments":{"name":"Vincent"}}}|}]
  in
  let json = List.hd responses in
  assert_has_result json;
  match get_result json with
  | Some result ->
    begin match get_field "messages" result with
    | Some (`List (first :: _)) ->
      begin match get_field "content" first with
      | Some (`Assoc _ as obj) ->
        begin match get_string "text" obj with
        | Some t ->
          Alcotest.(check bool) "contains name" true
            (let len = String.length t in len >= 7 &&
              let sub = String.sub t (len - 7) 7 in sub = "Vincent")
        | None -> Alcotest.fail "No text in content"
        end
      | Some other ->
        Alcotest.fail (Printf.sprintf "Unexpected content shape: %s"
          (Yojson.Safe.to_string other))
      | None -> Alcotest.fail "No content in message"
      end
    | _ -> Alcotest.fail "Missing messages"
    end
  | None -> Alcotest.fail "Expected result"

let test_prompts_get_unknown () =
  let responses = run_server_with (full_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"prompts/get","params":{"name":"nonexistent"}}|}]
  in
  assert_has_error (List.hd responses)

(* ── unknown method test ──────────────────────────────── *)

let test_unknown_method () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"custom/nonexistent"}|}]
  in
  assert_has_error (List.hd responses)

(* ── notification test (no response) ──────────────────── *)

let test_notification_no_response () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","method":"notifications/initialized"}|}]
  in
  Alcotest.(check int) "no response" 0 (List.length responses)

(* ── multi-message test ───────────────────────────────── *)

let test_multi_message_sequence () =
  let responses = run_server_with (basic_server ()) [
    {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}|};
    {|{"jsonrpc":"2.0","method":"notifications/initialized"}|};
    {|{"jsonrpc":"2.0","id":2,"method":"ping"}|};
    {|{"jsonrpc":"2.0","id":3,"method":"tools/list"}|};
    {|{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"echo","arguments":{"text":"world"}}}|};
  ] in
  Alcotest.(check int) "four responses" 4 (List.length responses)

(* ── empty server tests ───────────────────────────────── *)

let test_empty_server_no_tools_cap () =
  let empty = Mcp_protocol_eio.Server.create ~name:"empty" ~version:"1.0" () in
  let responses = run_server_with empty
    [{|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}|}]
  in
  let json = List.hd responses in
  match get_result json with
  | Some result ->
    begin match get_field "capabilities" result with
    | Some caps ->
      Alcotest.(check bool) "no tools cap" true (get_field "tools" caps = None);
      Alcotest.(check bool) "no resources cap" true (get_field "resources" caps = None);
      Alcotest.(check bool) "no prompts cap" true (get_field "prompts" caps = None)
    | None -> Alcotest.fail "Missing capabilities"
    end
  | None -> Alcotest.fail "Expected result"

(* ── logging/setLevel tests ──────────────────────────── *)

let test_logging_set_level () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"logging/setLevel","params":{"level":"debug"}}|}]
  in
  Alcotest.(check int) "one response" 1 (List.length responses);
  assert_has_result (List.hd responses)

let test_logging_set_level_invalid () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"logging/setLevel","params":{"level":"bogus"}}|}]
  in
  assert_has_error (List.hd responses)

let test_logging_set_level_missing_params () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"logging/setLevel"}|}]
  in
  assert_has_error (List.hd responses)

let test_logging_notification_sent () =
  let all_output = run_server_with (logging_server ()) [
    {|{"jsonrpc":"2.0","id":1,"method":"logging/setLevel","params":{"level":"debug"}}|};
    {|{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"log-test","arguments":{}}}|};
  ] in
  let responses, notifications = partition_output all_output in
  Alcotest.(check int) "two responses" 2 (List.length responses);
  Alcotest.(check bool) "has notification" true (List.length notifications > 0);
  let notif = List.hd notifications in
  Alcotest.(check (option string)) "notification method"
    (Some "notifications/message") (get_string "method" notif)

let test_logging_notification_filtered () =
  (* Default log level is Warning; Info log should be filtered *)
  let all_output = run_server_with (logging_server ()) [
    {|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"log-test","arguments":{}}}|};
  ] in
  let _responses, notifications = partition_output all_output in
  Alcotest.(check int) "no notification (filtered)" 0 (List.length notifications)

(* ── completion/complete tests ───────────────────────── *)

let test_completion_complete () =
  let responses = run_server_with (completion_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"completion/complete","params":{"ref":{"type":"ref/prompt","name":"greeting"},"argument":{"name":"name","value":"Vi"}}}|}]
  in
  let json = List.hd responses in
  assert_has_result json;
  match get_result json with
  | Some result ->
    begin match get_field "completion" result with
    | Some completion ->
      begin match get_field "values" completion with
      | Some (`List values) ->
        Alcotest.(check int) "three values" 3 (List.length values)
      | _ -> Alcotest.fail "Missing values array"
      end
    | None -> Alcotest.fail "Missing completion field"
    end
  | None -> Alcotest.fail "Expected result"

let test_completion_no_handler () =
  let responses = run_server_with (basic_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"completion/complete","params":{"ref":{"type":"ref/prompt","name":"x"},"argument":{"name":"n","value":"v"}}}|}]
  in
  assert_has_error (List.hd responses)

let test_completion_invalid_params () =
  let responses = run_server_with (completion_server ())
    [{|{"jsonrpc":"2.0","id":1,"method":"completion/complete","params":{"bad":"data"}}|}]
  in
  assert_has_error (List.hd responses)

(* ── test suite ───────────────────────────────────────── *)

let () =
  Alcotest.run "Server" [
    "initialize", [
      Alcotest.test_case "basic" `Quick test_initialize;
      Alcotest.test_case "version negotiation" `Quick test_initialize_version_negotiation;
      Alcotest.test_case "capabilities reflect tools" `Quick test_initialize_capabilities_reflect_tools;
      Alcotest.test_case "has logging capability" `Quick test_initialize_has_logging_cap;
      Alcotest.test_case "can disable logging capability" `Quick test_initialize_without_logging_cap;
    ];
    "ping", [
      Alcotest.test_case "responds" `Quick test_ping;
    ];
    "tools", [
      Alcotest.test_case "list" `Quick test_tools_list;
      Alcotest.test_case "call success" `Quick test_tools_call_success;
      Alcotest.test_case "call unknown tool" `Quick test_tools_call_unknown_tool;
      Alcotest.test_case "call handler error" `Quick test_tools_call_handler_error;
      Alcotest.test_case "call missing name" `Quick test_tools_call_missing_name;
    ];
    "resources", [
      Alcotest.test_case "list" `Quick test_resources_list;
      Alcotest.test_case "read" `Quick test_resources_read;
      Alcotest.test_case "read unknown" `Quick test_resources_read_unknown;
    ];
    "prompts", [
      Alcotest.test_case "list" `Quick test_prompts_list;
      Alcotest.test_case "get" `Quick test_prompts_get;
      Alcotest.test_case "get unknown" `Quick test_prompts_get_unknown;
    ];
    "dispatch", [
      Alcotest.test_case "unknown method" `Quick test_unknown_method;
      Alcotest.test_case "notification no response" `Quick test_notification_no_response;
      Alcotest.test_case "multi-message sequence" `Quick test_multi_message_sequence;
    ];
    "empty server", [
      Alcotest.test_case "no capabilities" `Quick test_empty_server_no_tools_cap;
    ];
    "logging", [
      Alcotest.test_case "set level" `Quick test_logging_set_level;
      Alcotest.test_case "set level invalid" `Quick test_logging_set_level_invalid;
      Alcotest.test_case "set level missing params" `Quick test_logging_set_level_missing_params;
      Alcotest.test_case "notification sent" `Quick test_logging_notification_sent;
      Alcotest.test_case "notification filtered" `Quick test_logging_notification_filtered;
    ];
    "completion", [
      Alcotest.test_case "complete" `Quick test_completion_complete;
      Alcotest.test_case "no handler" `Quick test_completion_no_handler;
      Alcotest.test_case "invalid params" `Quick test_completion_invalid_params;
    ];
  ]
