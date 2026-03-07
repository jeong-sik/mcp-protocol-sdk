(** Tests for MCP Client. *)

open Mcp_protocol

(* ── test helpers ─────────────────────────────────────── *)

(** Run a client test with pre-built server responses.
    Returns (callback_result, list_of_json_messages_client_sent). *)
let run_client_test responses fn =
  Eio_main.run @@ fun env ->
  let input_str =
    String.concat "\n" (List.map Yojson.Safe.to_string responses) ^ "\n"
  in
  let source = Eio.Flow.string_source input_str in
  let buf = Buffer.create 1024 in
  let sink = Eio.Flow.buffer_sink buf in
  let clock = Eio.Stdenv.clock env in
  let client = Mcp_protocol_eio.Client.create ~clock ~stdin:source ~stdout:sink () in
  let result = fn client in
  let output = Buffer.contents buf in
  let sent =
    String.split_on_char '\n' output
    |> List.filter (fun s -> String.length (String.trim s) > 0)
    |> List.map Yojson.Safe.from_string
  in
  (result, sent)

let get_field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let get_string key json =
  match get_field key json with
  | Some (`String s) -> s
  | _ -> ""

let get_int key json =
  match get_field key json with
  | Some (`Int i) -> Some i
  | _ -> None

(* ── mock responses ──────────────────────────────────── *)

let make_init_response ?(id = 1) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("result", `Assoc [
      ("protocolVersion", `String Version.latest);
      ("capabilities", `Assoc []);
      ("serverInfo", `Assoc [
        ("name", `String "test-server");
        ("version", `String "1.0");
      ]);
    ]);
  ]

let make_pong ?(id = 1) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("result", `Assoc []);
  ]

let make_tools_list_response ?(id = 1) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("result", `Assoc [
      ("tools", `List [
        `Assoc [
          ("name", `String "echo");
          ("description", `String "Echo tool");
          ("inputSchema", `Assoc [("type", `String "object")]);
        ];
      ]);
    ]);
  ]

let make_tool_call_response ?(id = 1) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("result", `Assoc [
      ("content", `List [
        `Assoc [
          ("type", `String "text");
          ("text", `String "Echo: hello");
        ];
      ]);
    ]);
  ]

let make_error_response ?(id = 1) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("error", `Assoc [
      ("code", `Int (-32600));
      ("message", `String "Something went wrong");
    ]);
  ]

let make_resources_list_response ?(id = 1) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("result", `Assoc [
      ("resources", `List [
        `Assoc [
          ("uri", `String "file:///test.txt");
          ("name", `String "test.txt");
        ];
      ]);
    ]);
  ]

let make_resource_read_response ?(id = 1) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("result", `Assoc [
      ("contents", `List [
        `Assoc [
          ("uri", `String "file:///test.txt");
          ("text", `String "file contents");
        ];
      ]);
    ]);
  ]

let make_prompts_list_response ?(id = 1) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("result", `Assoc [
      ("prompts", `List [
        `Assoc [
          ("name", `String "greet");
        ];
      ]);
    ]);
  ]

let make_prompt_get_response ?(id = 1) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("result", `Assoc [
      ("messages", `List [
        `Assoc [
          ("role", `String "assistant");
          ("content", `List [
            `String "PromptText";
            `Assoc [
              ("type_", `String "text");
              ("text", `String "Hello, world!");
            ];
          ]);
        ];
      ]);
    ]);
  ]

let make_notification () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "notifications/message");
    ("params", `Assoc [
      ("level", `String "info");
      ("data", `String "log message");
    ]);
  ]

(* ── tests ───────────────────────────────────────────── *)

let test_initialize () =
  let (result, sent) = run_client_test [make_init_response ()] (fun client ->
    Mcp_protocol_eio.Client.initialize client ~client_name:"test" ~client_version:"1.0"
  ) in
  begin match result with
  | Ok r ->
    Alcotest.(check string) "server name" "test-server" r.server_info.name;
    Alcotest.(check string) "server version" "1.0" r.server_info.version;
    Alcotest.(check string) "protocol version" Version.latest r.protocol_version
  | Error e -> Alcotest.fail e
  end;
  (* Should have sent: initialize request + initialized notification *)
  Alcotest.(check int) "2 messages sent" 2 (List.length sent);
  Alcotest.(check string) "init method" "initialize" (get_string "method" (List.nth sent 0));
  Alcotest.(check string) "initialized notif" "notifications/initialized"
    (get_string "method" (List.nth sent 1))

let test_initialize_sends_version () =
  let (_, sent) = run_client_test [make_init_response ()] (fun client ->
    Mcp_protocol_eio.Client.initialize client ~client_name:"test" ~client_version:"1.0"
  ) in
  let req = List.nth sent 0 in
  let params = get_field "params" req in
  match params with
  | Some p ->
    Alcotest.(check string) "protocol version in params" Version.latest
      (get_string "protocolVersion" p)
  | None -> Alcotest.fail "Missing params"

let test_ping () =
  let (result, sent) = run_client_test [make_pong ()] (fun client ->
    Mcp_protocol_eio.Client.ping client
  ) in
  Alcotest.(check bool) "ping ok" true (Result.is_ok result);
  Alcotest.(check int) "1 message sent" 1 (List.length sent);
  Alcotest.(check string) "ping method" "ping" (get_string "method" (List.nth sent 0))

let test_list_tools () =
  let (result, _) = run_client_test [make_tools_list_response ()] (fun client ->
    Mcp_protocol_eio.Client.list_tools client
  ) in
  match result with
  | Ok tools ->
    Alcotest.(check int) "1 tool" 1 (List.length tools);
    Alcotest.(check string) "tool name" "echo" (List.nth tools 0).name
  | Error e -> Alcotest.fail e

let test_call_tool () =
  let (result, sent) = run_client_test [make_tool_call_response ()] (fun client ->
    Mcp_protocol_eio.Client.call_tool client ~name:"echo"
      ~arguments:(`Assoc [("text", `String "hello")]) ()
  ) in
  begin match result with
  | Ok r ->
    Alcotest.(check int) "1 content" 1 (List.length r.content);
    begin match List.nth r.content 0 with
    | Mcp_types.TextContent t ->
      Alcotest.(check string) "text" "Echo: hello" t.text
    | _ -> Alcotest.fail "Expected TextContent"
    end
  | Error e -> Alcotest.fail e
  end;
  let req = List.nth sent 0 in
  Alcotest.(check string) "method" "tools/call" (get_string "method" req);
  let params = get_field "params" req in
  match params with
  | Some p ->
    Alcotest.(check string) "tool name" "echo" (get_string "name" p)
  | None -> Alcotest.fail "Missing params"

let test_call_tool_error () =
  let (result, _) = run_client_test [make_error_response ()] (fun client ->
    Mcp_protocol_eio.Client.call_tool client ~name:"fail" ()
  ) in
  match result with
  | Ok _ -> Alcotest.fail "Expected error"
  | Error msg ->
    Alcotest.(check string) "error message" "Something went wrong" msg

let test_list_resources () =
  let (result, _) = run_client_test [make_resources_list_response ()] (fun client ->
    Mcp_protocol_eio.Client.list_resources client
  ) in
  match result with
  | Ok resources ->
    Alcotest.(check int) "1 resource" 1 (List.length resources);
    Alcotest.(check string) "uri" "file:///test.txt" (List.nth resources 0).uri
  | Error e -> Alcotest.fail e

let test_read_resource () =
  let (result, sent) = run_client_test [make_resource_read_response ()] (fun client ->
    Mcp_protocol_eio.Client.read_resource client ~uri:"file:///test.txt"
  ) in
  begin match result with
  | Ok contents ->
    Alcotest.(check int) "1 content" 1 (List.length contents);
    let r = List.nth contents 0 in
    Alcotest.(check string) "uri" "file:///test.txt" r.Mcp_types.uri;
    Alcotest.(check string) "text" "file contents"
      (Option.value ~default:"" r.Mcp_types.text)
  | Error e -> Alcotest.fail e
  end;
  let req = List.nth sent 0 in
  let params = get_field "params" req in
  match params with
  | Some p ->
    Alcotest.(check string) "uri param" "file:///test.txt" (get_string "uri" p)
  | None -> Alcotest.fail "Missing params"

let test_list_prompts () =
  let (result, _) = run_client_test [make_prompts_list_response ()] (fun client ->
    Mcp_protocol_eio.Client.list_prompts client
  ) in
  match result with
  | Ok prompts ->
    Alcotest.(check int) "1 prompt" 1 (List.length prompts);
    Alcotest.(check string) "name" "greet" (List.nth prompts 0).name
  | Error e -> Alcotest.fail e

let test_get_prompt () =
  let (result, sent) = run_client_test [make_prompt_get_response ()] (fun client ->
    Mcp_protocol_eio.Client.get_prompt client ~name:"greet"
      ~arguments:[("name", "world")] ()
  ) in
  begin match result with
  | Ok r ->
    Alcotest.(check int) "1 message" 1 (List.length r.messages)
  | Error e -> Alcotest.fail e
  end;
  let req = List.nth sent 0 in
  Alcotest.(check string) "method" "prompts/get" (get_string "method" req);
  let params = get_field "params" req in
  match params with
  | Some p ->
    Alcotest.(check string) "prompt name" "greet" (get_string "name" p)
  | None -> Alcotest.fail "Missing params"

let test_connection_closed () =
  let (result, _) = run_client_test [] (fun client ->
    Mcp_protocol_eio.Client.ping client
  ) in
  match result with
  | Ok _ -> Alcotest.fail "Expected error on closed connection"
  | Error msg ->
    Alcotest.(check bool) "has error" true (String.length msg > 0)

let test_skip_notifications () =
  (* Server sends a notification before the actual response *)
  let responses = [make_notification (); make_pong ()] in
  let (result, _) = run_client_test responses (fun client ->
    Mcp_protocol_eio.Client.ping client
  ) in
  Alcotest.(check bool) "ping ok despite notification" true (Result.is_ok result)

let test_id_increment () =
  (* Two requests: first gets id=1, second gets id=2 *)
  let responses = [make_pong ~id:1 (); make_pong ~id:2 ()] in
  let (_, sent) = run_client_test responses (fun client ->
    let _ = Mcp_protocol_eio.Client.ping client in
    let _ = Mcp_protocol_eio.Client.ping client in
    ()
  ) in
  Alcotest.(check int) "2 requests sent" 2 (List.length sent);
  Alcotest.(check (option int)) "first id" (Some 1) (get_int "id" (List.nth sent 0));
  Alcotest.(check (option int)) "second id" (Some 2) (get_int "id" (List.nth sent 1))

let test_call_tool_no_arguments () =
  let (_, sent) = run_client_test [make_tool_call_response ()] (fun client ->
    let _ = Mcp_protocol_eio.Client.call_tool client ~name:"no-args" () in
    ()
  ) in
  let req = List.nth sent 0 in
  let params = get_field "params" req in
  match params with
  | Some p ->
    Alcotest.(check string) "tool name" "no-args" (get_string "name" p);
    (* Should not have "arguments" field *)
    begin match get_field "arguments" p with
    | None -> ()
    | Some _ -> Alcotest.fail "Should not have arguments field"
    end
  | None -> Alcotest.fail "Missing params"

let test_get_prompt_no_arguments () =
  let (_, sent) = run_client_test [make_prompt_get_response ()] (fun client ->
    let _ = Mcp_protocol_eio.Client.get_prompt client ~name:"simple" () in
    ()
  ) in
  let req = List.nth sent 0 in
  let params = get_field "params" req in
  match params with
  | Some p ->
    Alcotest.(check string) "prompt name" "simple" (get_string "name" p);
    begin match get_field "arguments" p with
    | None -> ()
    | Some _ -> Alcotest.fail "Should not have arguments field"
    end
  | None -> Alcotest.fail "Missing params"

(* ── test suite ──────────────────────────────────────── *)

let () =
  Alcotest.run "Client" [
    "initialize", [
      Alcotest.test_case "basic" `Quick test_initialize;
      Alcotest.test_case "sends version" `Quick test_initialize_sends_version;
    ];
    "ping", [
      Alcotest.test_case "responds" `Quick test_ping;
    ];
    "tools", [
      Alcotest.test_case "list" `Quick test_list_tools;
      Alcotest.test_case "call" `Quick test_call_tool;
      Alcotest.test_case "call error" `Quick test_call_tool_error;
      Alcotest.test_case "call no arguments" `Quick test_call_tool_no_arguments;
    ];
    "resources", [
      Alcotest.test_case "list" `Quick test_list_resources;
      Alcotest.test_case "read" `Quick test_read_resource;
    ];
    "prompts", [
      Alcotest.test_case "list" `Quick test_list_prompts;
      Alcotest.test_case "get" `Quick test_get_prompt;
      Alcotest.test_case "get no arguments" `Quick test_get_prompt_no_arguments;
    ];
    "error handling", [
      Alcotest.test_case "connection closed" `Quick test_connection_closed;
      Alcotest.test_case "skip notifications" `Quick test_skip_notifications;
    ];
    "request ids", [
      Alcotest.test_case "increment" `Quick test_id_increment;
    ];
  ]
