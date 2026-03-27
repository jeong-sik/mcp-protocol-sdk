(** Tests for MCP Client. *)

open Mcp_protocol

(* ── test helpers ─────────────────────────────────────── *)

(** Run a client test with pre-built server responses.
    Returns (callback_result, list_of_json_messages_client_sent). *)
let run_client_test responses fn =
  Eio_main.run @@ fun _env ->
  let input_str =
    String.concat "\n" (List.map Yojson.Safe.to_string responses) ^ "\n"
  in
  let source = Eio.Flow.string_source input_str in
  let buf = Buffer.create 1024 in
  let sink = Eio.Flow.buffer_sink buf in
  let client = Mcp_protocol_eio.Client.create ~stdin:source ~stdout:sink () in
  let result = fn client in
  let output = Buffer.contents buf in
  let sent =
    String.split_on_char '\n' output
    |> List.filter (fun s -> String.length (String.trim s) > 0)
    |> List.map Yojson.Safe.from_string
  in
  (result, sent)

(** Like [run_client_test] but applies [setup] to the client before [fn].
    Use to register handlers via [on_sampling], [on_roots_list], etc. *)
let run_client_test_with responses ?(setup = Fun.id) fn =
  Eio_main.run @@ fun _env ->
  let input_str =
    String.concat "\n" (List.map Yojson.Safe.to_string responses) ^ "\n"
  in
  let source = Eio.Flow.string_source input_str in
  let buf = Buffer.create 1024 in
  let sink = Eio.Flow.buffer_sink buf in
  let client = Mcp_protocol_eio.Client.create ~stdin:source ~stdout:sink () in
  let client = setup client in
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
          ("content", `Assoc [
            ("type", `String "text");
            ("text", `String "Hello, world!");
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

(* ── mock server requests (server → client) ─────────── *)

let make_sampling_request ?(id = 99) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String "sampling/createMessage");
    ("params", `Assoc [
      ("messages", `List [
        `Assoc [
          ("role", `String "user");
          ("content", `Assoc [
            ("type", `String "text");
            ("text", `String "Hello");
          ]);
        ];
      ]);
      ("maxTokens", `Int 100);
    ]);
  ]

let make_roots_request ?(id = 88) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String "roots/list");
  ]

let make_elicitation_request ?(id = 77) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String "elicitation/create");
    ("params", `Assoc [
      ("message", `String "Confirm?");
    ]);
  ]

let make_server_notification ?(method_ = "notifications/message") ?params () =
  let fields = [
    ("jsonrpc", `String "2.0");
    ("method", `String method_);
  ] in
  let fields = match params with
    | Some p -> fields @ [("params", p)]
    | None -> fields
  in
  `Assoc fields

(* ── callback dispatch tests ────────────────────────── *)

let test_sampling_callback () =
  let handler_called = ref false in
  let setup client =
    Mcp_protocol_eio.Client.on_sampling (fun params ->
      handler_called := true;
      Alcotest.(check int) "max_tokens" 100 params.max_tokens;
      Ok Sampling.{
        role = Assistant;
        content = Text { type_ = "text"; text = "Reply" };
        model = "test-model";
        stop_reason = Some "endTurn";
        _meta = None;
      }
    ) client
  in
  let responses = [make_sampling_request (); make_pong ()] in
  let (result, sent) = run_client_test_with responses ~setup (fun client ->
    Mcp_protocol_eio.Client.ping client
  ) in
  Alcotest.(check bool) "handler called" true !handler_called;
  Alcotest.(check bool) "ping ok" true (Result.is_ok result);
  (* sent: ping request + sampling response *)
  Alcotest.(check int) "2 messages sent" 2 (List.length sent);
  let sampling_resp = List.nth sent 1 in
  let resp_result = get_field "result" sampling_resp in
  (match resp_result with
   | Some r ->
     Alcotest.(check string) "model" "test-model" (get_string "model" r)
   | None -> Alcotest.fail "Missing result in sampling response")

let test_roots_callback () =
  let setup client =
    Mcp_protocol_eio.Client.on_roots_list (fun () ->
      Ok [
        Mcp_types.{ uri = "file:///project"; name = Some "project" };
        Mcp_types.{ uri = "file:///home"; name = None };
      ]
    ) client
  in
  let responses = [make_roots_request (); make_pong ()] in
  let (result, sent) = run_client_test_with responses ~setup (fun client ->
    Mcp_protocol_eio.Client.ping client
  ) in
  Alcotest.(check bool) "ping ok" true (Result.is_ok result);
  Alcotest.(check int) "2 messages sent" 2 (List.length sent);
  let roots_resp = List.nth sent 1 in
  let resp_result = get_field "result" roots_resp in
  (match resp_result with
   | Some r ->
     (match get_field "roots" r with
      | Some (`List roots) ->
        Alcotest.(check int) "2 roots" 2 (List.length roots);
        Alcotest.(check string) "first uri" "file:///project"
          (get_string "uri" (List.nth roots 0))
      | _ -> Alcotest.fail "Missing roots array")
   | None -> Alcotest.fail "Missing result in roots response")

let test_elicitation_callback () =
  let setup client =
    Mcp_protocol_eio.Client.on_elicitation (fun params ->
      Alcotest.(check string) "message" "Confirm?" params.message;
      Ok Mcp_types.{ action = Accept; content = None }
    ) client
  in
  let responses = [make_elicitation_request (); make_pong ()] in
  let (result, sent) = run_client_test_with responses ~setup (fun client ->
    Mcp_protocol_eio.Client.ping client
  ) in
  Alcotest.(check bool) "ping ok" true (Result.is_ok result);
  Alcotest.(check int) "2 messages sent" 2 (List.length sent);
  let elic_resp = List.nth sent 1 in
  let resp_result = get_field "result" elic_resp in
  (match resp_result with
   | Some r ->
     Alcotest.(check string) "action" "accept" (get_string "action" r)
   | None -> Alcotest.fail "Missing result in elicitation response")

let test_unregistered_sampling () =
  (* No sampling handler — should get method_not_found error back *)
  let responses = [make_sampling_request (); make_pong ()] in
  let (result, sent) = run_client_test responses (fun client ->
    Mcp_protocol_eio.Client.ping client
  ) in
  Alcotest.(check bool) "ping ok" true (Result.is_ok result);
  Alcotest.(check int) "2 messages sent" 2 (List.length sent);
  let err_resp = List.nth sent 1 in
  let err = get_field "error" err_resp in
  (match err with
   | Some e ->
     Alcotest.(check (option int)) "error code" (Some (-32601)) (get_int "code" e)
   | None -> Alcotest.fail "Expected error response for unregistered sampling")

let test_unregistered_roots () =
  (* No roots handler — should get method_not_found error back *)
  let responses = [make_roots_request (); make_pong ()] in
  let (result, sent) = run_client_test responses (fun client ->
    Mcp_protocol_eio.Client.ping client
  ) in
  Alcotest.(check bool) "ping ok" true (Result.is_ok result);
  Alcotest.(check int) "2 messages sent" 2 (List.length sent);
  let err_resp = List.nth sent 1 in
  let err = get_field "error" err_resp in
  (match err with
   | Some e ->
     Alcotest.(check (option int)) "error code" (Some (-32601)) (get_int "code" e)
   | None -> Alcotest.fail "Expected error response for unregistered roots")

let test_notification_callback () =
  let received_method = ref "" in
  let received_params = ref `Null in
  let notif_params = `Assoc [("level", `String "info"); ("data", `String "test")] in
  let setup client =
    Mcp_protocol_eio.Client.on_notification (fun method_ params ->
      received_method := method_;
      received_params := (match params with Some p -> p | None -> `Null)
    ) client
  in
  let responses = [
    make_server_notification ~params:notif_params ();
    make_pong ();
  ] in
  let (result, _) = run_client_test_with responses ~setup (fun client ->
    Mcp_protocol_eio.Client.ping client
  ) in
  Alcotest.(check bool) "ping ok" true (Result.is_ok result);
  Alcotest.(check string) "notif method" "notifications/message" !received_method;
  Alcotest.(check string) "notif data"
    (Yojson.Safe.to_string notif_params)
    (Yojson.Safe.to_string !received_params)

let test_notification_no_callback () =
  (* No notification handler — notification should be silently ignored *)
  let responses = [make_notification (); make_pong ()] in
  let (result, _) = run_client_test responses (fun client ->
    Mcp_protocol_eio.Client.ping client
  ) in
  Alcotest.(check bool) "ping ok despite ignored notification" true (Result.is_ok result)

let test_request_interleaved_with_response () =
  let handler_called = ref false in
  let setup client =
    Mcp_protocol_eio.Client.on_sampling (fun _params ->
      handler_called := true;
      Ok Sampling.{
        role = Assistant;
        content = Text { type_ = "text"; text = "LLM reply" };
        model = "interleaved-model";
        stop_reason = None;
        _meta = None;
      }
    ) client
  in
  (* Server sends: sampling request, then tools/list response *)
  let responses = [
    make_sampling_request ~id:99 ();
    make_tools_list_response ~id:1 ();
  ] in
  let (result, sent) = run_client_test_with responses ~setup (fun client ->
    Mcp_protocol_eio.Client.list_tools client
  ) in
  Alcotest.(check bool) "handler called" true !handler_called;
  (match result with
   | Ok tools ->
     Alcotest.(check int) "1 tool" 1 (List.length tools);
     Alcotest.(check string) "tool name" "echo" (List.nth tools 0).name
   | Error e -> Alcotest.fail e);
  (* sent: list_tools request + sampling response *)
  Alcotest.(check int) "2 messages sent" 2 (List.length sent)

let test_capabilities_advertised () =
  let setup client =
    client
    |> Mcp_protocol_eio.Client.on_sampling (fun _params ->
         Error "unused")
    |> Mcp_protocol_eio.Client.on_roots_list (fun () ->
         Error "unused")
    |> Mcp_protocol_eio.Client.on_elicitation (fun _params ->
         Error "unused")
  in
  let responses = [make_init_response ()] in
  let (result, sent) = run_client_test_with responses ~setup (fun client ->
    Mcp_protocol_eio.Client.initialize client ~client_name:"test" ~client_version:"1.0"
  ) in
  Alcotest.(check bool) "init ok" true (Result.is_ok result);
  (* Check the initialize request params for capabilities *)
  let init_req = List.nth sent 0 in
  let params = get_field "params" init_req in
  (match params with
   | Some p ->
     let caps = get_field "capabilities" p in
     (match caps with
      | Some (`Assoc fields) ->
        Alcotest.(check bool) "has sampling" true
          (List.mem_assoc "sampling" fields);
        Alcotest.(check bool) "has roots" true
          (List.mem_assoc "roots" fields);
        Alcotest.(check bool) "has elicitation" true
          (List.mem_assoc "elicitation" fields)
      | _ -> Alcotest.fail "Missing capabilities object")
   | None -> Alcotest.fail "Missing params")

let test_capabilities_empty () =
  (* No handlers registered — capabilities should be empty *)
  let responses = [make_init_response ()] in
  let (_, sent) = run_client_test responses (fun client ->
    Mcp_protocol_eio.Client.initialize client ~client_name:"test" ~client_version:"1.0"
  ) in
  let init_req = List.nth sent 0 in
  let params = get_field "params" init_req in
  (match params with
   | Some p ->
     let caps = get_field "capabilities" p in
     (match caps with
      | Some (`Assoc fields) ->
        Alcotest.(check int) "empty capabilities" 0 (List.length fields)
      | _ -> Alcotest.fail "Missing capabilities object")
   | None -> Alcotest.fail "Missing params")

let test_client_timeout_eof () =
  Eio_main.run @@ fun env ->
  (* Empty source = immediate EOF, so connection closes before timeout *)
  let source = Eio.Flow.string_source "" in
  let buf = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink buf in
  let client = Mcp_protocol_eio.Client.create ~stdin:source ~stdout:sink
    ~clock:(Eio.Stdenv.clock env) () in
  match Mcp_protocol_eio.Client.ping client with
  | Error msg ->
    Alcotest.(check bool) "has error message" true (String.length msg > 0)
  | Ok () -> Alcotest.fail "Expected error from empty source"

(* ── cancellation tests ───────────────────────────────── *)

let test_client_receives_cancellation () =
  (* Server sends a cancellation notification for request id 1, then EOF.
     Client.ping sends a request with id=1 (first request), so the
     cancellation notification targets that in-flight request. *)
  let cancel_notification =
    `Assoc [
      ("jsonrpc", `String "2.0");
      ("method", `String "notifications/cancelled");
      ("params", `Assoc [
        ("requestId", `Int 1);
        ("reason", `String "Server busy");
      ]);
    ]
  in
  let (result, _) = run_client_test [cancel_notification] (fun client ->
    Mcp_protocol_eio.Client.ping client
  ) in
  match result with
  | Error msg ->
    let contains ~sub s =
      let len_sub = String.length sub in
      let len_s = String.length s in
      if len_sub > len_s then false
      else
        let rec check i =
          if i > len_s - len_sub then false
          else if String.sub s i len_sub = sub then true
          else check (i + 1)
        in check 0
    in
    Alcotest.(check bool) "mentions cancelled or reason" true
      (contains ~sub:"cancelled" msg || contains ~sub:"Server busy" msg)
  | Ok _ -> Alcotest.fail "Expected cancellation error"

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
      Alcotest.test_case "timeout with clock (eof)" `Quick test_client_timeout_eof;
    ];
    "request ids", [
      Alcotest.test_case "increment" `Quick test_id_increment;
    ];
    "callback dispatch", [
      Alcotest.test_case "sampling callback" `Quick test_sampling_callback;
      Alcotest.test_case "roots callback" `Quick test_roots_callback;
      Alcotest.test_case "elicitation callback" `Quick test_elicitation_callback;
      Alcotest.test_case "unregistered sampling" `Quick test_unregistered_sampling;
      Alcotest.test_case "unregistered roots" `Quick test_unregistered_roots;
      Alcotest.test_case "notification callback" `Quick test_notification_callback;
      Alcotest.test_case "notification no callback" `Quick test_notification_no_callback;
      Alcotest.test_case "interleaved request" `Quick test_request_interleaved_with_response;
    ];
    "capabilities", [
      Alcotest.test_case "advertised" `Quick test_capabilities_advertised;
      Alcotest.test_case "empty" `Quick test_capabilities_empty;
    ];
    "cancellation", [
      Alcotest.test_case "receives_cancellation" `Quick test_client_receives_cancellation;
    ];
  ]
