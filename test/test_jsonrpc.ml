open Mcp_protocol

let json = Alcotest.testable
  (fun fmt j -> Format.pp_print_string fmt (Yojson.Safe.to_string j))
  (fun a b -> Yojson.Safe.equal a b)

let id_testable = Alcotest.testable
  (fun fmt id -> Format.pp_print_string fmt
    (Yojson.Safe.to_string (Jsonrpc.id_to_yojson id)))
  (fun a b -> Jsonrpc.id_to_yojson a = Jsonrpc.id_to_yojson b)

(* --- id round-trip --- *)

let test_id_string_roundtrip () =
  let id = Jsonrpc.String "abc-123" in
  let j = Jsonrpc.id_to_yojson id in
  Alcotest.(check (result id_testable string))
    "string id round-trip"
    (Ok id) (Jsonrpc.id_of_yojson j)

let test_id_int_roundtrip () =
  let id = Jsonrpc.Int 42 in
  let j = Jsonrpc.id_to_yojson id in
  Alcotest.(check (result id_testable string))
    "int id round-trip"
    (Ok id) (Jsonrpc.id_of_yojson j)

let test_id_null_roundtrip () =
  let id = Jsonrpc.Null in
  let j = Jsonrpc.id_to_yojson id in
  Alcotest.(check (result id_testable string))
    "null id round-trip"
    (Ok id) (Jsonrpc.id_of_yojson j)

let test_id_invalid () =
  let j = `Bool true in
  Alcotest.(check bool)
    "invalid id returns Error"
    true
    (Result.is_error (Jsonrpc.id_of_yojson j))

(* --- request round-trip --- *)

let test_request_roundtrip () =
  let req : Jsonrpc.request = {
    jsonrpc = "2.0";
    id = Int 1;
    method_ = "tools/list";
    params = None;
  } in
  let j = Jsonrpc.request_to_yojson req in
  match Jsonrpc.request_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "jsonrpc" "2.0" decoded.jsonrpc;
    Alcotest.(check string) "method" "tools/list" decoded.method_;
    Alcotest.(check (option pass)) "params" None decoded.params
  | Error e -> Alcotest.fail e

let test_request_with_params () =
  let params = `Assoc [("name", `String "my_tool")] in
  let req : Jsonrpc.request = {
    jsonrpc = "2.0";
    id = String "req-1";
    method_ = "tools/call";
    params = Some params;
  } in
  let j = Jsonrpc.request_to_yojson req in
  match Jsonrpc.request_of_yojson j with
  | Ok decoded ->
    Alcotest.(check (option json)) "params present"
      (Some params) decoded.params
  | Error e -> Alcotest.fail e

(* --- notification --- *)

let test_notification_roundtrip () =
  let notif : Jsonrpc.notification = {
    jsonrpc = "2.0";
    method_ = "notifications/initialized";
    params = None;
  } in
  let j = Jsonrpc.notification_to_yojson notif in
  match Jsonrpc.notification_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "method" "notifications/initialized" decoded.method_
  | Error e -> Alcotest.fail e

(* --- response --- *)

let test_response_roundtrip () =
  let resp : Jsonrpc.response = {
    jsonrpc = "2.0";
    id = Int 1;
    result = `Assoc [("tools", `List [])];
  } in
  let j = Jsonrpc.response_to_yojson resp in
  match Jsonrpc.response_of_yojson j with
  | Ok decoded ->
    Alcotest.(check json) "result" (`Assoc [("tools", `List [])]) decoded.result
  | Error e -> Alcotest.fail e

(* --- error_response --- *)

let test_error_response_roundtrip () =
  let err : Jsonrpc.error_response = {
    jsonrpc = "2.0";
    id = Int 1;
    error = { code = -32601; message = "Method not found"; data = None };
  } in
  let j = Jsonrpc.error_response_to_yojson err in
  match Jsonrpc.error_response_of_yojson j with
  | Ok decoded ->
    Alcotest.(check int) "code" (-32601) decoded.error.code;
    Alcotest.(check string) "message" "Method not found" decoded.error.message
  | Error e -> Alcotest.fail e

(* --- message parsing --- *)

let test_message_parse_request () =
  let j = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "tools/list");
  ] in
  match Jsonrpc.message_of_yojson j with
  | Ok (Request r) ->
    Alcotest.(check string) "method" "tools/list" r.method_
  | Ok _ -> Alcotest.fail "Expected Request"
  | Error e -> Alcotest.fail e

let test_message_parse_notification () =
  let j = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "notifications/initialized");
  ] in
  match Jsonrpc.message_of_yojson j with
  | Ok (Notification n) ->
    Alcotest.(check string) "method" "notifications/initialized" n.method_
  | Ok _ -> Alcotest.fail "Expected Notification"
  | Error e -> Alcotest.fail e

let test_message_parse_response () =
  let j = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("result", `Assoc []);
  ] in
  match Jsonrpc.message_of_yojson j with
  | Ok (Response _) -> ()
  | Ok _ -> Alcotest.fail "Expected Response"
  | Error e -> Alcotest.fail e

let test_message_parse_error () =
  let j = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("error", `Assoc [("code", `Int (-32601)); ("message", `String "Not found")]);
  ] in
  match Jsonrpc.message_of_yojson j with
  | Ok (Error _) -> ()
  | Ok _ -> Alcotest.fail "Expected Error"
  | Error e -> Alcotest.fail e

let test_message_parse_invalid () =
  let j = `Assoc [("foo", `String "bar")] in
  Alcotest.(check bool) "invalid message returns Error"
    true
    (Result.is_error (Jsonrpc.message_of_yojson j))

(* --- make_* helpers --- *)

let test_make_request () =
  match Jsonrpc.make_request ~id:(Int 1) ~method_:"tools/list" () with
  | Request r ->
    Alcotest.(check string) "jsonrpc" "2.0" r.jsonrpc;
    Alcotest.(check string) "method" "tools/list" r.method_
  | _ -> Alcotest.fail "Expected Request"

let test_make_notification () =
  match Jsonrpc.make_notification ~method_:"initialized" () with
  | Notification n ->
    Alcotest.(check string) "method" "initialized" n.method_
  | _ -> Alcotest.fail "Expected Notification"

let test_make_response () =
  match Jsonrpc.make_response ~id:(Int 1) ~result:(`Assoc []) with
  | Response r ->
    Alcotest.(check json) "result" (`Assoc []) r.result
  | _ -> Alcotest.fail "Expected Response"

let test_make_error () =
  match Jsonrpc.make_error ~id:(Int 1) ~code:(-32601) ~message:"Not found" () with
  | Error e ->
    Alcotest.(check int) "code" (-32601) e.error.code
  | _ -> Alcotest.fail "Expected Error"

(* --- message_to_yojson round-trip --- *)

let test_message_to_yojson_roundtrip () =
  let msg = Jsonrpc.make_request ~id:(Int 42) ~method_:"ping" () in
  let j = Jsonrpc.message_to_yojson msg in
  match Jsonrpc.message_of_yojson j with
  | Ok (Request r) ->
    Alcotest.(check string) "method" "ping" r.method_
  | _ -> Alcotest.fail "Round-trip failed"

(* --- message_of_string --- *)

let test_message_of_string_valid () =
  let s = {|{"jsonrpc":"2.0","id":1,"method":"tools/list"}|} in
  match Jsonrpc.message_of_string s with
  | Ok (Request r) ->
    Alcotest.(check string) "method" "tools/list" r.method_
  | Ok _ -> Alcotest.fail "Expected Request"
  | Error e -> Alcotest.fail e

let test_message_of_string_invalid_json () =
  let s = "not json at all" in
  Alcotest.(check bool) "invalid json returns Error"
    true
    (Result.is_error (Jsonrpc.message_of_string s))

(* --- Suite --- *)

let () =
  Alcotest.run "Jsonrpc" [
    "id", [
      Alcotest.test_case "string round-trip" `Quick test_id_string_roundtrip;
      Alcotest.test_case "int round-trip" `Quick test_id_int_roundtrip;
      Alcotest.test_case "null round-trip" `Quick test_id_null_roundtrip;
      Alcotest.test_case "invalid" `Quick test_id_invalid;
    ];
    "request", [
      Alcotest.test_case "round-trip" `Quick test_request_roundtrip;
      Alcotest.test_case "with params" `Quick test_request_with_params;
    ];
    "notification", [
      Alcotest.test_case "round-trip" `Quick test_notification_roundtrip;
    ];
    "response", [
      Alcotest.test_case "round-trip" `Quick test_response_roundtrip;
    ];
    "error_response", [
      Alcotest.test_case "round-trip" `Quick test_error_response_roundtrip;
    ];
    "message_parsing", [
      Alcotest.test_case "parse request" `Quick test_message_parse_request;
      Alcotest.test_case "parse notification" `Quick test_message_parse_notification;
      Alcotest.test_case "parse response" `Quick test_message_parse_response;
      Alcotest.test_case "parse error" `Quick test_message_parse_error;
      Alcotest.test_case "parse invalid" `Quick test_message_parse_invalid;
    ];
    "make_helpers", [
      Alcotest.test_case "make_request" `Quick test_make_request;
      Alcotest.test_case "make_notification" `Quick test_make_notification;
      Alcotest.test_case "make_response" `Quick test_make_response;
      Alcotest.test_case "make_error" `Quick test_make_error;
    ];
    "round_trip", [
      Alcotest.test_case "message_to_yojson" `Quick test_message_to_yojson_roundtrip;
    ];
    "message_of_string", [
      Alcotest.test_case "valid" `Quick test_message_of_string_valid;
      Alcotest.test_case "invalid json" `Quick test_message_of_string_invalid_json;
    ];
  ]
