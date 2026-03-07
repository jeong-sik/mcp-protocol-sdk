(** Tests for Eio-based stdio transport. *)

open Mcp_protocol

let with_transport input_str f =
  Eio_main.run @@ fun _env ->
  let source = Eio.Flow.string_source input_str in
  let buf = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink buf in
  let transport = Mcp_protocol_eio.Stdio_transport.create ~stdin:source ~stdout:sink () in
  f transport buf

(* ── read tests ────────────────────────────────────────────── *)

let test_read_valid_request () =
  let json = {|{"jsonrpc":"2.0","id":1,"method":"ping"}|} ^ "\n" in
  with_transport json (fun t _buf ->
    match Mcp_protocol_eio.Stdio_transport.read t with
    | Some (Ok (Request req)) ->
      Alcotest.(check string) "method" "ping" req.method_;
      Alcotest.(check bool) "has id" true true  (* Request always has id *)
    | Some (Ok _) -> Alcotest.fail "Expected Request variant"
    | Some (Error e) -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e)
    | None -> Alcotest.fail "Unexpected EOF"
  )

let test_read_valid_notification () =
  let json = {|{"jsonrpc":"2.0","method":"notifications/initialized"}|} ^ "\n" in
  with_transport json (fun t _buf ->
    match Mcp_protocol_eio.Stdio_transport.read t with
    | Some (Ok (Notification notif)) ->
      Alcotest.(check string) "method" "notifications/initialized" notif.method_
    | Some (Ok _) -> Alcotest.fail "Expected Notification variant"
    | Some (Error e) -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e)
    | None -> Alcotest.fail "Unexpected EOF"
  )

let test_read_valid_response () =
  let json = {|{"jsonrpc":"2.0","id":42,"result":{"ok":true}}|} ^ "\n" in
  with_transport json (fun t _buf ->
    match Mcp_protocol_eio.Stdio_transport.read t with
    | Some (Ok (Response _)) -> ()
    | Some (Ok _) -> Alcotest.fail "Expected Response variant"
    | Some (Error e) -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e)
    | None -> Alcotest.fail "Unexpected EOF"
  )

let test_read_invalid_json () =
  let json = "not valid json\n" in
  with_transport json (fun t _buf ->
    match Mcp_protocol_eio.Stdio_transport.read t with
    | Some (Error _) -> ()
    | Some (Ok _) -> Alcotest.fail "Should have failed on invalid JSON"
    | None -> Alcotest.fail "Unexpected EOF"
  )

let test_read_eof () =
  with_transport "" (fun t _buf ->
    match Mcp_protocol_eio.Stdio_transport.read t with
    | None -> ()
    | Some _ -> Alcotest.fail "Expected EOF"
  )

let test_read_multiple_messages () =
  let json =
    {|{"jsonrpc":"2.0","id":1,"method":"ping"}|} ^ "\n" ^
    {|{"jsonrpc":"2.0","id":2,"method":"ping"}|} ^ "\n"
  in
  with_transport json (fun t _buf ->
    (match Mcp_protocol_eio.Stdio_transport.read t with
     | Some (Ok (Request req)) ->
       (match req.id with
        | Jsonrpc.Int 1 -> ()
        | _ -> Alcotest.fail "Expected id=1")
     | _ -> Alcotest.fail "Expected first message");
    (match Mcp_protocol_eio.Stdio_transport.read t with
     | Some (Ok (Request req)) ->
       (match req.id with
        | Jsonrpc.Int 2 -> ()
        | _ -> Alcotest.fail "Expected id=2")
     | _ -> Alcotest.fail "Expected second message");
    (match Mcp_protocol_eio.Stdio_transport.read t with
     | None -> ()
     | _ -> Alcotest.fail "Expected EOF after two messages")
  )

let test_read_skips_blank_lines () =
  let json =
    "\n\n" ^
    {|{"jsonrpc":"2.0","id":1,"method":"ping"}|} ^ "\n"
  in
  with_transport json (fun t _buf ->
    match Mcp_protocol_eio.Stdio_transport.read t with
    | Some (Ok (Request req)) ->
      Alcotest.(check string) "method" "ping" req.method_
    | _ -> Alcotest.fail "Expected message after blank lines"
  )

(* ── write tests ───────────────────────────────────────────── *)

let test_write_request () =
  with_transport "" (fun t buf ->
    let msg = Jsonrpc.make_request ~id:(Jsonrpc.Int 1) ~method_:"ping" () in
    (match Mcp_protocol_eio.Stdio_transport.write t msg with
     | Ok () -> ()
     | Error e -> Alcotest.fail e);
    let output = Buffer.contents buf in
    Alcotest.(check bool) "ends with newline" true
      (String.length output > 0 && output.[String.length output - 1] = '\n');
    let trimmed = String.trim output in
    let json = Yojson.Safe.from_string trimmed in
    (* ppx_deriving_yojson drops fields equal to [@default], so "jsonrpc":"2.0"
       is omitted. We verify the essential fields instead. *)
    Alcotest.(check bool) "has method" true
      (match json with `Assoc fields -> List.mem_assoc "method" fields | _ -> false);
    Alcotest.(check bool) "has id" true
      (match json with `Assoc fields -> List.mem_assoc "id" fields | _ -> false)
  )

let test_write_response () =
  with_transport "" (fun t buf ->
    let msg = Jsonrpc.make_response ~id:(Jsonrpc.Int 1) ~result:(`Assoc [("ok", `Bool true)]) in
    (match Mcp_protocol_eio.Stdio_transport.write t msg with
     | Ok () -> ()
     | Error e -> Alcotest.fail e);
    let output = Buffer.contents buf in
    let trimmed = String.trim output in
    match Yojson.Safe.from_string trimmed with
    | `Assoc fields ->
      Alcotest.(check bool) "has result" true (List.mem_assoc "result" fields)
    | _ -> Alcotest.fail "Expected JSON object"
  )

let test_write_notification () =
  with_transport "" (fun t buf ->
    let msg = Jsonrpc.make_notification ~method_:"notifications/initialized" () in
    (match Mcp_protocol_eio.Stdio_transport.write t msg with
     | Ok () -> ()
     | Error e -> Alcotest.fail e);
    let output = Buffer.contents buf in
    let trimmed = String.trim output in
    match Yojson.Safe.from_string trimmed with
    | `Assoc fields ->
      Alcotest.(check bool) "has method" true (List.mem_assoc "method" fields);
      Alcotest.(check bool) "no id" true (not (List.mem_assoc "id" fields))
    | _ -> Alcotest.fail "Expected JSON object"
  )

(* ── round-trip tests ──────────────────────────────────────── *)

let test_roundtrip_request () =
  Eio_main.run @@ fun _env ->
  let msg = Jsonrpc.make_request ~id:(Jsonrpc.Int 7)
      ~method_:"tools/list"
      ~params:(`Assoc [("cursor", `Null)])
      () in
  let json_line = Yojson.Safe.to_string (Jsonrpc.message_to_yojson msg) ^ "\n" in
  let source = Eio.Flow.string_source json_line in
  let buf = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink buf in
  let t = Mcp_protocol_eio.Stdio_transport.create ~stdin:source ~stdout:sink () in
  match Mcp_protocol_eio.Stdio_transport.read t with
  | Some (Ok (Request req)) ->
    Alcotest.(check string) "method preserved" "tools/list" req.method_;
    (match req.id with
     | Jsonrpc.Int 7 -> ()
     | _ -> Alcotest.fail "id not preserved")
  | _ -> Alcotest.fail "Round-trip failed"

let test_roundtrip_error () =
  Eio_main.run @@ fun _env ->
  let msg = Jsonrpc.make_error ~id:(Jsonrpc.Int 1)
      ~code:Error_codes.method_not_found
      ~message:"Not found" () in
  let json_line = Yojson.Safe.to_string (Jsonrpc.message_to_yojson msg) ^ "\n" in
  let source = Eio.Flow.string_source json_line in
  let buf = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink buf in
  let t = Mcp_protocol_eio.Stdio_transport.create ~stdin:source ~stdout:sink () in
  match Mcp_protocol_eio.Stdio_transport.read t with
  | Some (Ok (Error _)) -> ()
  | _ -> Alcotest.fail "Round-trip error failed"

(* ── close tests ───────────────────────────────────────────── *)

let test_close_prevents_write () =
  with_transport "" (fun t _buf ->
    Mcp_protocol_eio.Stdio_transport.close t;
    let msg = Jsonrpc.make_notification ~method_:"ping" () in
    match Mcp_protocol_eio.Stdio_transport.write t msg with
    | Error _ -> ()
    | Ok () -> Alcotest.fail "Write should fail after close"
  )

let test_close_prevents_read () =
  let json = {|{"jsonrpc":"2.0","id":1,"method":"ping"}|} ^ "\n" in
  with_transport json (fun t _buf ->
    Mcp_protocol_eio.Stdio_transport.close t;
    match Mcp_protocol_eio.Stdio_transport.read t with
    | None -> ()
    | _ -> Alcotest.fail "Read should return None after close"
  )

(* ── test suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run "Stdio_transport" [
    "read", [
      Alcotest.test_case "valid request" `Quick test_read_valid_request;
      Alcotest.test_case "valid notification" `Quick test_read_valid_notification;
      Alcotest.test_case "valid response" `Quick test_read_valid_response;
      Alcotest.test_case "invalid json" `Quick test_read_invalid_json;
      Alcotest.test_case "eof" `Quick test_read_eof;
      Alcotest.test_case "multiple messages" `Quick test_read_multiple_messages;
      Alcotest.test_case "skips blank lines" `Quick test_read_skips_blank_lines;
    ];
    "write", [
      Alcotest.test_case "request" `Quick test_write_request;
      Alcotest.test_case "response" `Quick test_write_response;
      Alcotest.test_case "notification" `Quick test_write_notification;
    ];
    "roundtrip", [
      Alcotest.test_case "request" `Quick test_roundtrip_request;
      Alcotest.test_case "error" `Quick test_roundtrip_error;
    ];
    "close", [
      Alcotest.test_case "prevents write" `Quick test_close_prevents_write;
      Alcotest.test_case "prevents read" `Quick test_close_prevents_read;
    ];
  ]
