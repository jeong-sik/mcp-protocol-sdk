(** Tests for HTTP session management. *)

open Mcp_protocol_http

(* ── Session ID generation ───────────────────── *)

let test_generate_session_id_length () =
  let sid = Http_session.generate_session_id () in
  Alcotest.(check int) "32 hex chars" 32 (String.length sid)

let test_generate_session_id_hex () =
  let sid = Http_session.generate_session_id () in
  let is_hex c =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
  in
  String.iter (fun c ->
    if not (is_hex c) then
      Alcotest.failf "non-hex char: %c in %s" c sid
  ) sid

let test_generate_session_id_unique () =
  let a = Http_session.generate_session_id () in
  let b = Http_session.generate_session_id () in
  Alcotest.(check bool) "unique IDs" true (a <> b)

(* ── State transitions ───────────────────────── *)

let test_initial_state () =
  let s = Http_session.create () in
  Alcotest.(check bool) "uninitialized"
    true (Http_session.state s = Http_session.Uninitialized);
  Alcotest.(check (option string)) "no session ID" None (Http_session.session_id s)

let test_initialize () =
  let s = Http_session.create () in
  match Http_session.initialize s with
  | Error e -> Alcotest.failf "initialize failed: %s" e
  | Ok sid ->
    Alcotest.(check int) "session ID length" 32 (String.length sid);
    Alcotest.(check bool) "state = Initializing"
      true (Http_session.state s = Http_session.Initializing);
    Alcotest.(check (option string)) "session_id matches" (Some sid) (Http_session.session_id s)

let test_initialize_twice () =
  let s = Http_session.create () in
  let _ = Http_session.initialize s in
  match Http_session.initialize s with
  | Ok _ -> Alcotest.fail "double initialize should fail"
  | Error _ -> ()

let test_ready () =
  let s = Http_session.create () in
  let _ = Http_session.initialize s in
  match Http_session.ready s with
  | Error e -> Alcotest.failf "ready failed: %s" e
  | Ok () ->
    Alcotest.(check bool) "state = Ready"
      true (Http_session.state s = Http_session.Ready)

let test_ready_without_initialize () =
  let s = Http_session.create () in
  match Http_session.ready s with
  | Ok () -> Alcotest.fail "ready without init should fail"
  | Error _ -> ()

let test_close () =
  let s = Http_session.create () in
  let _ = Http_session.initialize s in
  let _ = Http_session.ready s in
  Http_session.close s;
  Alcotest.(check bool) "state = Closed"
    true (Http_session.state s = Http_session.Closed)

(* ── Validation ──────────────────────────────── *)

let test_validate_before_init () =
  let s = Http_session.create () in
  (* Before init, any header (or none) is accepted *)
  Alcotest.(check bool) "no header ok"
    true (Http_session.validate s None = Ok ());
  Alcotest.(check bool) "random header ok"
    true (Http_session.validate s (Some "random") = Ok ())

let test_validate_correct_id () =
  let s = Http_session.create () in
  let sid = match Http_session.initialize s with Ok s -> s | Error e -> failwith e in
  Alcotest.(check bool) "correct ID ok"
    true (Http_session.validate s (Some sid) = Ok ())

let test_validate_missing_header () =
  let s = Http_session.create () in
  let _ = Http_session.initialize s in
  match Http_session.validate s None with
  | Error (`Bad_request _) -> ()
  | _ -> Alcotest.fail "missing header should be Bad_request"

let test_validate_wrong_id () =
  let s = Http_session.create () in
  let _ = Http_session.initialize s in
  match Http_session.validate s (Some "wrong-id") with
  | Error `Not_found -> ()
  | _ -> Alcotest.fail "wrong ID should be Not_found"

(* ── Event IDs ───────────────────────────────── *)

let test_event_id_monotonic () =
  let s = Http_session.create () in
  let a = Http_session.next_event_id s in
  let b = Http_session.next_event_id s in
  let c = Http_session.next_event_id s in
  Alcotest.(check string) "first" "0" a;
  Alcotest.(check string) "second" "1" b;
  Alcotest.(check string) "third" "2" c

(* ── Header name ─────────────────────────────── *)

let test_header_name () =
  Alcotest.(check string) "header name" "mcp-session-id" Http_session.header_name

(* ── Test suite ──────────────────────────────── *)

let () =
  Alcotest.run "HTTP_session" [
    "session_id", [
      Alcotest.test_case "length" `Quick test_generate_session_id_length;
      Alcotest.test_case "hex chars" `Quick test_generate_session_id_hex;
      Alcotest.test_case "unique" `Quick test_generate_session_id_unique;
    ];
    "state", [
      Alcotest.test_case "initial" `Quick test_initial_state;
      Alcotest.test_case "initialize" `Quick test_initialize;
      Alcotest.test_case "initialize twice" `Quick test_initialize_twice;
      Alcotest.test_case "ready" `Quick test_ready;
      Alcotest.test_case "ready without init" `Quick test_ready_without_initialize;
      Alcotest.test_case "close" `Quick test_close;
    ];
    "validate", [
      Alcotest.test_case "before init" `Quick test_validate_before_init;
      Alcotest.test_case "correct id" `Quick test_validate_correct_id;
      Alcotest.test_case "missing header" `Quick test_validate_missing_header;
      Alcotest.test_case "wrong id" `Quick test_validate_wrong_id;
    ];
    "event_id", [
      Alcotest.test_case "monotonic" `Quick test_event_id_monotonic;
    ];
    "header", [
      Alcotest.test_case "name" `Quick test_header_name;
    ];
  ]
