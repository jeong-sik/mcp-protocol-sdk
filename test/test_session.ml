open Mcp_protocol

(* --- Request_tracker (immutable API) --- *)

let test_tracker_create () =
  let t = Session.Request_tracker.create () in
  Alcotest.(check int) "initially empty" 0 (Session.Request_tracker.pending_count t)

let test_tracker_track_and_complete () =
  let t = Session.Request_tracker.create () in
  let (id, t) = Session.Request_tracker.next_id t in
  let (_req, t) = Session.Request_tracker.track t ~id ~method_:"tools/list" () in
  Alcotest.(check int) "one pending" 1 (Session.Request_tracker.pending_count t);
  let (completed, t) = Session.Request_tracker.complete t ~id ~response:(`String "ok") in
  Alcotest.(check bool) "completed" true (Option.is_some completed);
  Alcotest.(check int) "none pending" 0 (Session.Request_tracker.pending_count t)

let test_tracker_next_id_increments () =
  let t = Session.Request_tracker.create () in
  let (id1, t) = Session.Request_tracker.next_id t in
  let (id2, _t) = Session.Request_tracker.next_id t in
  Alcotest.(check bool) "ids differ"
    true (Session.request_id_to_yojson id1 <> Session.request_id_to_yojson id2)

let test_tracker_cancel () =
  let t = Session.Request_tracker.create () in
  let (id, t) = Session.Request_tracker.next_id t in
  let (_req, t) = Session.Request_tracker.track t ~id ~method_:"tools/call" () in
  let (cancelled, t) = Session.Request_tracker.cancel t ~id ~reason:(Some "user aborted") in
  Alcotest.(check bool) "cancelled" true cancelled;
  Alcotest.(check int) "none pending" 0 (Session.Request_tracker.pending_count t)

let test_tracker_cancel_nonexistent () =
  let t = Session.Request_tracker.create () in
  let (cancelled, _t) = Session.Request_tracker.cancel t ~id:(Int_id 999) ~reason:None in
  Alcotest.(check bool) "not found" false cancelled

let test_tracker_complete_nonexistent () =
  let t = Session.Request_tracker.create () in
  let (result, _t) = Session.Request_tracker.complete t ~id:(Int_id 999) ~response:`Null in
  Alcotest.(check bool) "not found" true (Option.is_none result)

let test_tracker_cancel_all () =
  let t = Session.Request_tracker.create () in
  let (id1, t) = Session.Request_tracker.next_id t in
  let (id2, t) = Session.Request_tracker.next_id t in
  let (_r1, t) = Session.Request_tracker.track t ~id:id1 ~method_:"a" () in
  let (_r2, t) = Session.Request_tracker.track t ~id:id2 ~method_:"b" () in
  Alcotest.(check int) "two pending" 2 (Session.Request_tracker.pending_count t);
  let (cancelled, t) = Session.Request_tracker.cancel_all t ~reason:"shutdown" in
  Alcotest.(check int) "cancelled 2" 2 (List.length cancelled);
  Alcotest.(check int) "none pending" 0 (Session.Request_tracker.pending_count t)

let test_tracker_immutability () =
  let t0 = Session.Request_tracker.create () in
  let (id, t1) = Session.Request_tracker.next_id t0 in
  let (_req, t2) = Session.Request_tracker.track t1 ~id ~method_:"test" () in
  Alcotest.(check int) "t0 still empty" 0 (Session.Request_tracker.pending_count t0);
  Alcotest.(check int) "t1 still empty" 0 (Session.Request_tracker.pending_count t1);
  Alcotest.(check int) "t2 has one" 1 (Session.Request_tracker.pending_count t2)

let test_tracker_check_timeouts () =
  let t = Session.Request_tracker.create () in
  let (id, t) = Session.Request_tracker.next_id t in
  let (_req, t) = Session.Request_tracker.track t ~id ~method_:"slow" ~timeout:0.0 () in
  Unix.sleepf 0.01;
  let (timed_out, t) = Session.Request_tracker.check_timeouts t in
  Alcotest.(check int) "one timed out" 1 (List.length timed_out);
  Alcotest.(check int) "none pending" 0 (Session.Request_tracker.pending_count t)

let test_tracker_complete_returns_updated_request () =
  let t = Session.Request_tracker.create () in
  let (id, t) = Session.Request_tracker.next_id t in
  let (_req, t) = Session.Request_tracker.track t ~id ~method_:"test" () in
  let (completed, _t) = Session.Request_tracker.complete t ~id ~response:(`String "done") in
  match completed with
  | Some req ->
    Alcotest.(check bool) "state is Completed"
      true (req.state = Session.Completed);
    Alcotest.(check bool) "response set"
      true (Option.is_some req.response)
  | None -> Alcotest.fail "expected completed request"

let test_tracker_cancel_all_sets_error_state () =
  let t = Session.Request_tracker.create () in
  let (id, t) = Session.Request_tracker.next_id t in
  let (_req, t) = Session.Request_tracker.track t ~id ~method_:"test" () in
  let (cancelled, _t) = Session.Request_tracker.cancel_all t ~reason:"bye" in
  match cancelled with
  | [req] ->
    (match req.state with
     | Session.Error reason -> Alcotest.(check string) "reason" "bye" reason
     | _ -> Alcotest.fail "expected Error state")
  | _ -> Alcotest.fail "expected exactly one cancelled request"

(* --- Session lifecycle --- *)

let test_lifecycle_to_string () =
  Alcotest.(check string) "created" "created"
    (Session.lifecycle_to_string Created);
  Alcotest.(check string) "initializing" "initializing"
    (Session.lifecycle_to_string Initializing);
  Alcotest.(check string) "ready" "ready"
    (Session.lifecycle_to_string Ready);
  Alcotest.(check string) "closing" "closing"
    (Session.lifecycle_to_string Closing);
  Alcotest.(check string) "closed" "closed"
    (Session.lifecycle_to_string Closed)

let test_create_session () =
  let s = Session.create_session ~id:"test-1" ~protocol_version:"2025-03-26" () in
  Alcotest.(check string) "id" "test-1" s.id;
  Alcotest.(check string) "version" "2025-03-26" s.protocol_version;
  Alcotest.(check string) "lifecycle" "created"
    (Session.lifecycle_to_string s.lifecycle);
  Alcotest.(check bool) "not initialized" true (Option.is_none s.initialized_at);
  Alcotest.(check bool) "not closed" true (Option.is_none s.closed_at)

let test_session_ready () =
  let s = Session.create_session ~id:"test-2" ~protocol_version:"2025-03-26" () in
  Session.session_ready s;
  Alcotest.(check string) "lifecycle" "ready"
    (Session.lifecycle_to_string s.lifecycle);
  Alcotest.(check bool) "initialized_at set" true (Option.is_some s.initialized_at)

let test_session_close () =
  let s = Session.create_session ~id:"test-3" ~protocol_version:"2025-03-26" () in
  Session.session_ready s;
  Session.session_close s;
  Alcotest.(check string) "lifecycle" "closed"
    (Session.lifecycle_to_string s.lifecycle);
  Alcotest.(check bool) "closed_at set" true (Option.is_some s.closed_at)

let test_session_with_info () =
  let si : Mcp_types.server_info = { name = "test-server"; version = "1.0" } in
  let ci : Mcp_types.client_info = { name = "test-client"; version = "2.0" } in
  let s = Session.create_session ~id:"test-4" ~protocol_version:"2025-11-25"
    ~server_info:si ~client_info:ci () in
  Alcotest.(check bool) "has server_info" true (Option.is_some s.server_info);
  Alcotest.(check bool) "has client_info" true (Option.is_some s.client_info)

(* --- Connection errors --- *)

let test_connection_error_to_string () =
  Alcotest.(check string) "closed" "CONNECTION_CLOSED"
    (Session.connection_error_to_string Connection_closed);
  Alcotest.(check string) "read timeout" "READ_TIMEOUT"
    (Session.connection_error_to_string Read_timeout);
  Alcotest.(check string) "write timeout" "WRITE_TIMEOUT"
    (Session.connection_error_to_string Write_timeout);
  Alcotest.(check bool) "protocol error has message"
    true (String.length (Session.connection_error_to_string (Protocol_error "test")) > 0)

(* --- request_id --- *)

let test_request_id_roundtrip () =
  let id = Session.String_id "abc" in
  let j = Session.request_id_to_yojson id in
  Alcotest.(check (result pass string)) "string id"
    (Ok id) (Session.request_id_of_yojson j);
  let id2 = Session.Int_id 42 in
  let j2 = Session.request_id_to_yojson id2 in
  Alcotest.(check (result pass string)) "int id"
    (Ok id2) (Session.request_id_of_yojson j2)

(* --- Suite --- *)

let () =
  Alcotest.run "Session" [
    "request_tracker", [
      Alcotest.test_case "create" `Quick test_tracker_create;
      Alcotest.test_case "track and complete" `Quick test_tracker_track_and_complete;
      Alcotest.test_case "next_id increments" `Quick test_tracker_next_id_increments;
      Alcotest.test_case "cancel" `Quick test_tracker_cancel;
      Alcotest.test_case "cancel nonexistent" `Quick test_tracker_cancel_nonexistent;
      Alcotest.test_case "complete nonexistent" `Quick test_tracker_complete_nonexistent;
      Alcotest.test_case "cancel all" `Quick test_tracker_cancel_all;
      Alcotest.test_case "immutability" `Quick test_tracker_immutability;
      Alcotest.test_case "check timeouts" `Quick test_tracker_check_timeouts;
      Alcotest.test_case "complete returns updated" `Quick test_tracker_complete_returns_updated_request;
      Alcotest.test_case "cancel_all sets Error state" `Quick test_tracker_cancel_all_sets_error_state;
    ];
    "lifecycle", [
      Alcotest.test_case "to_string" `Quick test_lifecycle_to_string;
      Alcotest.test_case "create_session" `Quick test_create_session;
      Alcotest.test_case "session_ready" `Quick test_session_ready;
      Alcotest.test_case "session_close" `Quick test_session_close;
      Alcotest.test_case "with info" `Quick test_session_with_info;
    ];
    "connection_error", [
      Alcotest.test_case "to_string" `Quick test_connection_error_to_string;
    ];
    "request_id", [
      Alcotest.test_case "round-trip" `Quick test_request_id_roundtrip;
    ];
  ]
