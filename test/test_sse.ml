(** Tests for SSE encoding, parsing, and broadcasting. *)

open Mcp_protocol_http

(* ── Encoding tests ──────────────────────────── *)

let test_encode_data_only () =
  let e = Sse.data "hello" in
  let encoded = Sse.encode e in
  Alcotest.(check string) "data-only event"
    "data: hello\n\n"
    encoded

let test_encode_with_event_type () =
  let e = Sse.event "message" "hello world" in
  let encoded = Sse.encode e in
  Alcotest.(check string) "event with type"
    "event: message\ndata: hello world\n\n"
    encoded

let test_encode_with_id () =
  let e = Sse.data "test" |> Sse.with_id "42" in
  let encoded = Sse.encode e in
  Alcotest.(check string) "event with id"
    "data: test\nid: 42\n\n"
    encoded

let test_encode_with_retry () =
  let e = Sse.data "test" |> Sse.with_retry 3000 in
  let encoded = Sse.encode e in
  Alcotest.(check string) "event with retry"
    "data: test\nretry: 3000\n\n"
    encoded

let test_encode_all_fields () =
  let e = Sse.event "update" "payload"
          |> Sse.with_id "7"
          |> Sse.with_retry 5000 in
  let encoded = Sse.encode e in
  Alcotest.(check string) "all fields"
    "event: update\ndata: payload\nid: 7\nretry: 5000\n\n"
    encoded

let test_encode_multiline_data () =
  let e = Sse.data "line1\nline2\nline3" in
  let encoded = Sse.encode e in
  Alcotest.(check string) "multiline data"
    "data: line1\ndata: line2\ndata: line3\n\n"
    encoded

let test_encode_empty_data () =
  let e = Sse.data "" in
  let encoded = Sse.encode e in
  Alcotest.(check string) "empty data"
    "data: \n\n"
    encoded

(* ── Parsing tests ───────────────────────────── *)

let test_parse_data_only () =
  let result = Sse.parse_event "data: hello\n\n" in
  match result with
  | None -> Alcotest.fail "expected Some"
  | Some e ->
    Alcotest.(check (option string)) "no event type" None e.event_type;
    Alcotest.(check string) "data" "hello" e.data;
    Alcotest.(check (option string)) "no id" None e.id;
    Alcotest.(check (option int)) "no retry" None e.retry

let test_parse_all_fields () =
  let input = "event: update\ndata: payload\nid: 7\nretry: 5000\n\n" in
  let result = Sse.parse_event input in
  match result with
  | None -> Alcotest.fail "expected Some"
  | Some e ->
    Alcotest.(check (option string)) "event type" (Some "update") e.event_type;
    Alcotest.(check string) "data" "payload" e.data;
    Alcotest.(check (option string)) "id" (Some "7") e.id;
    Alcotest.(check (option int)) "retry" (Some 5000) e.retry

let test_parse_multiline_data () =
  let input = "data: line1\ndata: line2\ndata: line3\n\n" in
  let result = Sse.parse_event input in
  match result with
  | None -> Alcotest.fail "expected Some"
  | Some e ->
    Alcotest.(check string) "multiline" "line1\nline2\nline3" e.data

let test_parse_no_data () =
  let input = "event: ping\n\n" in
  let result = Sse.parse_event input in
  Alcotest.(check bool) "no data → None" true (result = None)

let test_parse_roundtrip () =
  let original = Sse.event "msg" "hello world"
                 |> Sse.with_id "99"
                 |> Sse.with_retry 1000 in
  let encoded = Sse.encode original in
  let parsed = Sse.parse_event encoded in
  match parsed with
  | None -> Alcotest.fail "roundtrip failed"
  | Some e ->
    Alcotest.(check (option string)) "event type" original.event_type e.event_type;
    Alcotest.(check string) "data" original.data e.data;
    Alcotest.(check (option string)) "id" original.id e.id;
    Alcotest.(check (option int)) "retry" original.retry e.retry

(* ── Ping test ───────────────────────────────── *)

let test_ping () =
  Alcotest.(check string) "ping format" ": ping\n\n" Sse.ping

(* ── Broadcaster tests ───────────────────────── *)

let test_broadcaster_subscribe () =
  Eio_main.run @@ fun _env ->
  let b = Sse.Broadcaster.create () in
  Alcotest.(check int) "initial count" 0 (Sse.Broadcaster.client_count b);
  let _id, _stream = Sse.Broadcaster.subscribe b in
  Alcotest.(check int) "after subscribe" 1 (Sse.Broadcaster.client_count b)

let test_broadcaster_unsubscribe () =
  Eio_main.run @@ fun _env ->
  let b = Sse.Broadcaster.create () in
  let id, _stream = Sse.Broadcaster.subscribe b in
  Alcotest.(check int) "subscribed" 1 (Sse.Broadcaster.client_count b);
  Sse.Broadcaster.unsubscribe b id;
  Alcotest.(check int) "unsubscribed" 0 (Sse.Broadcaster.client_count b)

let test_broadcaster_broadcast () =
  Eio_main.run @@ fun _env ->
  let b = Sse.Broadcaster.create () in
  let _id1, s1 = Sse.Broadcaster.subscribe b in
  let _id2, s2 = Sse.Broadcaster.subscribe b in
  let evt = Sse.data "hello" in
  Sse.Broadcaster.broadcast b evt;
  let r1 = Eio.Stream.take s1 in
  let r2 = Eio.Stream.take s2 in
  Alcotest.(check string) "client 1" "hello" r1.data;
  Alcotest.(check string) "client 2" "hello" r2.data

let test_broadcaster_broadcast_after_unsubscribe () =
  Eio_main.run @@ fun _env ->
  let b = Sse.Broadcaster.create () in
  let id1, _s1 = Sse.Broadcaster.subscribe b in
  let _id2, s2 = Sse.Broadcaster.subscribe b in
  Sse.Broadcaster.unsubscribe b id1;
  let evt = Sse.data "after unsub" in
  Sse.Broadcaster.broadcast b evt;
  let r2 = Eio.Stream.take s2 in
  Alcotest.(check string) "remaining client" "after unsub" r2.data;
  Alcotest.(check int) "count after unsub" 1 (Sse.Broadcaster.client_count b)

(* ── Test suite ──────────────────────────────── *)

let () =
  Alcotest.run "SSE" [
    "encode", [
      Alcotest.test_case "data only" `Quick test_encode_data_only;
      Alcotest.test_case "with event type" `Quick test_encode_with_event_type;
      Alcotest.test_case "with id" `Quick test_encode_with_id;
      Alcotest.test_case "with retry" `Quick test_encode_with_retry;
      Alcotest.test_case "all fields" `Quick test_encode_all_fields;
      Alcotest.test_case "multiline data" `Quick test_encode_multiline_data;
      Alcotest.test_case "empty data" `Quick test_encode_empty_data;
    ];
    "parse", [
      Alcotest.test_case "data only" `Quick test_parse_data_only;
      Alcotest.test_case "all fields" `Quick test_parse_all_fields;
      Alcotest.test_case "multiline data" `Quick test_parse_multiline_data;
      Alcotest.test_case "no data → None" `Quick test_parse_no_data;
      Alcotest.test_case "roundtrip" `Quick test_parse_roundtrip;
    ];
    "ping", [
      Alcotest.test_case "format" `Quick test_ping;
    ];
    "broadcaster", [
      Alcotest.test_case "subscribe" `Quick test_broadcaster_subscribe;
      Alcotest.test_case "unsubscribe" `Quick test_broadcaster_unsubscribe;
      Alcotest.test_case "broadcast" `Quick test_broadcaster_broadcast;
      Alcotest.test_case "broadcast after unsub" `Quick test_broadcaster_broadcast_after_unsubscribe;
    ];
  ]
