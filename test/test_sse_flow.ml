(** Unit tests for Sse_flow: Eio.Stream → Eio.Flow.source bridge. *)

open Mcp_protocol_http

(* ── helpers ────────────────────────────────── *)

(** Read all available bytes from a source until it would block.
    We push events first then read, so the source always has data. *)
let read_n_bytes source n =
  let buf = Cstruct.create n in
  let got = Eio.Flow.single_read source buf in
  Cstruct.to_string (Cstruct.sub buf 0 got)

let read_all_available source max =
  let result = Buffer.create 256 in
  let buf = Cstruct.create max in
  let rec loop () =
    let got = Eio.Flow.single_read source buf in
    Buffer.add_string result (Cstruct.to_string (Cstruct.sub buf 0 got));
    if got < Cstruct.length buf then ()  (* partial read = no more buffered *)
    else loop ()
  in
  loop ();
  Buffer.contents result

(* ── tests ──────────────────────────────────── *)

(** Single event is encoded and readable as SSE wire format. *)
let test_single_event () =
  Eio_main.run @@ fun _env ->
  let stream = Eio.Stream.create 10 in
  let evt = Sse.event "message" "hello" in
  Eio.Stream.add stream (Some evt);
  let flow = Sse_flow.create stream in
  let source = Sse_flow.as_source flow in
  let data = read_all_available source 4096 in
  let expected = "event: message\ndata: hello\n\n" in
  Alcotest.(check string) "SSE wire format" expected data

(** Multi-line data event produces multiple data: lines. *)
let test_multiline_event () =
  Eio_main.run @@ fun _env ->
  let stream = Eio.Stream.create 10 in
  let evt = Sse.event "message" "line1\nline2\nline3" in
  Eio.Stream.add stream (Some evt);
  let flow = Sse_flow.create stream in
  let source = Sse_flow.as_source flow in
  let data = read_all_available source 4096 in
  let expected =
    "event: message\ndata: line1\ndata: line2\ndata: line3\n\n" in
  Alcotest.(check string) "multi-line SSE" expected data

(** Event with id field is correctly encoded. *)
let test_event_with_id () =
  Eio_main.run @@ fun _env ->
  let stream = Eio.Stream.create 10 in
  let evt = Sse.event "message" "payload"
    |> Sse.with_id "42" in
  Eio.Stream.add stream (Some evt);
  let flow = Sse_flow.create stream in
  let source = Sse_flow.as_source flow in
  let data = read_all_available source 4096 in
  let expected = "event: message\ndata: payload\nid: 42\n\n" in
  Alcotest.(check string) "SSE with id" expected data

(** Multiple events are streamed sequentially. *)
let test_multiple_events () =
  Eio_main.run @@ fun _env ->
  let stream = Eio.Stream.create 10 in
  Eio.Stream.add stream (Some (Sse.event "message" "first"));
  Eio.Stream.add stream (Some (Sse.event "message" "second"));
  let flow = Sse_flow.create stream in
  let source = Sse_flow.as_source flow in
  (* Read first event *)
  let data1 = read_all_available source 4096 in
  Alcotest.(check string) "first event"
    "event: message\ndata: first\n\n" data1;
  (* Read second event *)
  let data2 = read_all_available source 4096 in
  Alcotest.(check string) "second event"
    "event: message\ndata: second\n\n" data2

(** Small buffer forces partial reads; all bytes are still delivered. *)
let test_small_buffer () =
  Eio_main.run @@ fun _env ->
  let stream = Eio.Stream.create 10 in
  let evt = Sse.event "message" "hello world" in
  Eio.Stream.add stream (Some evt);
  let flow = Sse_flow.create stream in
  let source = Sse_flow.as_source flow in
  let expected = Sse.encode evt in
  (* Read in tiny 4-byte chunks *)
  let result = Buffer.create 64 in
  let remaining = ref (String.length expected) in
  while !remaining > 0 do
    let chunk = read_n_bytes source 4 in
    Buffer.add_string result chunk;
    remaining := !remaining - String.length chunk
  done;
  Alcotest.(check string) "reassembled" expected (Buffer.contents result)

(** Blocking behavior: single_read blocks until event arrives. *)
let test_blocking_on_empty_stream () =
  Eio_main.run @@ fun _env ->
  let stream = Eio.Stream.create 10 in
  let flow = Sse_flow.create stream in
  let source = Sse_flow.as_source flow in
  let received = ref "" in
  Eio.Fiber.both
    (fun () ->
      (* Producer: wait a bit then push an event *)
      Eio.Fiber.yield ();
      Eio.Stream.add stream (Some (Sse.event "message" "delayed")))
    (fun () ->
      (* Consumer: will block until event arrives *)
      received := read_all_available source 4096);
  Alcotest.(check string) "received after block"
    "event: message\ndata: delayed\n\n" !received

(** Ping is emitted after idle timeout when sleep is provided. *)
let test_ping_on_idle () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let stream = Eio.Stream.create 10 in
  let flow = Sse_flow.create
    ~sleep:(Eio.Time.sleep clock)
    ~ping_interval:0.01
    stream in
  let source = Sse_flow.as_source flow in
  let data = read_all_available source 4096 in
  Alcotest.(check string) "ping on idle" ": ping\n\n" data

(** Events take priority over ping when already queued. *)
let test_event_before_ping () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let stream = Eio.Stream.create 10 in
  Eio.Stream.add stream (Some (Sse.event "message" "real"));
  let flow = Sse_flow.create
    ~sleep:(Eio.Time.sleep clock)
    ~ping_interval:10.0
    stream in
  let source = Sse_flow.as_source flow in
  let data = read_all_available source 4096 in
  Alcotest.(check string) "event before ping"
    "event: message\ndata: real\n\n" data

(** None (poison pill) raises End_of_file. *)
let test_eof_on_none () =
  Eio_main.run @@ fun _env ->
  let stream = Eio.Stream.create 10 in
  Eio.Stream.add stream None;
  let flow = Sse_flow.create stream in
  let source = Sse_flow.as_source flow in
  let buf = Cstruct.create 64 in
  let raised =
    try ignore (Eio.Flow.single_read source buf); false
    with End_of_file -> true
  in
  Alcotest.(check bool) "End_of_file raised" true raised

(* ── runner ─────────────────────────────────── *)

let () =
  Alcotest.run "Sse_flow" [
    "encoding", [
      Alcotest.test_case "single event" `Quick test_single_event;
      Alcotest.test_case "multi-line" `Quick test_multiline_event;
      Alcotest.test_case "with id" `Quick test_event_with_id;
    ];
    "streaming", [
      Alcotest.test_case "multiple events" `Quick test_multiple_events;
      Alcotest.test_case "small buffer" `Quick test_small_buffer;
      Alcotest.test_case "blocking" `Quick test_blocking_on_empty_stream;
      Alcotest.test_case "EOF on None" `Quick test_eof_on_none;
    ];
    "ping", [
      Alcotest.test_case "ping on idle" `Quick test_ping_on_idle;
      Alcotest.test_case "event before ping" `Quick test_event_before_ping;
    ];
  ]
