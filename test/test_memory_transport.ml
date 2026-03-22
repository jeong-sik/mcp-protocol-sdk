(** Tests for Memory_transport — in-memory paired transport for testing. *)

open Mcp_protocol
module Mt = Mcp_protocol_eio.Memory_transport

let () = Eio_main.run @@ fun _env ->

let make_request id method_ =
  Jsonrpc.make_request ~id:(Jsonrpc.Int id) ~method_ ()
in

let make_response id result =
  Jsonrpc.make_response ~id:(Jsonrpc.Int id) ~result
in

(* ── basic pair communication ────────────── *)

let test_write_read_pair () =
  let a, b = Mt.create_pair () in
  let msg = make_request 1 "ping" in
  (match Mt.write a msg with
   | Ok () -> ()
   | Error e -> Alcotest.fail e);
  (match Mt.read b with
   | Some (Ok received) ->
     let j1 = Jsonrpc.message_to_yojson msg in
     let j2 = Jsonrpc.message_to_yojson received in
     Alcotest.(check string) "message identity"
       (Yojson.Safe.to_string j1)
       (Yojson.Safe.to_string j2)
   | Some (Error e) -> Alcotest.fail ("read error: " ^ e)
   | None -> Alcotest.fail "expected message, got EOF")
in

(* ── bidirectional communication ─────────── *)

let test_bidirectional () =
  let client, server = Mt.create_pair () in
  let req = make_request 1 "tools/list" in
  let resp = make_response 1 (`Assoc [("tools", `List [])]) in
  (* client -> server *)
  (match Mt.write client req with Ok () -> () | Error e -> Alcotest.fail e);
  (match Mt.read server with
   | Some (Ok _) -> ()
   | _ -> Alcotest.fail "server should read request");
  (* server -> client *)
  (match Mt.write server resp with Ok () -> () | Error e -> Alcotest.fail e);
  (match Mt.read client with
   | Some (Ok _) -> ()
   | _ -> Alcotest.fail "client should read response")
in

(* ── close signals EOF ───────────────────── *)

let test_close_signals_eof () =
  let a, b = Mt.create_pair () in
  Mt.close a;
  (* b should see EOF *)
  (match Mt.read b with
   | None -> ()
   | Some (Ok _) -> Alcotest.fail "expected EOF after close"
   | Some (Error _) -> Alcotest.fail "expected EOF, not error")
in

(* ── write after close returns Error ─────── *)

let test_write_after_close () =
  let a, _b = Mt.create_pair () in
  Mt.close a;
  let msg = make_request 1 "ping" in
  (match Mt.write a msg with
   | Error _ -> ()
   | Ok () -> Alcotest.fail "write after close should fail")
in

(* ── multiple messages in order ──────────── *)

let test_message_ordering () =
  let a, b = Mt.create_pair () in
  let msgs = List.init 5 (fun i -> make_request (i + 1) (Printf.sprintf "method_%d" i)) in
  List.iter (fun msg ->
    match Mt.write a msg with
    | Ok () -> ()
    | Error e -> Alcotest.fail e
  ) msgs;
  List.iteri (fun i _msg ->
    match Mt.read b with
    | Some (Ok received) ->
      let expected_method = Printf.sprintf "method_%d" i in
      let j = Jsonrpc.message_to_yojson received in
      let method_ = match j with
        | `Assoc fields ->
          (match List.assoc_opt "method" fields with
           | Some (`String m) -> m
           | _ -> "")
        | _ -> ""
      in
      Alcotest.(check string) (Printf.sprintf "msg %d method" i) expected_method method_
    | _ -> Alcotest.fail (Printf.sprintf "expected message %d" i)
  ) msgs
in

(* ── close is idempotent ─────────────────── *)

let test_close_idempotent () =
  let a, _b = Mt.create_pair () in
  Mt.close a;
  Mt.close a;
  (* should not raise *)
  ()
in

(* ── test suite ──────────────────────────── *)

Alcotest.run "Memory_transport" [
  "pair", [
    Alcotest.test_case "write and read" `Quick test_write_read_pair;
    Alcotest.test_case "bidirectional" `Quick test_bidirectional;
    Alcotest.test_case "message ordering" `Quick test_message_ordering;
  ];
  "lifecycle", [
    Alcotest.test_case "close signals EOF" `Quick test_close_signals_eof;
    Alcotest.test_case "write after close" `Quick test_write_after_close;
    Alcotest.test_case "close idempotent" `Quick test_close_idempotent;
  ];
]
