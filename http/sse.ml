(** Server-Sent Events (SSE) encoding, decoding, and broadcasting. *)

(* ── Event type ──────────────────────────────── *)

type event = {
  event_type : string option;
  data : string;
  id : string option;
  retry : int option;
}

(* ── Constructors ────────────────────────────── *)

let data d = { event_type = None; data = d; id = None; retry = None }
let event t d = { event_type = Some t; data = d; id = None; retry = None }
let with_id id e = { e with id = Some id }
let with_retry ms e = { e with retry = Some ms }

(* ── Encoding ────────────────────────────────── *)

let encode e =
  let buf = Buffer.create 64 in
  (* 1. event type *)
  (match e.event_type with
   | Some t -> Buffer.add_string buf ("event: " ^ t ^ "\n")
   | None -> ());
  (* 2. data — handle multi-line by splitting *)
  String.split_on_char '\n' e.data
  |> List.iter (fun line ->
       Buffer.add_string buf ("data: " ^ line ^ "\n"));
  (* 3. id *)
  (match e.id with
   | Some id -> Buffer.add_string buf ("id: " ^ id ^ "\n")
   | None -> ());
  (* 4. retry *)
  (match e.retry with
   | Some ms -> Buffer.add_string buf ("retry: " ^ string_of_int ms ^ "\n")
   | None -> ());
  (* trailing blank line to terminate event *)
  Buffer.add_string buf "\n";
  Buffer.contents buf

(* ── Parsing ─────────────────────────────────── *)

let starts_with ~prefix s =
  String.length s >= String.length prefix &&
  String.sub s 0 (String.length prefix) = prefix

let strip_prefix ~prefix s =
  String.sub s (String.length prefix) (String.length s - String.length prefix)

let parse_event s =
  let lines = String.split_on_char '\n' s in
  let event_type = ref None in
  let data_lines = ref [] in
  let id = ref None in
  let retry = ref None in
  List.iter (fun line ->
    if starts_with ~prefix:"data: " line then
      data_lines := strip_prefix ~prefix:"data: " line :: !data_lines
    else if starts_with ~prefix:"event: " line then
      event_type := Some (strip_prefix ~prefix:"event: " line)
    else if starts_with ~prefix:"id: " line then
      id := Some (strip_prefix ~prefix:"id: " line)
    else if starts_with ~prefix:"retry: " line then
      let v = strip_prefix ~prefix:"retry: " line in
      (try retry := Some (int_of_string v) with Failure _ -> ())
  ) lines;
  match !data_lines with
  | [] -> None
  | _ ->
    let data = String.concat "\n" (List.rev !data_lines) in
    Some { event_type = !event_type; data; id = !id; retry = !retry }

(* ── Utilities ───────────────────────────────── *)

let ping = ": ping\n\n"

(* ── Broadcaster ─────────────────────────────── *)

module Broadcaster = struct
  type t = {
    clients : (int, event Eio.Stream.t) Hashtbl.t;
    next_id : int Atomic.t;
    mutex : Eio.Mutex.t;
  }

  let create () = {
    clients = Hashtbl.create 16;
    next_id = Atomic.make 0;
    mutex = Eio.Mutex.create ();
  }

  let subscribe t =
    let id = Atomic.fetch_and_add t.next_id 1 in
    let stream = Eio.Stream.create 100 in
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      Hashtbl.add t.clients id stream
    );
    (id, stream)

  let unsubscribe t id =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      Hashtbl.remove t.clients id
    )

  let broadcast t evt =
    Eio.Mutex.use_ro t.mutex (fun () ->
      Hashtbl.iter (fun _id stream ->
        Eio.Stream.add stream evt
      ) t.clients
    )

  let client_count t =
    Eio.Mutex.use_ro t.mutex (fun () ->
      Hashtbl.length t.clients
    )
end
