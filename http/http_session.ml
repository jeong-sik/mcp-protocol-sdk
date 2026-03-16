(** HTTP session management for MCP Streamable HTTP transport. *)

(* ── State ───────────────────────────────────── *)

type state =
  | Uninitialized
  | Initializing
  | Ready
  | Closing
  | Closed

(* ── Session type ────────────────────────────── *)

(** M5 fix: event_counter uses Atomic.t for safe concurrent fiber access.
    H1 note: state/session_id remain mutable — within a single Eio domain,
    fiber scheduling is cooperative and these transitions are always
    sequenced by the HTTP request handling (one transition per request). *)
type t = {
  mutable state : state;
  mutable session_id : string option;
  event_counter : int Atomic.t;
}

let create () = {
  state = Uninitialized;
  session_id = None;
  event_counter = Atomic.make 0;
}

(* ── Session ID generation ───────────────────── *)

let generate_session_id () =
  let len = 16 in
  let buf = Bytes.create len in
  let read_urandom () =
    let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let rec loop off =
        if off < len then begin
          let n = Unix.read fd buf off (len - off) in
          if n <= 0 then failwith "generate_session_id: /dev/urandom short read";
          loop (off + n)
        end
      in
      loop 0)
  in
  let fallback_random () =
    Printf.eprintf "Warning: /dev/urandom unavailable, using Random for session ID\n%!";
    Random.self_init ();
    for i = 0 to len - 1 do
      Bytes.set buf i (Char.chr (Random.int 256))
    done
  in
  (try read_urandom ()
   with Unix.Unix_error _ | Sys_error _ -> fallback_random ());
  Bytes.to_seq buf
  |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> List.of_seq |> String.concat ""

let session_id t = t.session_id

(* ── State transitions ───────────────────────── *)

let state t = t.state

let initialize t =
  match t.state with
  | Uninitialized ->
    let sid = generate_session_id () in
    t.state <- Initializing;
    t.session_id <- Some sid;
    Ok sid
  | _ ->
    Error "Session already initialized"

let ready t =
  match t.state with
  | Initializing ->
    t.state <- Ready;
    Ok ()
  | _ ->
    Error "Session not in Initializing state"

let close t =
  t.state <- Closed

(* ── Validation ──────────────────────────────── *)

(** M1 note: When session_id is None (pre-init), we accept any request.
    This is necessary because the initialize request itself arrives before
    any session ID exists. The HTTP server layer (http_server.ml callback)
    ensures that non-initialize requests are rejected at the auth/routing
    level before reaching this point. *)
let validate t header_value =
  match t.session_id with
  | None ->
    Ok ()
  | Some sid ->
    match header_value with
    | None ->
      Error (`Bad_request "Missing Mcp-Session-Id header")
    | Some v ->
      if v = sid then Ok ()
      else Error `Not_found

(* ── Event IDs ───────────────────────────────── *)

(** M5 fix: Use Atomic.fetch_and_add for monotonically increasing IDs,
    safe even if called from concurrent fibers within the same domain. *)
let next_event_id t =
  let id = Atomic.fetch_and_add t.event_counter 1 in
  string_of_int id

(* ── Constants ───────────────────────────────── *)

let header_name = "mcp-session-id"
