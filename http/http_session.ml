(** HTTP session management for MCP Streamable HTTP transport. *)

(* ── State ───────────────────────────────────── *)

type state =
  | Uninitialized
  | Initializing
  | Ready
  | Closing
  | Closed

(* ── Session type ────────────────────────────── *)

type t = {
  mutable state : state;
  mutable session_id : string option;
  mutable event_counter : int;
}

let create () = {
  state = Uninitialized;
  session_id = None;
  event_counter = 0;
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

let validate t header_value =
  match t.session_id with
  | None ->
    (* No session ID yet — before initialization, accept anything *)
    Ok ()
  | Some sid ->
    match header_value with
    | None ->
      Error (`Bad_request "Missing Mcp-Session-Id header")
    | Some v ->
      if v = sid then Ok ()
      else Error `Not_found

(* ── Event IDs ───────────────────────────────── *)

let next_event_id t =
  let id = t.event_counter in
  t.event_counter <- id + 1;
  string_of_int id

(* ── Constants ───────────────────────────────── *)

let header_name = "mcp-session-id"
