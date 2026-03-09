(** Bridge between Sse.Broadcaster event stream and Eio.Flow.source.

    Internal buffer holds the wire-format bytes of the most recently
    pulled event.  When the buffer is exhausted, the next [single_read]
    blocks on [Eio.Stream.take] until a new event arrives. *)

(* Invariant: single-reader. Only one fiber should call single_read. *)
type t = {
  stream : Sse.event option Eio.Stream.t;
  sleep : (float -> unit) option;
  ping_interval : float;
  mutable buf : string;
  mutable pos : int;
}

let create ?sleep ?(ping_interval = 30.0) stream =
  { stream; sleep; ping_interval; buf = ""; pos = 0 }

(* Result of attempting to pull the next chunk from the stream. *)
type take_result = Event of Sse.event | Eof | Ping

let take_next t =
  match t.sleep with
  | None ->
    (* No clock: block indefinitely on stream. *)
    (match Eio.Stream.take t.stream with
     | None -> Eof
     | Some evt -> Event evt)
  | Some sleep ->
    (* Race stream take against ping timeout. *)
    Eio.Fiber.first
      (fun () ->
        match Eio.Stream.take t.stream with
        | None -> Eof
        | Some evt -> Event evt)
      (fun () ->
        sleep t.ping_interval;
        Ping)

(* ── Eio.Flow.Pi.SOURCE implementation ──────── *)

module Source = struct
  type nonrec t = t

  let read_methods = []

  let single_read t dst =
    (* Refill from stream when the internal buffer is exhausted. *)
    if t.pos >= String.length t.buf then begin
      match take_next t with
      | Eof -> raise End_of_file
      | Event evt ->
        t.buf <- Sse.encode evt;
        t.pos <- 0
      | Ping ->
        t.buf <- Sse.ping;
        t.pos <- 0
    end;
    let available = String.length t.buf - t.pos in
    let n = min available (Cstruct.length dst) in
    Cstruct.blit_from_string t.buf t.pos dst 0 n;
    t.pos <- t.pos + n;
    n
end

let handler = Eio.Flow.Pi.source (module Source)

let as_source t = Eio.Resource.T (t, handler)
