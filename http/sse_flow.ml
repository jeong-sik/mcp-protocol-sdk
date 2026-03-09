(** Bridge between Sse.Broadcaster event stream and Eio.Flow.source.

    Internal buffer holds the wire-format bytes of the most recently
    pulled event.  When the buffer is exhausted, the next [single_read]
    blocks on [Eio.Stream.take] until a new event arrives. *)

type t = {
  stream : Sse.event Eio.Stream.t;
  mutable buf : string;
  mutable pos : int;
}

let create stream = { stream; buf = ""; pos = 0 }

(* ── Eio.Flow.Pi.SOURCE implementation ──────── *)

module Source = struct
  type nonrec t = t

  let read_methods = []

  let single_read t dst =
    (* Refill from stream when the internal buffer is exhausted. *)
    if t.pos >= String.length t.buf then begin
      let evt = Eio.Stream.take t.stream in
      t.buf <- Sse.encode evt;
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
