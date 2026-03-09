(** Bridge between Sse.Broadcaster event stream and Eio.Flow.source.

    Converts SSE events from a bounded stream into a byte flow
    suitable for cohttp-eio streaming responses.  Events are pulled
    on demand: [single_read] blocks until the next event arrives,
    encodes it to the SSE wire format, and fills the caller's buffer. *)

type t

(** [create stream] wraps an event stream as a flow source.
    Events are encoded to SSE wire format via {!Sse.encode} on demand. *)
val create : Sse.event Eio.Stream.t -> t

(** [as_source t] returns the flow source for use with
    [Cohttp_eio.Server.respond ~body]. *)
val as_source : t -> Eio.Flow.source_ty Eio.Resource.t
