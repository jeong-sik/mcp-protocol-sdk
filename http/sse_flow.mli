(** Bridge between Sse.Broadcaster event stream and Eio.Flow.source.

    Converts SSE events from a bounded stream into a byte flow
    suitable for cohttp-eio streaming responses.  Events are pulled
    on demand: [single_read] blocks until the next event arrives,
    encodes it to the SSE wire format, and fills the caller's buffer.

    When [sleep] and [ping_interval] are provided, an idle connection
    emits SSE comment pings to prevent proxy/load-balancer timeouts. *)

type t

(** [create ?sleep ?ping_interval stream] wraps an event stream as a
    flow source.  If [sleep] is provided, a [": ping"] comment is
    emitted after [ping_interval] seconds of inactivity (default 30s). *)
val create :
  ?sleep:(float -> unit) ->
  ?ping_interval:float ->
  Sse.event option Eio.Stream.t -> t

(** [as_source t] returns the flow source for use with
    [Cohttp_eio.Server.respond ~body]. *)
val as_source : t -> Eio.Flow.source_ty Eio.Resource.t
