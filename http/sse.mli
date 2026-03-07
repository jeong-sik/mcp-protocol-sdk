(** Server-Sent Events (SSE) encoding, decoding, and broadcasting.

    Provides typed SSE events, wire format encoding/parsing,
    and a thread-safe Broadcaster for pushing events to multiple
    connected clients via bounded Eio streams. *)

(** {2 Event Type} *)

(** An SSE event with optional fields. *)
type event = {
  event_type : string option;
  data : string;
  id : string option;
  retry : int option;
}

(** {2 Constructors} *)

(** [data s] creates an event with only the data field set. *)
val data : string -> event

(** [event typ s] creates an event with event type and data. *)
val event : string -> string -> event

(** [with_id id e] returns [e] with its id field set to [id]. *)
val with_id : string -> event -> event

(** [with_retry ms e] returns [e] with its retry field set to [ms] milliseconds. *)
val with_retry : int -> event -> event

(** {2 Encoding} *)

(** [encode e] serializes an event to the SSE wire format.
    Multi-line data is split into separate [data:] lines. *)
val encode : event -> string

(** {2 Parsing} *)

(** [parse_event s] attempts to parse an SSE-formatted string into an event.
    Returns [None] if no [data:] line is found. *)
val parse_event : string -> event option

(** {2 Utilities} *)

(** SSE keep-alive comment line: [": ping\n\n"]. *)
val ping : string

(** {2 Broadcaster}

    Thread-safe multi-client event broadcaster using bounded Eio streams. *)

module Broadcaster : sig
  type t

  (** Create a new broadcaster. *)
  val create : unit -> t

  (** [subscribe t] registers a new client, returning [(id, stream)].
      The client receives future broadcasts on the returned stream. *)
  val subscribe : t -> int * event Eio.Stream.t

  (** [unsubscribe t id] removes the client with the given [id]. *)
  val unsubscribe : t -> int -> unit

  (** [broadcast t event] sends [event] to all connected clients. *)
  val broadcast : t -> event -> unit

  (** [client_count t] returns the number of connected clients. *)
  val client_count : t -> int
end
