(** Transport middleware functors.

    Middleware wraps a transport to add cross-cutting concerns.
    Compose by chaining: [Middleware.Logging(My_transport)]. *)

(** Logs all read/written messages to stderr. *)
module Logging (T : Mcp_protocol.Transport.S) : sig
  include Mcp_protocol.Transport.S

  (** Wrap a transport with logging.
      @param label Prefix for log lines (default: ["mcp"]). *)
  val wrap : ?label:string -> T.t -> t
end
