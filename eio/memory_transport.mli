(** In-memory transport for testing.

    Creates paired transports that communicate directly via Eio streams.
    No serialization/deserialization overhead — messages are passed as values.

    {[
      let client_t, server_t = Memory_transport.create_pair () in
      (* Messages written to client_t are read from server_t, and vice versa *)
    ]}
*)

(** Transport handle. Satisfies {!Mcp_protocol.Transport.S}. *)
type t

(** Create a connected pair of transports.
    Messages written to the first appear in the second's [read], and vice versa.
    Closing one side signals EOF to the other. *)
val create_pair : unit -> t * t

(** Read the next message. Returns [None] on EOF or after close. *)
val read : t -> (Mcp_protocol.Jsonrpc.message, string) result option

(** Write a message. Returns [Error] if the transport is closed. *)
val write : t -> Mcp_protocol.Jsonrpc.message -> (unit, string) result

(** Close the transport and signal EOF to the peer. *)
val close : t -> unit
