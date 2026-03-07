(** Transport layer abstraction for MCP.

    Defines the module type signature that transport implementations must satisfy.
    This module contains no implementation — concrete transports (e.g., stdio)
    are provided by [mcp_protocol_eio] in a separate library.
*)

(** Transport module type. *)
module type S = sig
  type t

  (** Read the next JSON-RPC message.
      Returns [None] on EOF, [Some (Ok msg)] on success,
      or [Some (Error reason)] on parse/read failure. *)
  val read : t -> (Jsonrpc.message, string) result option

  (** Write a JSON-RPC message.
      Returns [Ok ()] on success or [Error reason] on write failure. *)
  val write : t -> Jsonrpc.message -> (unit, string) result

  (** Close the transport. *)
  val close : t -> unit
end
