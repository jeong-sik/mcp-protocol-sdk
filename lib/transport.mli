(** Transport layer abstraction for MCP.

    Defines the module type signature that transport implementations must satisfy.
    Concrete transports are provided by separate libraries (e.g., [mcp_protocol_eio]).
*)

module type S = sig
  type t
  val read : t -> (Jsonrpc.message, string) result option
  val write : t -> Jsonrpc.message -> (unit, string) result
  val close : t -> unit
end
