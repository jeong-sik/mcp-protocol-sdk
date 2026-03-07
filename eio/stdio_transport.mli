(** Eio-based stdio transport for MCP.

    Implements [Mcp_protocol.Transport.S] using NDJSON over stdin/stdout.
    Each line on stdin is a JSON-RPC message; each outgoing message is
    written as a single JSON line followed by a newline to stdout.
*)

type t

(** Create a stdio transport from Eio flow endpoints.

    @param stdin    Source to read JSON-RPC messages from (one per line).
    @param stdout   Sink to write JSON-RPC messages to (one per line).
    @param max_size Maximum buffer size in bytes (default: 1 MB).
*)
val create :
  stdin:_ Eio.Flow.source ->
  stdout:_ Eio.Flow.sink ->
  ?max_size:int ->
  unit ->
  t

(** Read the next JSON-RPC message from stdin.

    Returns [Some (Ok msg)] on success, [Some (Error reason)] on parse
    failure, or [None] on EOF.
*)
val read : t -> (Mcp_protocol.Jsonrpc.message, string) result option

(** Write a JSON-RPC message as a single JSON line to stdout.

    Returns [Ok ()] on success, [Error reason] on serialization failure.
*)
val write : t -> Mcp_protocol.Jsonrpc.message -> (unit, string) result

(** Close the transport. Currently a no-op for stdio, but satisfies
    the [Transport.S] interface. *)
val close : t -> unit
