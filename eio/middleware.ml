(** Transport middleware functors.

    Middleware wraps a transport to add cross-cutting concerns (logging,
    metrics, validation) without modifying the transport or server.

    Compose by chaining functors:
    {[
      module Logged_stdio = Middleware.Logging(Stdio_transport)
      module Server = Generic_server.Make(Logged_stdio)
    ]}
*)

(** {2 Logging Middleware}

    Logs every message read/written to stderr with a configurable label.
    Useful for debugging MCP protocol exchanges. *)

module Logging (T : Mcp_protocol.Transport.S) : sig
  include Mcp_protocol.Transport.S

  (** Wrap a transport with logging. Messages are printed to stderr
      with the given [label] prefix (default: ["mcp"]). *)
  val wrap : ?label:string -> T.t -> t
end = struct
  type t = {
    inner: T.t;
    label: string;
  }

  let wrap ?(label = "mcp") inner = { inner; label }

  let read t =
    let result = T.read t.inner in
    (match result with
     | Some (Ok msg) ->
       let json = Mcp_protocol.Jsonrpc.message_to_yojson msg in
       Printf.eprintf "[%s] <- %s\n%!" t.label (Yojson.Safe.to_string json)
     | Some (Error e) ->
       Printf.eprintf "[%s] <- ERROR: %s\n%!" t.label e
     | None ->
       Printf.eprintf "[%s] <- EOF\n%!" t.label);
    result

  let write t msg =
    let json = Mcp_protocol.Jsonrpc.message_to_yojson msg in
    Printf.eprintf "[%s] -> %s\n%!" t.label (Yojson.Safe.to_string json);
    T.write t.inner msg

  let close t =
    Printf.eprintf "[%s] CLOSE\n%!" t.label;
    T.close t.inner
end
