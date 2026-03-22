(** In-memory transport for testing MCP servers and clients.

    Creates paired transports that communicate via Eio streams,
    with no serialization overhead — messages pass as OCaml values.
    Sending [None] through the stream signals EOF to the peer.

    Usage:
    {[
      Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun _sw ->
          let client_t, server_t = Memory_transport.create_pair () in
          (* Use server_t in Server.run, client_t for sending requests *)
    ]}
*)

type t = {
  inbox: Mcp_protocol.Jsonrpc.message option Eio.Stream.t;
  outbox: Mcp_protocol.Jsonrpc.message option Eio.Stream.t;
  mutable closed: bool;
}

let create_pair () =
  let a_to_b = Eio.Stream.create 64 in
  let b_to_a = Eio.Stream.create 64 in
  let a = { inbox = b_to_a; outbox = a_to_b; closed = false } in
  let b = { inbox = a_to_b; outbox = b_to_a; closed = false } in
  (a, b)

let read t =
  if t.closed then None
  else
    match Eio.Stream.take t.inbox with
    | Some msg -> Some (Ok msg)
    | None ->
      t.closed <- true;
      None

let write t msg =
  if t.closed then Error "Transport is closed"
  else begin
    Eio.Stream.add t.outbox (Some msg);
    Ok ()
  end

let close t =
  if not t.closed then begin
    t.closed <- true;
    Eio.Stream.add t.outbox None
  end
