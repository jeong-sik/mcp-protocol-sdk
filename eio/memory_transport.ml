(** In-memory transport for testing MCP servers and clients.

    Creates paired transports that communicate via Eio streams,
    with no serialization overhead — messages pass as OCaml values.
    Sending [None] through the stream signals EOF to the peer.

    Fiber-safe: the [closed] flag uses [Atomic.t] to prevent TOCTOU
    races when read/write/close are called from different fibers.

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
  closed: bool Atomic.t;
}

let create_pair () =
  let a_to_b = Eio.Stream.create 64 in
  let b_to_a = Eio.Stream.create 64 in
  let a = { inbox = b_to_a; outbox = a_to_b; closed = Atomic.make false } in
  let b = { inbox = a_to_b; outbox = b_to_a; closed = Atomic.make false } in
  (a, b)

let read t =
  if Atomic.get t.closed then None
  else
    match Eio.Stream.take t.inbox with
    | Some msg -> Some (Ok msg)
    | None ->
      Atomic.set t.closed true;
      None

let write t msg =
  if Atomic.get t.closed then Error "Transport is closed"
  else begin
    Eio.Stream.add t.outbox (Some msg);
    Ok ()
  end

let close t =
  if Atomic.compare_and_set t.closed false true then
    Eio.Stream.add t.outbox None
