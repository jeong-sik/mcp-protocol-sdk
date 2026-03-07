(** Eio-based stdio transport for MCP.

    NDJSON (Newline-Delimited JSON) over stdin/stdout.
    One JSON-RPC message per line.
*)

type t = {
  reader: Eio.Buf_read.t;
  sink: Eio.Flow.sink_ty Eio.Resource.t;
  mutable closed: bool;
}

let create ~stdin ~stdout =
  let reader = Eio.Buf_read.of_flow stdin ~max_size:(1024 * 1024) in
  { reader; sink = (stdout :> Eio.Flow.sink_ty Eio.Resource.t); closed = false }

let rec read t =
  if t.closed then None
  else
    match Eio.Buf_read.line t.reader with
    | line ->
      let trimmed = String.trim line in
      if String.length trimmed = 0 then
        (* Skip blank lines, read next *)
        read t  (* Note: limited recursion depth, blank lines are rare *)
      else begin
        match Yojson.Safe.from_string trimmed with
        | json ->
          begin match Mcp_protocol.Jsonrpc.message_of_yojson json with
          | Ok msg -> Some (Ok msg)
          | Error e -> Some (Error (Printf.sprintf "JSON-RPC parse error: %s" e))
          end
        | exception Yojson.Json_error e ->
          Some (Error (Printf.sprintf "JSON parse error: %s" e))
      end
    | exception End_of_file -> None

let write t msg =
  if t.closed then Error "Transport is closed"
  else begin
    let json = Mcp_protocol.Jsonrpc.message_to_yojson msg in
    let line = Yojson.Safe.to_string json ^ "\n" in
    Eio.Flow.copy_string line t.sink;
    Ok ()
  end

let close t =
  t.closed <- true
