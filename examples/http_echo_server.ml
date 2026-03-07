(** Minimal MCP echo server over HTTP (Streamable HTTP transport).

    Usage:
      dune exec examples/http_echo_server.exe

    Then connect with an MCP client:
      curl -X POST http://localhost:8080/mcp \
        -H 'Content-Type: application/json' \
        -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-05","capabilities":{},"clientInfo":{"name":"curl","version":"1.0"}}}'
*)

open Mcp_protocol

let echo_tool =
  Mcp_types.make_tool
    ~name:"echo"
    ~description:"Echoes back the input text"
    ~input_schema:(`Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("text", `Assoc [
          ("type", `String "string");
          ("description", `String "Text to echo")])
      ]);
      ("required", `List [`String "text"])
    ])
    ()

let echo_handler _ctx _name arguments =
  let text =
    match arguments with
    | Some (`Assoc args) ->
      begin match List.assoc_opt "text" args with
      | Some (`String s) -> s
      | _ -> "(no text provided)"
      end
    | _ -> "(no arguments)"
  in
  Ok (Mcp_types.tool_result_of_text (Printf.sprintf "Echo: %s" text))

let () =
  let port = 8080 in
  Printf.printf "Starting HTTP MCP echo server on port %d...\n%!" port;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server =
    Mcp_protocol_http.Http_server.create
      ~name:"http-echo-server"
      ~version:"0.9.0"
      ()
    |> Mcp_protocol_http.Http_server.add_tool echo_tool echo_handler
  in
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let socket = Eio.Net.listen ~sw net addr ~backlog:128 in
  let cohttp_server = Cohttp_eio.Server.make
    ~callback:(fun conn request body ->
      Mcp_protocol_http.Http_server.callback server conn request body)
    ()
  in
  Printf.printf "Listening on http://127.0.0.1:%d/mcp\n%!" port;
  Cohttp_eio.Server.run socket cohttp_server
    ~on_error:(fun exn ->
      Printf.eprintf "Server error: %s\n%!" (Printexc.to_string exn))
