(** Minimal MCP client over HTTP that connects to the echo server.

    Usage:
      # Terminal 1: start server
      dune exec examples/http_echo_server.exe

      # Terminal 2: run client
      dune exec examples/http_echo_client.exe
*)

open Mcp_protocol_http

let () =
  let endpoint = "http://127.0.0.1:8080/mcp" in
  Printf.printf "Connecting to %s...\n%!" endpoint;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let client = Http_client.create ~endpoint ~net ~sw () in

  (* Initialize *)
  (match Http_client.initialize client
           ~client_name:"http-echo-client" ~client_version:"0.14.0" with
   | Ok result ->
     Printf.printf "Connected to %s v%s\n%!"
       result.server_info.name result.server_info.version
   | Error e ->
     Printf.eprintf "Init failed: %s\n%!" e; exit 1);

  (* List tools *)
  (match Http_client.list_tools client with
   | Ok tools ->
     Printf.printf "Available tools: %s\n%!"
       (String.concat ", "
          (List.map (fun (t : Mcp_protocol.Mcp_types.tool) -> t.name) tools))
   | Error e ->
     Printf.eprintf "List tools failed: %s\n%!" e);

  (* Call echo tool *)
  let args = `Assoc [("text", `String "Hello from HTTP client!")] in
  (match Http_client.call_tool client ~name:"echo" ~arguments:args () with
   | Ok result ->
     (match result.content with
      | [Mcp_protocol.Mcp_types.TextContent { text; _ }] ->
        Printf.printf "Echo response: %s\n%!" text
      | _ ->
        Printf.printf "Unexpected response format\n%!")
   | Error e ->
     Printf.eprintf "Call tool failed: %s\n%!" e);

  (* Ping *)
  (match Http_client.ping client with
   | Ok () -> Printf.printf "Ping: OK\n%!"
   | Error e -> Printf.eprintf "Ping failed: %s\n%!" e);

  (* Close *)
  (match Http_client.close client with
   | Ok () -> Printf.printf "Session closed.\n%!"
   | Error e -> Printf.eprintf "Close failed: %s\n%!" e)
