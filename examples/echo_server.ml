(** Minimal MCP echo server using the Server framework.

    Demonstrates Tool_arg for type-safe argument extraction
    and ergonomic Server.tool / Server.resource registration.

    Usage:
      echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{...}}' | \
        dune exec examples/echo_server.exe
*)

open Mcp_protocol

let () =
  Eio_main.run @@ fun env ->
  let server =
    Mcp_protocol_eio.Server.create
      ~name:"echo-server" ~version:"0.14.0"
      ~instructions:"Echo server demonstrating Tool_arg and ergonomic API."
      ()
    |> Mcp_protocol_eio.Server.tool "echo" ~description:"Echoes back the input text"
         (fun _ctx _name args ->
           let open Tool_arg in
           let* text = required args "text" string in
           Ok (Mcp_types.tool_result_of_text ("Echo: " ^ text)))
    |> Mcp_protocol_eio.Server.tool "reverse" ~description:"Reverses the input text"
         (fun _ctx _name args ->
           let open Tool_arg in
           let* text = required args "text" string in
           let reversed = String.init (String.length text) (fun i ->
             text.[String.length text - 1 - i]) in
           Ok (Mcp_types.tool_result_of_text reversed))
    |> Mcp_protocol_eio.Server.resource ~uri:"mcp://info" "server-info"
         ~description:"Server version information" ~mime_type:"application/json"
         (fun _ctx _uri ->
           Ok [Mcp_types.{
             uri = "mcp://info";
             mime_type = Some "application/json";
             text = Some {|{"name":"echo-server","version":"0.14.0"}|};
             blob = None;
           }])
  in
  Mcp_protocol_eio.Server.run server
    ~stdin:(Eio.Stdenv.stdin env)
    ~stdout:(Eio.Stdenv.stdout env)
    ()
