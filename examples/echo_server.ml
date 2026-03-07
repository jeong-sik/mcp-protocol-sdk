(** Minimal MCP echo server using the Server framework.

    Usage:
      echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{...}}' | \
        dune exec examples/echo_server.exe
*)

open Mcp_protocol

let echo_tool =
  Mcp_types.make_tool
    ~name:"echo"
    ~description:"Echoes back the input text"
    ~input_schema:(`Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("text", `Assoc [("type", `String "string"); ("description", `String "Text to echo")])
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
  Eio_main.run @@ fun env ->
  let server =
    Mcp_protocol_eio.Server.create
      ~name:"echo-server"
      ~version:"0.6.0"
      ~instructions:"A minimal echo server for testing the MCP Server framework."
      ()
    |> Mcp_protocol_eio.Server.add_tool echo_tool echo_handler
  in
  Mcp_protocol_eio.Server.run server
    ~stdin:(Eio.Stdenv.stdin env)
    ~stdout:(Eio.Stdenv.stdout env)
