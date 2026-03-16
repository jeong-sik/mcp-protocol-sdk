(** Minimal MCP echo server using the Server framework.

    Demonstrates ppx_deriving_jsonschema for automatic input_schema generation.

    Usage:
      echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{...}}' | \
        dune exec examples/echo_server.exe
*)

open Mcp_protocol

(** ppx_deriving_jsonschema generates [echo_input_jsonschema : Yojson.Safe.t]
    from this record type, eliminating manual JSON Schema construction. *)
type echo_input = {
  text: string;
} [@@deriving yojson, jsonschema]

let echo_tool =
  Mcp_types.make_tool
    ~name:"echo"
    ~description:"Echoes back the input text"
    ~input_schema:echo_input_jsonschema
    ()

let echo_handler _ctx _name arguments =
  let text =
    match arguments with
    | Some json ->
      begin match echo_input_of_yojson json with
      | Ok input -> input.text
      | Error _ -> "(invalid input)"
      end
    | None -> "(no arguments)"
  in
  Ok (Mcp_types.tool_result_of_text (Printf.sprintf "Echo: %s" text))

let () =
  Eio_main.run @@ fun env ->
  let server =
    Mcp_protocol_eio.Server.create
      ~name:"echo-server"
      ~version:"0.11.0"
      ~instructions:"A minimal echo server for testing the MCP Server framework."
      ()
    |> Mcp_protocol_eio.Server.add_tool echo_tool echo_handler
  in
  Mcp_protocol_eio.Server.run server
    ~stdin:(Eio.Stdenv.stdin env)
    ~stdout:(Eio.Stdenv.stdout env)
    ()
