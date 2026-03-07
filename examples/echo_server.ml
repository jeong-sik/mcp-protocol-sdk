(** Minimal MCP echo server over stdio.

    Handles: initialize, ping, tools/list, tools/call (echo tool),
    notifications/initialized, and shutdown.

    Usage:
      echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{...}}' | \
        dune exec examples/echo_server.exe
*)

open Mcp_protocol

let server_name = "echo-server"
let server_version = "0.4.0"

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

let handle_initialize id =
  let result = Mcp_types.{
    protocol_version = Version.latest;
    capabilities = {
      tools = Some (`Assoc [("listChanged", `Bool false)]);
      resources = None;
      prompts = None;
      logging = None;
      experimental = None;
    };
    server_info = { name = server_name; version = server_version };
    instructions = Some "A minimal echo server for testing MCP transport.";
  } in
  Jsonrpc.make_response ~id ~result:(Mcp_types.initialize_result_to_yojson result)

let handle_tools_list id =
  let tools_json = `Assoc [
    ("tools", `List [Mcp_types.tool_to_yojson echo_tool]);
  ] in
  Jsonrpc.make_response ~id ~result:tools_json

let handle_tools_call id params =
  let text =
    match params with
    | Some (`Assoc fields) ->
      begin match List.assoc_opt "arguments" fields with
      | Some (`Assoc args) ->
        begin match List.assoc_opt "text" args with
        | Some (`String s) -> s
        | _ -> "(no text provided)"
        end
      | _ -> "(no arguments)"
      end
    | _ -> "(no params)"
  in
  let result = Mcp_types.tool_result_of_text (Printf.sprintf "Echo: %s" text) in
  Jsonrpc.make_response ~id ~result:(Mcp_types.tool_result_to_yojson result)

let handle_ping id =
  Jsonrpc.make_response ~id ~result:(`Assoc [])

let dispatch (msg : Jsonrpc.message) : Jsonrpc.message option =
  match msg with
  | Request req ->
    let response = match req.method_ with
      | "initialize" -> handle_initialize req.id
      | "ping" -> handle_ping req.id
      | "tools/list" -> handle_tools_list req.id
      | "tools/call" -> handle_tools_call req.id req.params
      | _ ->
        Jsonrpc.make_error ~id:req.id
          ~code:Error_codes.method_not_found
          ~message:(Printf.sprintf "Unknown method: %s" req.method_) ()
    in
    Some response
  | Notification notif ->
    begin match notif.method_ with
    | "notifications/initialized" -> ()
    | "notifications/cancelled" -> ()
    | _ ->
      Printf.eprintf "[echo-server] Unknown notification: %s\n%!" notif.method_
    end;
    None
  | Response _ ->
    Printf.eprintf "[echo-server] Unexpected response message\n%!";
    None
  | Error _ ->
    Printf.eprintf "[echo-server] Unexpected error message\n%!";
    None

let () =
  Eio_main.run @@ fun env ->
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  let transport = Mcp_protocol_eio.Stdio_transport.create ~stdin ~stdout in
  Printf.eprintf "[echo-server] Started (MCP %s)\n%!" Version.latest;
  let rec loop () =
    match Mcp_protocol_eio.Stdio_transport.read transport with
    | None ->
      Printf.eprintf "[echo-server] EOF, shutting down\n%!"
    | Some (Error e) ->
      Printf.eprintf "[echo-server] Read error: %s\n%!" e;
      loop ()
    | Some (Ok msg) ->
      begin match dispatch msg with
      | Some response ->
        begin match Mcp_protocol_eio.Stdio_transport.write transport response with
        | Ok () -> ()
        | Error e -> Printf.eprintf "[echo-server] Write error: %s\n%!" e
        end
      | None -> ()
      end;
      loop ()
  in
  loop ();
  Mcp_protocol_eio.Stdio_transport.close transport
