(** Minimal MCP client that connects to echo_server via subprocess.

    Usage:
      dune exec examples/echo_client.exe
*)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Create pipes for child process I/O *)
  let child_stdin_r, child_stdin_w = Eio_unix.pipe sw in
  let child_stdout_r, child_stdout_w = Eio_unix.pipe sw in
  (* Spawn echo_server as subprocess *)
  let _proc = Eio.Process.spawn ~sw
    (Eio.Stdenv.process_mgr env)
    ~stdin:child_stdin_r
    ~stdout:child_stdout_w
    ["dune"; "exec"; "--root"; "."; "--"; "examples/echo_server.exe"]
  in
  (* Close child-side pipe ends in parent *)
  Eio.Flow.close child_stdin_r;
  Eio.Flow.close child_stdout_w;
  (* Create MCP client *)
  let clock = Eio.Stdenv.clock env in
  let client = Mcp_protocol_eio.Client.create ~clock
    ~stdin:child_stdout_r ~stdout:child_stdin_w ()
  in
  (* Initialize *)
  begin match Mcp_protocol_eio.Client.initialize client
    ~client_name:"echo-client" ~client_version:"0.6.0" with
  | Error e ->
    Printf.eprintf "Initialize failed: %s\n" e
  | Ok result ->
    Printf.printf "Connected to: %s %s\n"
      result.server_info.name result.server_info.version;
    (* List tools *)
    begin match Mcp_protocol_eio.Client.list_tools client with
    | Error e -> Printf.eprintf "list_tools failed: %s\n" e
    | Ok tools ->
      Printf.printf "Tools: %s\n"
        (String.concat ", " (List.map (fun (t : Mcp_protocol.Mcp_types.tool) -> t.name) tools))
    end;
    (* Call echo tool *)
    let args = `Assoc [("text", `String "Hello from client!")] in
    begin match Mcp_protocol_eio.Client.call_tool client ~name:"echo" ~arguments:args () with
    | Error e -> Printf.eprintf "call_tool failed: %s\n" e
    | Ok result ->
      let texts = List.filter_map (fun c ->
        match (c : Mcp_protocol.Mcp_types.tool_content) with
        | TextContent t -> Some t.text | _ -> None
      ) result.content in
      Printf.printf "Result: %s\n" (String.concat "\n" texts)
    end;
  end;
  Mcp_protocol_eio.Client.close client
