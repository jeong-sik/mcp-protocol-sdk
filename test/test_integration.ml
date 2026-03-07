(** Integration tests: Server ↔ Client via Eio pipes. *)

open Mcp_protocol

(* ── server builders ─────────────────────────────────── *)

let echo_tool : Mcp_types.tool = {
  name = "echo";
  description = Some "Returns the input text";
  input_schema = `Assoc [("type", `String "object")];
  title = None;
  annotations = None;
}

let test_resource : Mcp_types.resource = {
  uri = "file:///hello.txt";
  name = "hello.txt";
  description = Some "A test file";
  mime_type = Some "text/plain";
}

let greet_prompt : Mcp_types.prompt = {
  name = "greet";
  description = Some "Greeting prompt";
  arguments = Some [{ name = "name"; description = None; required = Some true }];
}

let make_server () =
  Mcp_protocol_eio.Server.create ~name:"integration-server" ~version:"0.6.0" ()
  |> Mcp_protocol_eio.Server.add_tool echo_tool (fun _ctx _name args ->
    let text = match args with
      | Some (`Assoc fields) ->
        begin match List.assoc_opt "text" fields with
        | Some (`String s) -> s
        | _ -> "no text"
        end
      | _ -> "no args"
    in
    Ok (Mcp_types.tool_result_of_text ("Echo: " ^ text)))
  |> Mcp_protocol_eio.Server.add_resource test_resource (fun _ctx _uri ->
    Ok [{ Mcp_types.uri = "file:///hello.txt"; mime_type = Some "text/plain";
          text = Some "hello world"; blob = None }])
  |> Mcp_protocol_eio.Server.add_prompt greet_prompt (fun _ctx _name args ->
    let who = match List.assoc_opt "name" args with Some v -> v | None -> "stranger" in
    Ok { Mcp_types.description = None;
         messages = [{ role = Mcp_types.Assistant;
                       content = Mcp_types.PromptText { type_ = "text"; text = "Hello, " ^ who } }] })

(** Run a full server-client test.
    Creates two pipes, runs server in one fiber and client callback in another.
    Closes client-to-server pipe when done to trigger server EOF. *)
let run_integration fn =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Pipe A: client → server *)
  let c2s_r, c2s_w = Eio_unix.pipe sw in
  (* Pipe B: server → client *)
  let s2c_r, s2c_w = Eio_unix.pipe sw in
  let server = make_server () in
  let result = ref (Error "test did not run") in
  let clock = Eio.Stdenv.clock env in
  Eio.Fiber.both
    (fun () ->
      Mcp_protocol_eio.Server.run server ~stdin:c2s_r ~stdout:s2c_w)
    (fun () ->
      let client = Mcp_protocol_eio.Client.create ~clock ~stdin:s2c_r ~stdout:c2s_w () in
      result := fn client;
      Mcp_protocol_eio.Client.close client;
      (* Close write end so server sees EOF *)
      Eio.Flow.close c2s_w);
  !result

(* ── tests ───────────────────────────────────────────── *)

let test_full_lifecycle () =
  match run_integration (fun client ->
    match Mcp_protocol_eio.Client.initialize client
      ~client_name:"test-client" ~client_version:"1.0" with
    | Error e -> Error e
    | Ok init ->
      if init.server_info.name <> "integration-server" then
        Error "wrong server name"
      else
        match Mcp_protocol_eio.Client.list_tools client with
        | Error e -> Error e
        | Ok tools ->
          if List.length tools <> 1 then
            Error "expected 1 tool"
          else if (List.hd tools).name <> "echo" then
            Error "wrong tool name"
          else
            let args = `Assoc [("text", `String "integration")] in
            match Mcp_protocol_eio.Client.call_tool client ~name:"echo" ~arguments:args () with
            | Error e -> Error e
            | Ok result ->
              match result.content with
              | [Mcp_types.TextContent t] when t.text = "Echo: integration" -> Ok ()
              | _ -> Error "unexpected tool result"
  ) with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

let test_resources_roundtrip () =
  match run_integration (fun client ->
    match Mcp_protocol_eio.Client.initialize client
      ~client_name:"test" ~client_version:"1.0" with
    | Error e -> Error e
    | Ok _ ->
      match Mcp_protocol_eio.Client.list_resources client with
      | Error e -> Error e
      | Ok resources ->
        if List.length resources <> 1 then Error "expected 1 resource"
        else if (List.hd resources).uri <> "file:///hello.txt" then Error "wrong uri"
        else
          match Mcp_protocol_eio.Client.read_resource client ~uri:"file:///hello.txt" with
          | Error e -> Error e
          | Ok contents ->
            let c = List.hd contents in
            if c.uri <> "file:///hello.txt" then Error "wrong content uri"
            else if c.text <> Some "hello world" then Error "wrong text"
            else Ok ()
  ) with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

let test_prompts_roundtrip () =
  match run_integration (fun client ->
    match Mcp_protocol_eio.Client.initialize client
      ~client_name:"test" ~client_version:"1.0" with
    | Error e -> Error e
    | Ok _ ->
      match Mcp_protocol_eio.Client.list_prompts client with
      | Error e -> Error e
      | Ok prompts ->
        if List.length prompts <> 1 then Error "expected 1 prompt"
        else if (List.hd prompts).name <> "greet" then Error "wrong name"
        else
          match Mcp_protocol_eio.Client.get_prompt client ~name:"greet"
            ~arguments:[("name", "World")] () with
          | Error e -> Error e
          | Ok result ->
            if List.length result.messages <> 1 then Error "expected 1 message"
            else Ok ()
  ) with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

let test_call_unknown_tool () =
  match run_integration (fun client ->
    match Mcp_protocol_eio.Client.initialize client
      ~client_name:"test" ~client_version:"1.0" with
    | Error e -> Error e
    | Ok _ ->
      match Mcp_protocol_eio.Client.call_tool client ~name:"nonexistent" () with
      | Ok _ -> Error "expected error for unknown tool"
      | Error _ -> Ok ()
  ) with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

let test_ping_after_init () =
  match run_integration (fun client ->
    match Mcp_protocol_eio.Client.initialize client
      ~client_name:"test" ~client_version:"1.0" with
    | Error e -> Error e
    | Ok _ -> Mcp_protocol_eio.Client.ping client
  ) with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

(* ── test suite ──────────────────────────────────────── *)

let () =
  Alcotest.run "Integration" [
    "lifecycle", [
      Alcotest.test_case "full" `Quick test_full_lifecycle;
      Alcotest.test_case "ping after init" `Quick test_ping_after_init;
    ];
    "tools", [
      Alcotest.test_case "unknown tool" `Quick test_call_unknown_tool;
    ];
    "resources", [
      Alcotest.test_case "roundtrip" `Quick test_resources_roundtrip;
    ];
    "prompts", [
      Alcotest.test_case "roundtrip" `Quick test_prompts_roundtrip;
    ];
  ]
