(** MCP conformance test server.

    A server that registers tools, resources, prompts, and completions
    to exercise the full MCP protocol surface for conformance testing.

    Usage:
      dune exec examples/conformance_server.exe
      # Then in another terminal:
      npx @modelcontextprotocol/conformance server \
        --url http://localhost:9100/mcp

    The server listens on port 9100 (or MCP_CONFORMANCE_PORT env var).
*)

open Mcp_protocol

let () =
  let port =
    match Sys.getenv_opt "MCP_CONFORMANCE_PORT" with
    | Some p -> int_of_string p
    | None -> 9100
  in
  Printf.printf "Starting MCP conformance server on port %d...\n%!" port;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server =
    Mcp_protocol_http.Http_server.create
      ~name:"mcp-conformance-server"
      ~version:"0.16.0"
      ~instructions:"Conformance test server for MCP protocol SDK."
      ()
    (* Tool: echo *)
    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"echo"
            ~description:"Echoes back the input text"
            ~title:"Echo Tool" ())
         (fun _ctx _name args ->
           let open Tool_arg in
           let* text = required args "text" string in
           Ok (Mcp_types.tool_result_of_text (Printf.sprintf "Echo: %s" text)))
    (* Tool: add *)
    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"add"
            ~description:"Adds two numbers"
            ~title:"Add Tool" ())
         (fun _ctx _name args ->
           let open Tool_arg in
           let* a = required args "a" float in
           let* b = required args "b" float in
           Ok (Mcp_types.tool_result_of_text
                 (Printf.sprintf "%g" (a +. b))))
    (* Resource: static greeting *)
    |> Mcp_protocol_http.Http_server.add_resource
         (Mcp_types.make_resource
            ~uri:"test://greeting"
            ~name:"greeting"
            ~title:"Greeting Resource"
            ~description:"A static greeting resource"
            ~mime_type:"text/plain" ())
         (fun _ctx _uri ->
           Ok [Mcp_types.{
             uri = "test://greeting";
             mime_type = Some "text/plain";
             text = Some "Hello from conformance server.";
             blob = None;
           }])
    (* Prompt: greet *)
    |> Mcp_protocol_http.Http_server.add_prompt
         (Mcp_types.make_prompt
            ~name:"greet"
            ~title:"Greeting Prompt"
            ~description:"Generates a greeting"
            ~arguments:[
              { name = "name"; description = Some "Name to greet"; required = Some true };
            ] ())
         (fun _ctx _name args ->
           let who = match List.assoc_opt "name" args with
             | Some n -> n
             | None -> "World"
           in
           Ok Mcp_types.{
             description = Some "A greeting";
             messages = [{
               role = User;
               content = PromptText {
                 type_ = "text";
                 text = Printf.sprintf "Hello, %s." who;
               };
             }];
           })
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
