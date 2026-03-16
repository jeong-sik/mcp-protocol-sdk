(** MCP HTTP server with OAuth 2.1 bearer token authentication.

    Demonstrates:
    - Auth middleware with custom token verifier
    - Protected Resource Metadata endpoint
    - Bearer token validation on all requests

    Usage:
      dune exec examples/http_auth_server.exe

    Test with:
      # Without token (401):
      curl -X POST http://localhost:8081/mcp \
        -H 'Content-Type: application/json' \
        -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'

      # With valid token:
      curl -X POST http://localhost:8081/mcp \
        -H 'Content-Type: application/json' \
        -H 'Authorization: Bearer secret-token-123' \
        -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'

      # Protected Resource Metadata:
      curl http://localhost:8081/.well-known/oauth-protected-resource
*)

open Mcp_protocol

type echo_input = {
  text: string;
} [@@deriving yojson, jsonschema]

let echo_tool =
  Mcp_types.make_tool
    ~name:"echo"
    ~description:"Echoes back the input text (requires auth)"
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

(* Simple static token verifier for demonstration.
   In production, this would validate JWTs or call an introspection endpoint. *)
let demo_verifier token _request =
  if token = "secret-token-123" then
    Ok Auth.{
      scopes = ["read"; "write"];
      expires_at = Unix.gettimeofday () +. 3600.0;
      user_id = Some "demo-user";
      extra = `Null;
    }
  else
    Error "Invalid or expired token"

let () =
  let port = 8081 in
  Printf.printf "Starting OAuth-protected MCP server on port %d...\n%!" port;
  Printf.printf "Valid token: secret-token-123\n%!" ;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let auth_config = Mcp_protocol_http.Auth_middleware.create
    ~verifier:demo_verifier
    ~required_scopes:["read"]
    ~resource_server:(Printf.sprintf "http://localhost:%d" port)
    ~authorization_servers:["https://auth.example.com"]
    ()
  in
  let server =
    Mcp_protocol_http.Http_server.create
      ~name:"auth-echo-server"
      ~version:"0.12.1"
      ~auth:auth_config
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
  Printf.printf "GET http://127.0.0.1:%d/.well-known/oauth-protected-resource\n%!" port;
  Cohttp_eio.Server.run socket cohttp_server
    ~on_error:(fun exn ->
      Printf.eprintf "Server error: %s\n%!" (Printexc.to_string exn))
