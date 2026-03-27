(** MCP conformance test server.

    Implements all tools, resources, and prompts required by the official
    @modelcontextprotocol/conformance test suite (v0.1.15).

    Usage:
      dune exec examples/conformance_server.exe
      # Then:
      npx @modelcontextprotocol/conformance server \
        --url http://localhost:9100/mcp
*)

open Mcp_protocol

let base64_1px_png =
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="

let base64_short_wav =
  "UklGRiQAAABXQVZFZm10IBAAAAABAAEARKwAAIhYAQACABAAZGF0YQAAAAA="

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
      ~version:"1.2.0"
      ~instructions:"Conformance test server for MCP protocol SDK."
      ~enable_logging:true
      ()

    (* ── Tools: simple content types ───────────────── *)

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_simple_text"
            ~description:"Returns simple text content" ())
         (fun _ctx _name _args ->
           Ok Mcp_types.{
             content = [TextContent { type_ = "text";
               text = "This is a simple text response.";
               annotations = None }];
             is_error = None;
             structured_content = None;
             _meta = None;
           })

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_image_content"
            ~description:"Returns image content" ())
         (fun _ctx _name _args ->
           Ok Mcp_types.{
             content = [ImageContent { type_ = "image";
               data = base64_1px_png;
               mime_type = "image/png";
               annotations = None }];
             is_error = None;
             structured_content = None;
             _meta = None;
           })

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_audio_content"
            ~description:"Returns audio content" ())
         (fun _ctx _name _args ->
           Ok Mcp_types.{
             content = [AudioContent { type_ = "audio";
               data = base64_short_wav;
               mime_type = "audio/wav";
               annotations = None }];
             is_error = None;
             structured_content = None;
             _meta = None;
           })

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_embedded_resource"
            ~description:"Returns embedded resource content" ())
         (fun _ctx _name _args ->
           Ok Mcp_types.{
             content = [make_resource_content {
               uri = "test://embedded";
               mime_type = Some "text/plain";
               text = Some "This is an embedded resource content.";
               blob = None;
             }];
             is_error = None;
             structured_content = None;
             _meta = None;
           })

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_multiple_content_types"
            ~description:"Returns multiple content types" ())
         (fun _ctx _name _args ->
           Ok Mcp_types.{
             content = [
               TextContent { type_ = "text";
                 text = "Here is a text message and an image:";
                 annotations = None };
               ImageContent { type_ = "image";
                 data = base64_1px_png;
                 mime_type = "image/png";
                 annotations = None };
             ];
             is_error = None;
             structured_content = None;
             _meta = None;
           })

    (* ── Tools: behavioral ─────────────────────────── *)

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_tool_with_logging"
            ~description:"Sends log messages during execution" ())
         (fun ctx _name _args ->
           ignore (ctx.send_log Logging.Info "Log message 1 of 3");
           Unix.sleepf 0.1;
           ignore (ctx.send_log Logging.Info "Log message 2 of 3");
           Unix.sleepf 0.1;
           ignore (ctx.send_log Logging.Info "Log message 3 of 3");
           Ok (Mcp_types.tool_result_of_text "Logging complete"))

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_error_handling"
            ~description:"Always returns an error" ())
         (fun _ctx _name _args ->
           Ok Mcp_types.{
             content = [TextContent { type_ = "text";
               text = "This tool intentionally returns an error for testing";
               annotations = None }];
             is_error = Some true;
             structured_content = None;
             _meta = None;
           })

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_tool_with_progress"
            ~description:"Reports progress notifications" ())
         (fun ctx _name args ->
           let token = match args with
             | Some (`Assoc fields) ->
               (match List.assoc_opt "_meta" fields with
                | Some (`Assoc meta) ->
                  (match List.assoc_opt "progressToken" meta with
                   | Some (`String s) -> Some (Mcp_result.String_token s)
                   | Some (`Int i) -> Some (Mcp_result.Int_token i)
                   | _ -> None)
                | _ -> None)
             | _ -> None
           in
           (match token with
            | Some tok ->
              for i = 1 to 5 do
                ignore (ctx.send_progress
                  ~token:tok
                  ~progress:(float_of_int i)
                  ~message:(Some (Printf.sprintf "Step %d of 5" i))
                  ~total:(Some 5.0));
                Unix.sleepf 0.05
              done
            | None -> ());
           Ok (Mcp_types.tool_result_of_text "Progress complete"))

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_sampling"
            ~description:"Requests LLM sampling from client" ())
         (fun ctx _name args ->
           let prompt = match args with
             | Some (`Assoc fields) ->
               (match List.assoc_opt "prompt" fields with
                | Some (`String s) -> s
                | _ -> "Hello")
             | _ -> "Hello"
           in
           let params : Sampling.create_message_params = {
             messages = [{
               role = User;
               content = Text { type_ = "text"; text = prompt };
             }];
             model_preferences = None;
             system_prompt = None;
             include_context = None;
             temperature = None;
             max_tokens = 100;
             stop_sequences = None;
             metadata = None;
             tools = None;
             tool_choice = None;
             _meta = None;
           } in
           match ctx.request_sampling params with
           | Ok result ->
             let text = match result.content with
               | Sampling.Text { text; _ } -> text
               | _ -> "<non-text response>"
             in
             Ok (Mcp_types.tool_result_of_text
                   (Printf.sprintf "Sampling result: %s" text))
           | Error e ->
             Ok (Mcp_types.tool_result_of_text
                   (Printf.sprintf "Sampling failed: %s" e)))

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_elicitation"
            ~description:"Requests user input from client" ())
         (fun ctx _name _args ->
           let params : Mcp_types.elicitation_params = {
             message = "Please provide your name";
             requested_schema = Some {
               type_ = "object";
               properties = [
                 ("name", `Assoc [
                   ("type", `String "string");
                   ("description", `String "Your name");
                 ]);
               ];
               required = Some ["name"];
             };
             mode = None;
             url = None;
           } in
           match ctx.request_elicitation params with
           | Ok result ->
             let action = match result.action with
               | Mcp_types.Accept -> "accepted"
               | Mcp_types.Decline -> "declined"
               | Mcp_types.Cancel -> "cancelled"
             in
             Ok (Mcp_types.tool_result_of_text
                   (Printf.sprintf "Elicitation %s" action))
           | Error e ->
             Ok (Mcp_types.tool_result_of_text
                   (Printf.sprintf "Elicitation failed: %s" e)))

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_elicitation_sep1034_defaults"
            ~description:"Elicitation with default values" ())
         (fun ctx _name _args ->
           let params : Mcp_types.elicitation_params = {
             message = "Test default values";
             requested_schema = Some {
               type_ = "object";
               properties = [
                 ("stringField", `Assoc [
                   ("type", `String "string");
                   ("default", `String "default_string");
                 ]);
                 ("numberField", `Assoc [
                   ("type", `String "number");
                   ("default", `Float 42.0);
                 ]);
                 ("booleanField", `Assoc [
                   ("type", `String "boolean");
                   ("default", `Bool true);
                 ]);
               ];
               required = None;
             };
             mode = None;
             url = None;
           } in
           match ctx.request_elicitation params with
           | Ok result ->
             let action = match result.action with
               | Mcp_types.Accept -> "accepted"
               | Mcp_types.Decline -> "declined"
               | Mcp_types.Cancel -> "cancelled"
             in
             Ok (Mcp_types.tool_result_of_text
                   (Printf.sprintf "Elicitation %s" action))
           | Error e ->
             Ok (Mcp_types.tool_result_of_text
                   (Printf.sprintf "Elicitation failed: %s" e)))

    |> Mcp_protocol_http.Http_server.add_tool
         (Mcp_types.make_tool ~name:"test_elicitation_sep1330_enums"
            ~description:"Elicitation with enum values" ())
         (fun ctx _name _args ->
           let params : Mcp_types.elicitation_params = {
             message = "Test enum values";
             requested_schema = Some {
               type_ = "object";
               properties = [
                 ("choice", `Assoc [
                   ("type", `String "string");
                   ("enum", `List [
                     `String "option1"; `String "option2"; `String "option3";
                     `String "option4"; `String "option5";
                   ]);
                 ]);
               ];
               required = None;
             };
             mode = None;
             url = None;
           } in
           match ctx.request_elicitation params with
           | Ok result ->
             let action = match result.action with
               | Mcp_types.Accept -> "accepted"
               | Mcp_types.Decline -> "declined"
               | Mcp_types.Cancel -> "cancelled"
             in
             Ok (Mcp_types.tool_result_of_text
                   (Printf.sprintf "Elicitation %s" action))
           | Error e ->
             Ok (Mcp_types.tool_result_of_text
                   (Printf.sprintf "Elicitation failed: %s" e)))

    (* ── Resources ─────────────────────────────────── *)

    |> Mcp_protocol_http.Http_server.add_resource
         (Mcp_types.make_resource
            ~uri:"test://static-text"
            ~name:"static-text"
            ~description:"A static text resource"
            ~mime_type:"text/plain" ())
         (fun _ctx _uri ->
           Ok [Mcp_types.{
             uri = "test://static-text";
             mime_type = Some "text/plain";
             text = Some "This is the content of the static text resource.";
             blob = None;
           }])

    |> Mcp_protocol_http.Http_server.add_resource
         (Mcp_types.make_resource
            ~uri:"test://static-binary"
            ~name:"static-binary"
            ~description:"A static binary resource"
            ~mime_type:"application/octet-stream" ())
         (fun _ctx _uri ->
           Ok [Mcp_types.{
             uri = "test://static-binary";
             mime_type = Some "application/octet-stream";
             text = None;
             blob = Some base64_1px_png;
           }])

    |> Mcp_protocol_http.Http_server.add_resource_template
         Mcp_types.{
           uri_template = "test://template/{id}/data";
           name = "template-data";
           title = Some "Template Data Resource";
           description = Some "A dynamic template resource";
           mime_type = Some "application/json";
           icon = None;
         }
         (fun _ctx uri ->
           Ok [Mcp_types.{
             uri;
             mime_type = Some "application/json";
             text = Some (Printf.sprintf {|{"uri":"%s","type":"template-data"}|} uri);
             blob = None;
           }])

    (* ── Prompts ───────────────────────────────────── *)

    |> Mcp_protocol_http.Http_server.add_prompt
         (Mcp_types.make_prompt
            ~name:"test_simple_prompt"
            ~description:"A simple test prompt" ())
         (fun _ctx _name _args ->
           Ok Mcp_types.{
             description = Some "A simple prompt";
             messages = [{
               role = User;
               content = PromptText { type_ = "text";
                 text = "This is a simple prompt for testing." };
             }];
             _meta = None;
           })

    |> Mcp_protocol_http.Http_server.add_prompt
         (Mcp_types.make_prompt
            ~name:"test_prompt_with_arguments"
            ~description:"A prompt with arguments"
            ~arguments:[
              { name = "arg1"; description = Some "First argument"; required = Some true };
              { name = "arg2"; description = Some "Second argument"; required = Some true };
            ] ())
         (fun _ctx _name args ->
           let arg1 = match List.assoc_opt "arg1" args with Some v -> v | None -> "" in
           let arg2 = match List.assoc_opt "arg2" args with Some v -> v | None -> "" in
           Ok Mcp_types.{
             description = Some "A prompt with arguments";
             messages = [{
               role = User;
               content = PromptText { type_ = "text";
                 text = Printf.sprintf "Argument 1: %s, Argument 2: %s" arg1 arg2 };
             }];
             _meta = None;
           })

    |> Mcp_protocol_http.Http_server.add_prompt
         (Mcp_types.make_prompt
            ~name:"test_prompt_with_embedded_resource"
            ~description:"A prompt with embedded resource"
            ~arguments:[
              { name = "resourceUri"; description = Some "URI of resource to embed"; required = Some true };
            ] ())
         (fun _ctx _name args ->
           let uri = match List.assoc_opt "resourceUri" args with
             | Some v -> v | None -> "test://embedded"
           in
           Ok Mcp_types.{
             description = Some "A prompt with embedded resource";
             messages = [{
               role = User;
               content = PromptResource { type_ = "resource";
                 resource = {
                   uri;
                   mime_type = Some "text/plain";
                   text = Some "This is embedded resource content.";
                   blob = None;
                 }};
             }];
             _meta = None;
           })

    |> Mcp_protocol_http.Http_server.add_prompt
         (Mcp_types.make_prompt
            ~name:"test_prompt_with_image"
            ~description:"A prompt with image content" ())
         (fun _ctx _name _args ->
           Ok Mcp_types.{
             description = Some "A prompt with image";
             messages = [{
               role = User;
               content = PromptImage { type_ = "image";
                 data = base64_1px_png;
                 mime_type = "image/png" };
             }];
             _meta = None;
           })

    (* ── Completion ────────────────────────────────── *)

    |> Mcp_protocol_http.Http_server.add_completion_handler
         (fun ref_ arg_name arg_value ~context:_ ->
           let values =
             match ref_, arg_name with
             | Mcp_types.Prompt_ref { name }, "arg1"
               when String.equal name "test_prompt_with_arguments" ->
               let prefix = String.lowercase_ascii arg_value in
               ["hello"; "help"; "world"]
               |> List.filter (fun v ->
                    String.starts_with ~prefix (String.lowercase_ascii v))
             | _ -> []
           in
           Mcp_types.make_completion_result ~values ())
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
