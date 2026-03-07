(** MCP Server framework over Eio stdio transport. *)

open Mcp_protocol

type tool_handler = string -> Yojson.Safe.t option -> (Mcp_types.tool_result, string) result
type resource_handler = string -> (Mcp_types.resource_contents list, string) result
type prompt_handler = string -> (string * string) list -> (Mcp_types.prompt_result, string) result

type registered_tool = {
  tool: Mcp_types.tool;
  handler: tool_handler;
}

type registered_resource = {
  resource: Mcp_types.resource;
  handler: resource_handler;
}

type registered_prompt = {
  prompt: Mcp_types.prompt;
  handler: prompt_handler;
}

type t = {
  name: string;
  version: string;
  instructions: string option;
  tools: registered_tool list;
  resources: registered_resource list;
  prompts: registered_prompt list;
}

let create ~name ~version ?instructions () =
  { name; version; instructions; tools = []; resources = []; prompts = [] }

let add_tool tool handler s =
  { s with tools = s.tools @ [{ tool; handler }] }

let add_resource resource handler s =
  { s with resources = s.resources @ [{ resource; handler }] }

let add_prompt prompt handler s =
  { s with prompts = s.prompts @ [{ prompt; handler }] }

(* ── capabilities ─────────────────────────────────────── *)

let server_capabilities s =
  let tools_cap =
    if s.tools = [] then None
    else Some (`Assoc [("listChanged", `Bool false)])
  in
  let resources_cap =
    if s.resources = [] then None
    else Some (`Assoc [("listChanged", `Bool false)])
  in
  let prompts_cap =
    if s.prompts = [] then None
    else Some (`Assoc [("listChanged", `Bool false)])
  in
  Mcp_types.{
    tools = tools_cap;
    resources = resources_cap;
    prompts = prompts_cap;
    logging = None;
    experimental = None;
  }

(* ── request handlers ─────────────────────────────────── *)

let handle_initialize s id params =
  let protocol_version =
    match params with
    | Some json ->
      begin match Mcp_types.initialize_params_of_yojson json with
      | Ok p ->
        begin match Version.negotiate ~requested:p.protocol_version with
        | Some v -> v
        | None -> Version.latest
        end
      | Error _ -> Version.latest
      end
    | None -> Version.latest
  in
  let result = Mcp_types.{
    protocol_version;
    capabilities = server_capabilities s;
    server_info = { name = s.name; version = s.version };
    instructions = s.instructions;
  } in
  Jsonrpc.make_response ~id ~result:(Mcp_types.initialize_result_to_yojson result)

let handle_ping id =
  Jsonrpc.make_response ~id ~result:(`Assoc [])

let handle_tools_list s id =
  let tools_json = List.map (fun rt -> Mcp_types.tool_to_yojson rt.tool) s.tools in
  Jsonrpc.make_response ~id ~result:(`Assoc [("tools", `List tools_json)])

let handle_tools_call s id params =
  match params with
  | Some (`Assoc fields) ->
    let name = match List.assoc_opt "name" fields with
      | Some (`String n) -> Some n
      | _ -> None
    in
    let arguments = List.assoc_opt "arguments" fields in
    begin match name with
    | None ->
      Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
        ~message:"Missing 'name' in tools/call params" ()
    | Some tool_name ->
      let handler_opt =
        List.find_opt (fun rt -> rt.tool.Mcp_types.name = tool_name) s.tools
      in
      begin match handler_opt with
      | None ->
        Jsonrpc.make_error ~id ~code:Error_codes.tool_execution_error
          ~message:(Printf.sprintf "Unknown tool: %s" tool_name) ()
      | Some rt ->
        begin match rt.handler tool_name arguments with
        | Ok result ->
          Jsonrpc.make_response ~id
            ~result:(Mcp_types.tool_result_to_yojson result)
        | Error msg ->
          Jsonrpc.make_response ~id
            ~result:(Mcp_types.tool_result_to_yojson (Mcp_types.tool_result_of_error msg))
        end
      end
    end
  | _ ->
    Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
      ~message:"Invalid tools/call params" ()

let handle_resources_list s id =
  let resources_json =
    List.map (fun rr -> Mcp_types.resource_to_yojson rr.resource) s.resources
  in
  Jsonrpc.make_response ~id ~result:(`Assoc [("resources", `List resources_json)])

let handle_resources_read s id params =
  match params with
  | Some (`Assoc fields) ->
    let uri = match List.assoc_opt "uri" fields with
      | Some (`String u) -> Some u
      | _ -> None
    in
    begin match uri with
    | None ->
      Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
        ~message:"Missing 'uri' in resources/read params" ()
    | Some resource_uri ->
      let handler_opt =
        List.find_opt (fun rr -> rr.resource.Mcp_types.uri = resource_uri) s.resources
      in
      begin match handler_opt with
      | None ->
        Jsonrpc.make_error ~id ~code:Error_codes.resource_not_found
          ~message:(Printf.sprintf "Unknown resource: %s" resource_uri) ()
      | Some rr ->
        begin match rr.handler resource_uri with
        | Ok contents ->
          let contents_json =
            List.map Mcp_types.resource_contents_to_yojson contents
          in
          Jsonrpc.make_response ~id
            ~result:(`Assoc [("contents", `List contents_json)])
        | Error msg ->
          Jsonrpc.make_error ~id ~code:Error_codes.internal_error
            ~message:msg ()
        end
      end
    end
  | _ ->
    Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
      ~message:"Invalid resources/read params" ()

let handle_prompts_list s id =
  let prompts_json =
    List.map (fun rp -> Mcp_types.prompt_to_yojson rp.prompt) s.prompts
  in
  Jsonrpc.make_response ~id ~result:(`Assoc [("prompts", `List prompts_json)])

let handle_prompts_get s id params =
  match params with
  | Some (`Assoc fields) ->
    let name = match List.assoc_opt "name" fields with
      | Some (`String n) -> Some n
      | _ -> None
    in
    let arguments =
      match List.assoc_opt "arguments" fields with
      | Some (`Assoc pairs) ->
        List.filter_map (fun (k, v) ->
          match v with `String s -> Some (k, s) | _ -> None
        ) pairs
      | _ -> []
    in
    begin match name with
    | None ->
      Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
        ~message:"Missing 'name' in prompts/get params" ()
    | Some prompt_name ->
      let handler_opt =
        List.find_opt (fun rp -> rp.prompt.Mcp_types.name = prompt_name) s.prompts
      in
      begin match handler_opt with
      | None ->
        Jsonrpc.make_error ~id ~code:Error_codes.prompt_not_found
          ~message:(Printf.sprintf "Unknown prompt: %s" prompt_name) ()
      | Some rp ->
        begin match rp.handler prompt_name arguments with
        | Ok result ->
          Jsonrpc.make_response ~id
            ~result:(Mcp_types.prompt_result_to_yojson result)
        | Error msg ->
          Jsonrpc.make_error ~id ~code:Error_codes.internal_error
            ~message:msg ()
        end
      end
    end
  | _ ->
    Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
      ~message:"Invalid prompts/get params" ()

(* ── dispatch ─────────────────────────────────────────── *)

let dispatch s (msg : Jsonrpc.message) : Jsonrpc.message option =
  match msg with
  | Request req ->
    let response = match req.method_ with
      | m when m = Notifications.initialize ->
        handle_initialize s req.id req.params
      | m when m = Notifications.ping ->
        handle_ping req.id
      | m when m = Notifications.tools_list ->
        handle_tools_list s req.id
      | m when m = Notifications.tools_call ->
        handle_tools_call s req.id req.params
      | m when m = Notifications.resources_list ->
        handle_resources_list s req.id
      | m when m = Notifications.resources_read ->
        handle_resources_read s req.id req.params
      | m when m = Notifications.prompts_list ->
        handle_prompts_list s req.id
      | m when m = Notifications.prompts_get ->
        handle_prompts_get s req.id req.params
      | _ ->
        Jsonrpc.make_error ~id:req.id
          ~code:Error_codes.method_not_found
          ~message:(Printf.sprintf "Unknown method: %s" req.method_) ()
    in
    Some response
  | Notification _ ->
    None
  | Response _ | Error _ ->
    None

(* ── main loop ────────────────────────────────────────── *)

let run s ~stdin ~stdout =
  let transport = Stdio_transport.create ~stdin ~stdout in
  let rec loop () =
    match Stdio_transport.read transport with
    | None -> ()
    | Some (Error e) ->
      Printf.eprintf "[%s] Read error: %s\n%!" s.name e;
      loop ()
    | Some (Ok msg) ->
      begin match dispatch s msg with
      | Some response ->
        begin match Stdio_transport.write transport response with
        | Ok () -> ()
        | Error e -> Printf.eprintf "[%s] Write error: %s\n%!" s.name e
        end
      | None -> ()
      end;
      loop ()
  in
  loop ();
  Stdio_transport.close transport
