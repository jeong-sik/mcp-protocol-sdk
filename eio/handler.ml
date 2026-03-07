(** Transport-agnostic MCP request handler.

    Extracted from Server to allow reuse across stdio and HTTP transports. *)

open Mcp_protocol

(* ── context ─────────────────────────────────────────── *)

type context = {
  send_notification : method_:string -> params:Yojson.Safe.t option -> (unit, string) result;
  send_log : Logging.log_level -> string -> (unit, string) result;
  send_progress : token:Mcp_result.progress_token -> progress:float -> total:float option -> (unit, string) result;
  request_sampling : Sampling.create_message_params -> (Sampling.create_message_result, string) result;
  request_roots_list : unit -> (Mcp_types.root list, string) result;
  request_elicitation : Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result;
}

(* ── handler types ───────────────────────────────────── *)

type tool_handler = context -> string -> Yojson.Safe.t option -> (Mcp_types.tool_result, string) result
type resource_handler = context -> string -> (Mcp_types.resource_contents list, string) result
type prompt_handler = context -> string -> (string * string) list -> (Mcp_types.prompt_result, string) result
type completion_handler =
  Mcp_types.completion_reference -> string -> string -> Mcp_types.completion_result

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
  completion_handler: completion_handler option;
}

let create ~name ~version ?instructions () =
  { name; version; instructions;
    tools = []; resources = []; prompts = [];
    completion_handler = None }

let add_tool tool handler s =
  { s with tools = s.tools @ [{ tool; handler }] }

let add_resource resource handler s =
  { s with resources = s.resources @ [{ resource; handler }] }

let add_prompt prompt handler s =
  { s with prompts = s.prompts @ [{ prompt; handler }] }

let add_completion_handler handler s =
  { s with completion_handler = Some handler }

(* ── accessors ───────────────────────────────────────── *)

let name s = s.name
let version s = s.version
let instructions s = s.instructions
let tools s = List.map (fun rt -> rt.tool) s.tools
let resources s = List.map (fun rr -> rr.resource) s.resources
let prompts s = List.map (fun rp -> rp.prompt) s.prompts

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
  let logging_cap = Some (`Assoc []) in
  let completion_cap = match s.completion_handler with
    | Some _ -> Some (`Assoc [])
    | None -> None
  in
  let experimental = match completion_cap with
    | Some c -> Some (`Assoc [("completion", c)])
    | None -> None
  in
  Mcp_types.{
    tools = tools_cap;
    resources = resources_cap;
    prompts = prompts_cap;
    logging = logging_cap;
    experimental;
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

let handle_tools_call s ctx id params =
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
        begin match rt.handler ctx tool_name arguments with
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

let handle_resources_read s ctx id params =
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
        begin match rr.handler ctx resource_uri with
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

let handle_prompts_get s ctx id params =
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
        begin match rp.handler ctx prompt_name arguments with
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

(* ── logging/setLevel handler ────────────────────────── *)

let handle_logging_set_level log_level_ref id params =
  match params with
  | Some json ->
    begin match Logging.set_level_params_of_yojson json with
    | Ok p ->
      log_level_ref := p.Logging.level;
      Jsonrpc.make_response ~id ~result:(`Assoc [])
    | Error msg ->
      Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
        ~message:(Printf.sprintf "Invalid logging/setLevel params: %s" msg) ()
    end
  | None ->
    Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
      ~message:"Missing params for logging/setLevel" ()

(* ── completion/complete handler ─────────────────────── *)

let handle_completion_complete s id params =
  match s.completion_handler with
  | None ->
    Jsonrpc.make_error ~id ~code:Error_codes.method_not_found
      ~message:"No completion handler registered" ()
  | Some handler ->
    match params with
    | Some (`Assoc fields) ->
      let ref_ = match List.assoc_opt "ref" fields with
        | Some j -> Mcp_types.completion_reference_of_yojson j
        | None -> Error "Missing 'ref' in completion/complete params"
      in
      let argument = match List.assoc_opt "argument" fields with
        | Some (`Assoc arg_fields) ->
          let name = match List.assoc_opt "name" arg_fields with
            | Some (`String n) -> Ok n
            | _ -> Error "Missing 'name' in argument"
          in
          let value = match List.assoc_opt "value" arg_fields with
            | Some (`String v) -> Ok v
            | _ -> Error "Missing 'value' in argument"
          in
          begin match name, value with
          | Ok n, Ok v -> Ok (n, v)
          | Error e, _ | _, Error e -> Error e
          end
        | _ -> Error "Missing 'argument' in completion/complete params"
      in
      begin match ref_, argument with
      | Ok ref_, Ok (arg_name, arg_value) ->
        let result = handler ref_ arg_name arg_value in
        let completion_json = Mcp_types.completion_result_to_yojson result in
        Jsonrpc.make_response ~id
          ~result:(`Assoc [("completion", completion_json)])
      | Error msg, _ | _, Error msg ->
        Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
          ~message:msg ()
      end
    | _ ->
      Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
        ~message:"Invalid completion/complete params" ()

(* ── dispatch ─────────────────────────────────────────── *)

let dispatch s ctx log_level_ref (msg : Jsonrpc.message) : Jsonrpc.message option =
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
        handle_tools_call s ctx req.id req.params
      | m when m = Notifications.resources_list ->
        handle_resources_list s req.id
      | m when m = Notifications.resources_read ->
        handle_resources_read s ctx req.id req.params
      | m when m = Notifications.prompts_list ->
        handle_prompts_list s req.id
      | m when m = Notifications.prompts_get ->
        handle_prompts_get s ctx req.id req.params
      | m when m = Notifications.logging_set_level ->
        handle_logging_set_level log_level_ref req.id req.params
      | m when m = Notifications.completion_complete ->
        handle_completion_complete s req.id req.params
      | _ ->
        Jsonrpc.make_error ~id:req.id
          ~code:Error_codes.method_not_found
          ~message:(Printf.sprintf "Unknown method: %s" req.method_) ()
    in
    Some response
  | Notification notif ->
    if notif.method_ = Notifications.cancelled then
      Printf.eprintf "[%s] Received cancellation for request%s\n%!" s.name
        (match notif.params with
         | Some (`Assoc fields) ->
           (match List.assoc_opt "requestId" fields with
            | Some id -> " " ^ Yojson.Safe.to_string id
            | None -> "")
         | _ -> "");
    None
  | Response _ | Error _ ->
    None
