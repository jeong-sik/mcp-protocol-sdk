(** Transport-agnostic MCP request handler.

    Extracted from Server to allow reuse across stdio and HTTP transports. *)

open Mcp_protocol

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

(* ── context ─────────────────────────────────────────── *)

type context = {
  send_notification : method_:string -> params:Yojson.Safe.t option -> (unit, string) result;
  send_log : Logging.log_level -> string -> (unit, string) result;
  send_progress : token:Mcp_result.progress_token -> progress:float -> message:string option -> total:float option -> (unit, string) result;
  request_sampling : Sampling.create_message_params -> (Sampling.create_message_result, string) result;
  request_roots_list : unit -> (Mcp_types.root list, string) result;
  request_elicitation : Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result;
}

(* ── handler types ───────────────────────────────────── *)

type tool_handler = context -> string -> Yojson.Safe.t option -> (Mcp_types.tool_result, string) result
type resource_handler = context -> string -> (Mcp_types.resource_contents list, string) result
type prompt_handler = context -> string -> (string * string) list -> (Mcp_types.prompt_result, string) result
type completion_handler =
  Mcp_types.completion_reference -> string -> string -> context:Mcp_types.completion_context option -> Mcp_types.completion_result

type task_handlers = {
  get: context -> string -> (Mcp_types.task, string) result;
  result: context -> string -> (Yojson.Safe.t, string) result;
  list: context -> string option -> (Mcp_types.task list * string option, string) result;
  cancel: context -> string -> (Mcp_types.task, string) result;
}

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

type registered_resource_template = {
  template: Mcp_types.resource_template;
  handler: resource_handler;
}

type t = {
  name: string;
  version: string;
  instructions: string option;
  enable_logging: bool;
  tools: registered_tool StringMap.t;
  resources: registered_resource StringMap.t;
  resource_templates: registered_resource_template StringMap.t;
  prompts: registered_prompt StringMap.t;
  completion_handler: completion_handler option;
  task_handlers: task_handlers option;
  subscribed_uris: StringSet.t Atomic.t;
}

let create ~name ~version ?instructions ?(enable_logging = true) () =
  { name; version; instructions; enable_logging;
    tools = StringMap.empty; resources = StringMap.empty;
    resource_templates = StringMap.empty; prompts = StringMap.empty;
    completion_handler = None;
    task_handlers = None;
    subscribed_uris = Atomic.make StringSet.empty }

(** H3 fix: Check for duplicate tool names before adding.
    Previously, registering two tools with the same name would silently
    shadow the second one (List.find_opt returns the first match). *)
let add_tool (tool : Mcp_types.tool) handler s =
  begin if StringMap.mem tool.Mcp_types.name s.tools then
    Printf.eprintf "[mcp-protocol] Warning: duplicate tool name '%s', replacing previous registration\n%!" tool.Mcp_types.name
  end;
  { s with tools = StringMap.add tool.Mcp_types.name { tool; handler } s.tools }

let add_resource (resource : Mcp_types.resource) handler s =
  { s with resources = StringMap.add resource.Mcp_types.uri { resource; handler } s.resources }

let add_resource_template (template : Mcp_types.resource_template) handler s =
  { s with resource_templates = StringMap.add template.Mcp_types.uri_template { template; handler } s.resource_templates }

let add_prompt (prompt : Mcp_types.prompt) handler s =
  { s with prompts = StringMap.add prompt.Mcp_types.name { prompt; handler } s.prompts }

let add_completion_handler handler s =
  { s with completion_handler = Some handler }

let add_task_handlers handlers s =
  { s with task_handlers = Some handlers }

(* ── ergonomic registration ───────────────────────────── *)

let tool name ?description ?input_schema handler s =
  let t = Mcp_types.make_tool ~name ?description ?input_schema () in
  add_tool t handler s

let resource ~uri name ?description ?mime_type handler s =
  let r = Mcp_types.make_resource ~uri ~name ?description ?mime_type () in
  add_resource r handler s

let resource_template ~uri_template name ?description ?mime_type handler s =
  let t : Mcp_types.resource_template = {
    uri_template; name; title = None; description; mime_type; icon = None;
  } in
  add_resource_template t handler s

let prompt name ?description ?arguments handler s =
  let p = Mcp_types.make_prompt ~name ?description ?arguments () in
  add_prompt p handler s

(* ── shared client helpers ────────────────────────────── *)

let parse_list_field field_name parser result =
  match result with
  | `Assoc fields ->
    begin match List.assoc_opt field_name fields with
    | Some (`List items) ->
      List.mapi
        (fun idx j ->
          parser j |> Result.map_error (fun err ->
              Printf.sprintf "Invalid '%s' item at index %d: %s"
                field_name idx err))
        items
      |> List.fold_left
           (fun acc item ->
             Result.bind acc (fun parsed ->
                 Result.bind item (fun value -> Ok (value :: parsed))))
           (Ok [])
      |> Result.map List.rev
    | _ -> Error (Printf.sprintf "Missing '%s' array in response" field_name)
    end
  | _ -> Error "Invalid response format"

let parse_paginated_list_field field_name parser result =
  match result with
  | `Assoc fields ->
    let next_cursor =
      match List.assoc_opt "nextCursor" fields with
      | Some (`String s) when String.trim s <> "" -> Some s
      | _ -> None
    in
    Result.map (fun items -> (items, next_cursor))
      (parse_list_field field_name parser result)
  | _ -> Error "Invalid response format"

let build_initialize_params ~has_sampling ~has_roots ~has_elicitation
    ~client_name ~client_version =
  let caps : Mcp_types.client_capabilities = {
    roots = if has_roots then Some { list_changed = Some false } else None;
    sampling = if has_sampling then Some () else None;
    elicitation = if has_elicitation then Some () else None;
    experimental = None;
    extensions = None;
  } in
  Mcp_types.initialize_params_to_yojson {
    protocol_version = Version.latest;
    capabilities = caps;
    client_info = { name = client_name; version = client_version };
    _meta = None;
  }

(* ── accessors ───────────────────────────────────────── *)

let name s = s.name
let version s = s.version
let instructions s = s.instructions
let tools s = StringMap.fold (fun _ rt acc -> rt.tool :: acc) s.tools [] |> List.rev
let resources s = StringMap.fold (fun _ rr acc -> rr.resource :: acc) s.resources [] |> List.rev
let resource_templates s = StringMap.fold (fun _ rt acc -> rt.template :: acc) s.resource_templates [] |> List.rev
let prompts s = StringMap.fold (fun _ rp acc -> rp.prompt :: acc) s.prompts [] |> List.rev
let subscribed_uris s = StringSet.elements (Atomic.get s.subscribed_uris)

(* ── capabilities ─────────────────────────────────────── *)

let server_capabilities s =
  let tools_cap : Mcp_types.tools_capability option =
    if StringMap.is_empty s.tools then None
    else Some { list_changed = Some false }
  in
  let resources_cap : Mcp_types.resources_capability option =
    if StringMap.is_empty s.resources && StringMap.is_empty s.resource_templates then None
    else Some { subscribe = Some true; list_changed = Some false }
  in
  let prompts_cap : Mcp_types.prompts_capability option =
    if StringMap.is_empty s.prompts then None
    else Some { list_changed = Some false }
  in
  let logging_cap =
    if s.enable_logging then Some () else None
  in
  let completions_cap = match s.completion_handler with
    | Some _ -> Some ()
    | None -> None
  in
  let tasks_cap = match s.task_handlers with
    | Some _ -> Some (`Assoc [
        ("list", `Bool true);
        ("cancel", `Bool true);
      ])
    | None -> None
  in
  let experimental_fields =
    (match tasks_cap with Some t -> [("tasks", t)] | None -> [])
  in
  let experimental = match experimental_fields with
    | [] -> None
    | fields -> Some (`Assoc fields)
  in
  Mcp_types.{
    tools = tools_cap;
    resources = resources_cap;
    prompts = prompts_cap;
    logging = logging_cap;
    completions = completions_cap;
    experimental;
    extensions = None;
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
        | None ->
          (* H2 fix: When no compatible version exists, the spec says
             the server should return an error. However, initialize is a
             handshake response — returning latest with a log warning
             is more practical than failing the connection entirely.
             The client can inspect protocolVersion and decide. *)
          Printf.eprintf "[mcp-protocol] Warning: no compatible version for '%s', using latest '%s'\n%!"
            p.protocol_version Version.latest;
          Version.latest
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
    _meta = None;
  } in
  Jsonrpc.make_response ~id ~result:(Mcp_types.initialize_result_to_yojson result)

let handle_ping id =
  Jsonrpc.make_response ~id ~result:(`Assoc [])

let handle_tools_list s id =
  let tools_json = StringMap.fold (fun _ rt acc -> Mcp_types.tool_to_yojson rt.tool :: acc) s.tools [] |> List.rev in
  Jsonrpc.make_response ~id ~result:(`Assoc [("tools", `List tools_json)])

let handle_tools_call s ctx id params =
  match params with
  | Some (`Assoc fields) ->
    let name = match List.assoc_opt "name" fields with
      | Some (`String n) -> Some n
      | _ -> None
    in
    let arguments =
      let args = List.assoc_opt "arguments" fields in
      let meta = List.assoc_opt "_meta" fields in
      match args, meta with
      | Some (`Assoc a), Some m -> Some (`Assoc (("_meta", m) :: a))
      | None, Some m -> Some (`Assoc [("_meta", m)])
      | _ -> args
    in
    begin match name with
    | None ->
      Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
        ~message:"Missing 'name' in tools/call params" ()
    | Some tool_name ->
      let handler_opt = StringMap.find_opt tool_name s.tools in
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
    StringMap.fold (fun _ rr acc -> Mcp_types.resource_to_yojson rr.resource :: acc) s.resources [] |> List.rev
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
      let handler_opt = StringMap.find_opt resource_uri s.resources in
      begin match handler_opt with
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
      | None ->
        (* Fall back to resource templates: find a template whose
           uri_template is a prefix match for the requested URI. *)
        let template_handler_opt =
          StringMap.bindings s.resource_templates
          |> List.find_opt (fun (_key, rt) ->
            (* Simple prefix match: check if the URI starts with the
               non-variable prefix of the template. A full RFC 6570
               implementation can be added later. *)
            let tmpl = rt.template.Mcp_types.uri_template in
            let prefix = match String.index_opt tmpl '{' with
              | Some i -> String.sub tmpl 0 i
              | None -> tmpl
            in
            String.length resource_uri >= String.length prefix
            && String.sub resource_uri 0 (String.length prefix) = prefix
          )
          |> Option.map snd
        in
        begin match template_handler_opt with
        | Some rt ->
          begin match rt.handler ctx resource_uri with
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
        | None ->
          Jsonrpc.make_error ~id ~code:Error_codes.resource_not_found
            ~message:(Printf.sprintf "Unknown resource: %s" resource_uri) ()
        end
      end
    end
  | _ ->
    Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
      ~message:"Invalid resources/read params" ()

let handle_prompts_list s id =
  let prompts_json =
    StringMap.fold (fun _ rp acc -> Mcp_types.prompt_to_yojson rp.prompt :: acc) s.prompts [] |> List.rev
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
      let handler_opt = StringMap.find_opt prompt_name s.prompts in
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

(* ── resources/subscribe + unsubscribe ────────────────── *)

(** Atomically update subscribed_uris via compare-and-set loop.
    Lock-free: StringSet is immutable, so the CAS swaps a pointer. *)
let atomic_update_uris s f =
  let rec loop () =
    let old = Atomic.get s.subscribed_uris in
    let updated = f old in
    if Atomic.compare_and_set s.subscribed_uris old updated then ()
    else loop ()
  in
  loop ()

let handle_resources_subscribe s id params =
  match params with
  | Some (`Assoc fields) ->
    begin match List.assoc_opt "uri" fields with
    | Some (`String uri) ->
      atomic_update_uris s (StringSet.add uri);
      Jsonrpc.make_response ~id ~result:(`Assoc [])
    | _ ->
      Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
        ~message:"Missing 'uri' in resources/subscribe params" ()
    end
  | _ ->
    Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
      ~message:"Invalid resources/subscribe params" ()

let handle_resources_unsubscribe s id params =
  match params with
  | Some (`Assoc fields) ->
    begin match List.assoc_opt "uri" fields with
    | Some (`String uri) ->
      atomic_update_uris s (StringSet.remove uri);
      Jsonrpc.make_response ~id ~result:(`Assoc [])
    | _ ->
      Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
        ~message:"Missing 'uri' in resources/unsubscribe params" ()
    end
  | _ ->
    Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
      ~message:"Invalid resources/unsubscribe params" ()

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
      let context = match List.assoc_opt "context" fields with
        | Some j ->
          begin match Mcp_types.completion_context_of_yojson j with
          | Ok ctx -> Some ctx
          | Error _ -> None
          end
        | None -> None
      in
      begin match ref_, argument with
      | Ok ref_, Ok (arg_name, arg_value) ->
        let result = handler ref_ arg_name arg_value ~context in
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

(* ── tasks handlers ──────────────────────────────────── *)

let handle_tasks_get s ctx id params =
  match s.task_handlers with
  | None ->
    Jsonrpc.make_error ~id ~code:Error_codes.method_not_found
      ~message:"No task handlers registered" ()
  | Some th ->
    match params with
    | Some (`Assoc fields) ->
      begin match List.assoc_opt "taskId" fields with
      | Some (`String task_id) ->
        begin match th.get ctx task_id with
        | Ok task ->
          Jsonrpc.make_response ~id ~result:(Mcp_types.task_to_yojson task)
        | Error msg ->
          Jsonrpc.make_error ~id ~code:Error_codes.internal_error ~message:msg ()
        end
      | _ ->
        Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
          ~message:"Missing 'taskId' in tasks/get params" ()
      end
    | _ ->
      Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
        ~message:"Invalid tasks/get params" ()

let handle_tasks_result s ctx id params =
  match s.task_handlers with
  | None ->
    Jsonrpc.make_error ~id ~code:Error_codes.method_not_found
      ~message:"No task handlers registered" ()
  | Some th ->
    match params with
    | Some (`Assoc fields) ->
      begin match List.assoc_opt "taskId" fields with
      | Some (`String task_id) ->
        begin match th.result ctx task_id with
        | Ok result_json ->
          Jsonrpc.make_response ~id ~result:result_json
        | Error msg ->
          Jsonrpc.make_error ~id ~code:Error_codes.internal_error ~message:msg ()
        end
      | _ ->
        Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
          ~message:"Missing 'taskId' in tasks/result params" ()
      end
    | _ ->
      Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
        ~message:"Invalid tasks/result params" ()

let handle_tasks_list s ctx id params =
  match s.task_handlers with
  | None ->
    Jsonrpc.make_error ~id ~code:Error_codes.method_not_found
      ~message:"No task handlers registered" ()
  | Some th ->
    let cursor = match params with
      | Some (`Assoc fields) ->
        begin match List.assoc_opt "cursor" fields with
        | Some (`String c) -> Some c | _ -> None
        end
      | _ -> None
    in
    begin match th.list ctx cursor with
    | Ok (tasks, next_cursor) ->
      let tasks_json = List.map Mcp_types.task_to_yojson tasks in
      let fields = [("tasks", `List tasks_json)] in
      let fields = match next_cursor with
        | Some c -> ("nextCursor", `String c) :: fields
        | None -> fields
      in
      Jsonrpc.make_response ~id ~result:(`Assoc fields)
    | Error msg ->
      Jsonrpc.make_error ~id ~code:Error_codes.internal_error ~message:msg ()
    end

let handle_tasks_cancel s ctx id params =
  match s.task_handlers with
  | None ->
    Jsonrpc.make_error ~id ~code:Error_codes.method_not_found
      ~message:"No task handlers registered" ()
  | Some th ->
    match params with
    | Some (`Assoc fields) ->
      begin match List.assoc_opt "taskId" fields with
      | Some (`String task_id) ->
        begin match th.cancel ctx task_id with
        | Ok task ->
          Jsonrpc.make_response ~id ~result:(Mcp_types.task_to_yojson task)
        | Error msg ->
          Jsonrpc.make_error ~id ~code:Error_codes.internal_error ~message:msg ()
        end
      | _ ->
        Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
          ~message:"Missing 'taskId' in tasks/cancel params" ()
      end
    | _ ->
      Jsonrpc.make_error ~id ~code:Error_codes.invalid_params
        ~message:"Invalid tasks/cancel params" ()

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
      | m when m = Notifications.resources_subscribe ->
        handle_resources_subscribe s req.id req.params
      | m when m = Notifications.resources_unsubscribe ->
        handle_resources_unsubscribe s req.id req.params
      | m when m = Notifications.resources_templates_list ->
        let templates_json = StringMap.fold (fun _ rt acc ->
          Mcp_types.resource_template_to_yojson rt.template :: acc) s.resource_templates [] |> List.rev in
        Jsonrpc.make_response ~id:req.id
          ~result:(`Assoc [("resourceTemplates", `List templates_json)])
      | m when m = Notifications.prompts_list ->
        handle_prompts_list s req.id
      | m when m = Notifications.prompts_get ->
        handle_prompts_get s ctx req.id req.params
      | m when m = Notifications.logging_set_level ->
        handle_logging_set_level log_level_ref req.id req.params
      | m when m = Notifications.completion_complete ->
        handle_completion_complete s req.id req.params
      | m when m = Notifications.tasks_get ->
        handle_tasks_get s ctx req.id req.params
      | m when m = Notifications.tasks_result ->
        handle_tasks_result s ctx req.id req.params
      | m when m = Notifications.tasks_list ->
        handle_tasks_list s ctx req.id req.params
      | m when m = Notifications.tasks_cancel ->
        handle_tasks_cancel s ctx req.id req.params
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
