(** MCP Client over Eio stdio transport. *)

open Mcp_protocol

type sampling_handler =
  Sampling.create_message_params -> (Sampling.create_message_result, string) result
type roots_handler =
  unit -> (Mcp_types.root list, string) result
type elicitation_handler =
  Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result
type notification_handler =
  string -> Yojson.Safe.t option -> unit

type t = {
  transport: Stdio_transport.t;
  mutable next_id: int;
  sampling_handler: sampling_handler option;
  roots_handler: roots_handler option;
  elicitation_handler: elicitation_handler option;
  notification_handler: notification_handler option;
}

let create ~stdin ~stdout = {
  transport = Stdio_transport.create ~stdin ~stdout;
  next_id = 1;
  sampling_handler = None;
  roots_handler = None;
  elicitation_handler = None;
  notification_handler = None;
}

let on_sampling handler t = { t with sampling_handler = Some handler }
let on_roots_list handler t = { t with roots_handler = Some handler }
let on_elicitation handler t = { t with elicitation_handler = Some handler }
let on_notification handler t = { t with notification_handler = Some handler }

(* ── request/response ─────────────────────────────── *)

let read_response t expected_id =
  let rec loop () =
    match Stdio_transport.read t.transport with
    | None -> Error "Connection closed"
    | Some (Error e) -> Error (Printf.sprintf "Read error: %s" e)
    | Some (Ok msg) ->
      begin match msg with
      | Jsonrpc.Response resp when resp.id = expected_id ->
        Ok resp.result
      | Jsonrpc.Error err when err.id = expected_id ->
        Error err.error.message
      | Jsonrpc.Notification _ ->
        loop ()
      | _ ->
        loop ()
      end
  in
  loop ()

let send_request t ~method_ ?params () =
  let id = Jsonrpc.Int t.next_id in
  t.next_id <- t.next_id + 1;
  let msg = Jsonrpc.make_request ~id ~method_ ?params () in
  match Stdio_transport.write t.transport msg with
  | Error e -> Error e
  | Ok () -> read_response t id

let send_notification t ~method_ ?params () =
  let msg = Jsonrpc.make_notification ~method_ ?params () in
  Stdio_transport.write t.transport msg

(* ── helpers ──────────────────────────────────────── *)

let parse_list_field field_name parser result =
  match result with
  | `Assoc fields ->
    begin match List.assoc_opt field_name fields with
    | Some (`List items) ->
      let parsed = List.filter_map (fun j ->
        match parser j with
        | Ok v -> Some v
        | Error _ -> None
      ) items in
      Ok parsed
    | _ -> Error (Printf.sprintf "Missing '%s' array in response" field_name)
    end
  | _ -> Error "Invalid response format"

(* ── initialize ───────────────────────────────────── *)

let initialize t ~client_name ~client_version =
  let params = `Assoc [
    ("protocolVersion", `String Version.latest);
    ("capabilities", `Assoc []);
    ("clientInfo", `Assoc [
      ("name", `String client_name);
      ("version", `String client_version);
    ]);
  ] in
  match send_request t ~method_:Notifications.initialize ~params () with
  | Error e -> Error e
  | Ok result ->
    match send_notification t ~method_:Notifications.initialized () with
    | Error e -> Error (Printf.sprintf "Failed to send initialized notification: %s" e)
    | Ok () -> Mcp_types.initialize_result_of_yojson result

(* ── ping ─────────────────────────────────────────── *)

let ping t =
  match send_request t ~method_:Notifications.ping () with
  | Error e -> Error e
  | Ok _ -> Ok ()

(* ── tools ────────────────────────────────────────── *)

let list_tools t =
  match send_request t ~method_:Notifications.tools_list () with
  | Error e -> Error e
  | Ok result -> parse_list_field "tools" Mcp_types.tool_of_yojson result

let call_tool t ~name ?arguments () =
  let params_fields = [("name", `String name)] in
  let params_fields = match arguments with
    | Some args -> params_fields @ [("arguments", args)]
    | None -> params_fields
  in
  let params = `Assoc params_fields in
  match send_request t ~method_:Notifications.tools_call ~params () with
  | Error e -> Error e
  | Ok result -> Mcp_types.tool_result_of_yojson result

(* ── resources ────────────────────────────────────── *)

let list_resources t =
  match send_request t ~method_:Notifications.resources_list () with
  | Error e -> Error e
  | Ok result -> parse_list_field "resources" Mcp_types.resource_of_yojson result

let read_resource t ~uri =
  let params = `Assoc [("uri", `String uri)] in
  match send_request t ~method_:Notifications.resources_read ~params () with
  | Error e -> Error e
  | Ok result -> parse_list_field "contents" Mcp_types.resource_contents_of_yojson result

(* ── prompts ──────────────────────────────────────── *)

let list_prompts t =
  match send_request t ~method_:Notifications.prompts_list () with
  | Error e -> Error e
  | Ok result -> parse_list_field "prompts" Mcp_types.prompt_of_yojson result

let get_prompt t ~name ?arguments () =
  let params_fields = [("name", `String name)] in
  let params_fields = match arguments with
    | Some pairs ->
      let args_json = `Assoc (List.map (fun (k, v) -> (k, `String v)) pairs) in
      params_fields @ [("arguments", args_json)]
    | None -> params_fields
  in
  let params = `Assoc params_fields in
  match send_request t ~method_:Notifications.prompts_get ~params () with
  | Error e -> Error e
  | Ok result -> Mcp_types.prompt_result_of_yojson result

(* ── close ────────────────────────────────────────── *)

let close t =
  Stdio_transport.close t.transport
