(** MCP HTTP Client using Streamable HTTP transport (cohttp-eio). *)

open Mcp_protocol

(* ── types ───────────────────────────────────── *)

type sampling_handler =
  Sampling.create_message_params -> (Sampling.create_message_result, string) result
type roots_handler =
  unit -> (Mcp_types.root list, string) result
type elicitation_handler =
  Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result
type notification_handler = string -> Yojson.Safe.t option -> unit

type timeout_fn = { run : 'a. float -> (unit -> 'a) -> 'a }

type t = {
  endpoint : Uri.t;
  client : Cohttp_eio.Client.t;
  sw : Eio.Switch.t;
  mutable next_id : int;
  mutable session_id : string option;
  sampling_handler : sampling_handler option;
  roots_handler : roots_handler option;
  elicitation_handler : elicitation_handler option;
  notification_handler : notification_handler option;
  timeout_fn : timeout_fn option;
  access_token : string option;
}

let default_timeout = 60.0

(* ── construction ────────────────────────────── *)

let create ~endpoint ~net ~sw ?clock ?access_token () =
  let timeout_fn = match clock with
    | Some c -> Some { run = fun d f -> Eio.Time.with_timeout_exn c d f }
    | None -> None
  in
  {
    endpoint = Uri.of_string endpoint;
    client = Cohttp_eio.Client.make ~https:None net;
    sw;
    next_id = 1;
    session_id = None;
    sampling_handler = None;
    roots_handler = None;
    elicitation_handler = None;
    notification_handler = None;
    timeout_fn;
    access_token;
  }

let on_sampling handler t = { t with sampling_handler = Some handler }
let on_roots_list handler t = { t with roots_handler = Some handler }
let on_elicitation handler t = { t with elicitation_handler = Some handler }
let on_notification handler t = { t with notification_handler = Some handler }

(* ── HTTP helpers ────────────────────────────── *)

let json_content_type = "application/json"

let make_headers t =
  let h = [
    ("Content-Type", json_content_type);
    ("Accept", json_content_type);
  ] in
  let h = match t.access_token with
    | Some token -> ("Authorization", Printf.sprintf "Bearer %s" token) :: h
    | None -> h
  in
  match t.session_id with
  | Some sid -> (Http_session.header_name, sid) :: h
  | None -> h

let post_json t body_json =
  let body_str = Yojson.Safe.to_string body_json in
  let body = Cohttp_eio.Body.of_string body_str in
  let headers = Http.Header.of_list (make_headers t) in
  Eio.Switch.run @@ fun sw ->
  let resp, resp_body =
    Cohttp_eio.Client.post t.client ~sw
      ~body ~headers t.endpoint
  in
  (* Extract session ID from response headers *)
  let resp_headers = Http.Response.headers resp in
  (match Http.Header.get resp_headers Http_session.header_name with
   | Some sid -> t.session_id <- Some sid
   | None -> ());
  let status = Http.Response.status resp in
  let body_str =
    Eio.Buf_read.of_flow ~max_size:(10 * 1024 * 1024) resp_body
    |> Eio.Buf_read.take_all
  in
  (status, body_str)

(* ── notification ────────────────────────────── *)

let send_notification t ~method_ ?params () =
  let msg = Jsonrpc.make_notification ~method_ ?params () in
  let json = Jsonrpc.message_to_yojson msg in
  match post_json t json with
  | exception exn ->
    Error (Printf.sprintf "HTTP notification failed: %s"
      (Printexc.to_string exn))
  | (`OK, _) | (`Accepted, _) -> Ok ()
  | (status, body) ->
    Error (Printf.sprintf "HTTP %d: %s"
      (Http.Status.to_int status) body)

(* ── request/response ────────────────────────── *)

let do_request t ~method_ ?params () =
  if method_ <> Notifications.initialize && t.session_id = None then
    Error "Not initialized: call initialize first"
  else
  let id = Jsonrpc.Int t.next_id in
  t.next_id <- t.next_id + 1;
  let msg = Jsonrpc.make_request ~id ~method_ ?params () in
  let json = Jsonrpc.message_to_yojson msg in
  match post_json t json with
  | exception exn ->
    Error (Printf.sprintf "HTTP request failed: %s" (Printexc.to_string exn))
  | (status, body_str) ->
    if status <> `OK then
      Error (Printf.sprintf "HTTP %d: %s"
        (Http.Status.to_int status) body_str)
    else
      match Yojson.Safe.from_string body_str with
      | exception Yojson.Json_error msg ->
        Error (Printf.sprintf "JSON parse error: %s" msg)
      | json ->
        match Jsonrpc.message_of_yojson json with
        | Error e -> Error (Printf.sprintf "JSON-RPC parse error: %s" e)
        | Ok (Jsonrpc.Response resp) -> Ok resp.result
        | Ok (Jsonrpc.Error err) -> Error err.error.message
        | Ok _ -> Error "Unexpected message type in response"

let send_request t ~method_ ?params ?(timeout = default_timeout) () =
  match t.timeout_fn with
  | None -> do_request t ~method_ ?params ()
  | Some tf ->
    begin try
      tf.run timeout (fun () -> do_request t ~method_ ?params ())
    with Eio.Time.Timeout ->
      (* Best effort: notify peer we gave up *)
      let id = Jsonrpc.Int (t.next_id - 1) in
      (try
        ignore (send_notification t
          ~method_:Notifications.cancelled
          ~params:(`Assoc [
            ("requestId", Jsonrpc.id_to_yojson id);
            ("reason", `String "Request timed out")
          ]) ())
      with
      | Out_of_memory | Stack_overflow as exn -> raise exn
      | _exn -> ());
      Error (Printf.sprintf "Request timed out after %.1fs" timeout)
    end

(* ── lifecycle ───────────────────────────────── *)

let initialize t ~client_name ~client_version =
  let params = Mcp_protocol_eio.Handler.build_initialize_params
    ~has_sampling:(Option.is_some t.sampling_handler)
    ~has_roots:(Option.is_some t.roots_handler)
    ~has_elicitation:(Option.is_some t.elicitation_handler)
    ~client_name ~client_version
  in
  match send_request t ~method_:Notifications.initialize ~params () with
  | Error e -> Error e
  | Ok result ->
    match send_notification t ~method_:Notifications.initialized () with
    | Error e ->
      Error (Printf.sprintf "Failed to send initialized notification: %s" e)
    | Ok () ->
      Mcp_types.initialize_result_of_yojson result

let ping t =
  match send_request t ~method_:Notifications.ping () with
  | Error e -> Error e
  | Ok _ -> Ok ()

(* ── tools ───────────────────────────────────── *)

let list_tools ?cursor t =
  let params = match cursor with
    | Some c -> Some (`Assoc [("cursor", `String c)])
    | None -> None
  in
  match send_request t ~method_:Notifications.tools_list ?params () with
  | Error e -> Error e
  | Ok result -> Mcp_protocol_eio.Handler.parse_list_field "tools" Mcp_types.tool_of_yojson result

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

(* ── resources ───────────────────────────────── *)

let list_resources ?cursor t =
  let params = match cursor with
    | Some c -> Some (`Assoc [("cursor", `String c)])
    | None -> None
  in
  match send_request t ~method_:Notifications.resources_list ?params () with
  | Error e -> Error e
  | Ok result ->
    Mcp_protocol_eio.Handler.parse_list_field "resources" Mcp_types.resource_of_yojson result

let read_resource t ~uri =
  let params = `Assoc [("uri", `String uri)] in
  match send_request t ~method_:Notifications.resources_read ~params () with
  | Error e -> Error e
  | Ok result ->
    Mcp_protocol_eio.Handler.parse_list_field "contents" Mcp_types.resource_contents_of_yojson result

let subscribe_resource t ~uri =
  let params = `Assoc [("uri", `String uri)] in
  match send_request t ~method_:Notifications.resources_subscribe ~params () with
  | Error e -> Error e
  | Ok _ -> Ok ()

let unsubscribe_resource t ~uri =
  let params = `Assoc [("uri", `String uri)] in
  match send_request t ~method_:Notifications.resources_unsubscribe ~params () with
  | Error e -> Error e
  | Ok _ -> Ok ()

(* ── prompts ─────────────────────────────────── *)

let list_prompts ?cursor t =
  let params = match cursor with
    | Some c -> Some (`Assoc [("cursor", `String c)])
    | None -> None
  in
  match send_request t ~method_:Notifications.prompts_list ?params () with
  | Error e -> Error e
  | Ok result ->
    Mcp_protocol_eio.Handler.parse_list_field "prompts" Mcp_types.prompt_of_yojson result

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

(* ── tasks ──────────────────────────────────── *)

let get_task t ~task_id =
  let params = `Assoc [("taskId", `String task_id)] in
  match send_request t ~method_:Notifications.tasks_get ~params () with
  | Error e -> Error e
  | Ok result -> Mcp_types.task_of_yojson result

let get_task_result t ~task_id =
  let params = `Assoc [("taskId", `String task_id)] in
  match send_request t ~method_:Notifications.tasks_result ~params () with
  | Error e -> Error e
  | Ok result -> Ok result

let list_tasks ?cursor t =
  let params = match cursor with
    | Some c -> Some (`Assoc [("cursor", `String c)])
    | None -> None
  in
  match send_request t ~method_:Notifications.tasks_list ?params () with
  | Error e -> Error e
  | Ok result -> Mcp_protocol_eio.Handler.parse_list_field "tasks" Mcp_types.task_of_yojson result

let cancel_task t ~task_id =
  let params = `Assoc [("taskId", `String task_id)] in
  match send_request t ~method_:Notifications.tasks_cancel ~params () with
  | Error e -> Error e
  | Ok result -> Mcp_types.task_of_yojson result

(* ── cleanup ─────────────────────────────────── *)

let close t =
  match t.session_id with
  | None -> Ok ()
  | Some _sid ->
    let headers = Http.Header.of_list (make_headers t) in
    match
      Eio.Switch.run @@ fun sw ->
      let resp, body =
        Cohttp_eio.Client.call t.client ~sw `DELETE
          ~headers t.endpoint
      in
      let _body_str =
        Eio.Buf_read.of_flow ~max_size:(1024 * 1024) body
        |> Eio.Buf_read.take_all
      in
      Http.Response.status resp
    with
    | exception exn ->
      Error (Printf.sprintf "DELETE failed: %s" (Printexc.to_string exn))
    | status ->
      if status = `OK then
        (t.session_id <- None; Ok ())
      else
        Error (Printf.sprintf "DELETE returned %d"
          (Http.Status.to_int status))
