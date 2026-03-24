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
  next_id : int Atomic.t;
  mutable session_id : string option;
  sampling_handler : sampling_handler option;
  roots_handler : roots_handler option;
  elicitation_handler : elicitation_handler option;
  notification_handler : notification_handler option;
  timeout_fn : timeout_fn option;
  access_token : string option;
  headers : (string * string) list;
}

let default_timeout = 60.0

(* ── construction ────────────────────────────── *)

let create ~endpoint ~net ~sw ?(headers = []) ?clock ?access_token () =
  let timeout_fn = match clock with
    | Some c -> Some { run = fun d f -> Eio.Time.with_timeout_exn c d f }
    | None -> None
  in
  {
    endpoint = Uri.of_string endpoint;
    client = Tls_helpers.make_client net;
    sw;
    next_id = Atomic.make 1;
    session_id = None;
    sampling_handler = None;
    roots_handler = None;
    elicitation_handler = None;
    notification_handler = None;
    timeout_fn;
    access_token;
    headers;
  }

let on_sampling handler t = { t with sampling_handler = Some handler }
let on_roots_list handler t = { t with roots_handler = Some handler }
let on_elicitation handler t = { t with elicitation_handler = Some handler }
let on_notification handler t = { t with notification_handler = Some handler }

(* ── HTTP helpers ────────────────────────────── *)

let json_content_type = "application/json"
let streamable_accept = json_content_type ^ ", " ^ Http_negotiation.sse_content_type

let make_headers t =
  let h = [
    ("Content-Type", json_content_type);
    ("Accept", streamable_accept);
    ("Mcp-Protocol-Version", Version.latest);
  ] in
  let h = t.headers @ h in
  let h = match t.access_token with
    | Some token -> ("Authorization", Printf.sprintf "Bearer %s" token) :: h
    | None -> h
  in
  match t.session_id with
  | Some sid -> (Http_session.header_name, sid) :: h
  | None -> h

let parse_sse_body raw =
  let trim_cr line =
    let len = String.length line in
    if len > 0 && Char.equal line.[len - 1] '\r' then
      String.sub line 0 (len - 1)
    else
      line
  in
  let data_of_line line =
    if String.length line >= 5 && String.sub line 0 5 = "data:" then
      let payload = String.sub line 5 (String.length line - 5) in
      if String.length payload > 0 && Char.equal payload.[0] ' ' then
        Some (String.sub payload 1 (String.length payload - 1))
      else
        Some payload
    else
      None
  in
  let flush_event current acc =
    match current with
    | [] -> acc
    | _ -> String.concat "\n" (List.rev current) :: acc
  in
  let events =
    List.fold_left
      (fun (current, acc) raw_line ->
        let line = trim_cr raw_line in
        if String.equal line "" then
          ([], flush_event current acc)
        else
          match data_of_line line with
          | Some payload -> (payload :: current, acc)
          | None -> (current, acc))
      ([], []) (String.split_on_char '\n' raw)
    |> fun (current, acc) -> List.rev (flush_event current acc)
  in
  match List.rev events with
  | [] -> None
  | last :: _ when last = "" -> None
  | last :: _ -> (
      try Some (Yojson.Safe.from_string last)
      with Yojson.Json_error _ -> None)

let parse_response_body ~content_type body_str =
  let is_sse =
    match content_type with
    | Some ct ->
      let lowered = String.lowercase_ascii ct in
      String.length lowered >= 17
      && String.sub lowered 0 17 = "text/event-stream"
    | None -> false
  in
  if is_sse then parse_sse_body body_str
  else
    try Some (Yojson.Safe.from_string body_str)
    with Yojson.Json_error _ -> None

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
  (status, resp_headers, body_str)

(* ── notification ────────────────────────────── *)

let send_notification t ~method_ ?params () =
  let msg = Jsonrpc.make_notification ~method_ ?params () in
  let json = Jsonrpc.message_to_yojson msg in
  match post_json t json with
  | exception exn ->
    Error (Printf.sprintf "HTTP notification failed: %s"
      (Printexc.to_string exn))
  | (`OK, _, _) | (`Accepted, _, _) -> Ok ()
  | (status, _, body) ->
    Error (Printf.sprintf "HTTP %d: %s"
      (Http.Status.to_int status) body)

(* ── request/response ────────────────────────── *)

let do_request t ~method_ ?params () =
  if method_ <> Notifications.initialize && t.session_id = None then
    Error "Not initialized: call initialize first"
  else
  let id = Jsonrpc.Int (Atomic.fetch_and_add t.next_id 1) in
  let msg = Jsonrpc.make_request ~id ~method_ ?params () in
  let json = Jsonrpc.message_to_yojson msg in
  match post_json t json with
  | exception exn ->
    Error (Printf.sprintf "HTTP request failed: %s" (Printexc.to_string exn))
  | (status, resp_headers, body_str) ->
    if status <> `OK then
      Error (Printf.sprintf "HTTP %d: %s"
        (Http.Status.to_int status) body_str)
    else
      let content_type = Http.Header.get resp_headers "content-type" in
      match parse_response_body ~content_type body_str with
      | None ->
        Error "JSON parse error: empty or unparseable response body"
      | Some json ->
        match Jsonrpc.message_of_yojson json with
        | Error e -> Error (Printf.sprintf "JSON-RPC parse error: %s" e)
        | Ok (Jsonrpc.Response resp) -> Ok resp.result
        | Ok (Jsonrpc.Error err) -> Error err.error.message
        | Ok _ -> Error "Unexpected message type in response"

let send_request t ~method_ ?params ?(timeout = default_timeout) () =
  match t.timeout_fn with
  | None -> do_request t ~method_ ?params ()
  | Some tf ->
    (* Capture the request ID before do_request increments next_id *)
    let request_id = Jsonrpc.Int (Atomic.get t.next_id) in
    begin try
      tf.run timeout (fun () -> do_request t ~method_ ?params ())
    with Eio.Time.Timeout ->
      (* Best effort: notify peer we gave up *)
      let id = request_id in
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

let collect_pages fetch_page =
  let module Cursor_set = Set.Make (String) in
  let rec loop seen cursor acc_rev =
    match fetch_page cursor with
    | Error _ as err -> err
    | Ok (page, next_cursor) ->
      begin match next_cursor with
      | Some value when Cursor_set.mem value seen ->
        Error (Printf.sprintf "Pagination cursor loop detected: %s" value)
      | Some value ->
        loop (Cursor_set.add value seen) (Some value)
          (List.rev_append page acc_rev)
      | None -> Ok (List.rev_append page acc_rev |> List.rev)
      end
  in
  loop Cursor_set.empty None []

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
  | Ok result ->
    Result.map fst
      (Mcp_protocol_eio.Handler.parse_paginated_list_field "tools" Mcp_types.tool_of_yojson result)

let list_tools_all t =
  collect_pages (fun cursor ->
    let params = Option.map (fun c -> `Assoc [("cursor", `String c)]) cursor in
    match send_request t ~method_:Notifications.tools_list ?params () with
    | Error e -> Error e
    | Ok result ->
      Mcp_protocol_eio.Handler.parse_paginated_list_field "tools" Mcp_types.tool_of_yojson result)

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
    Result.map fst
      (Mcp_protocol_eio.Handler.parse_paginated_list_field "resources" Mcp_types.resource_of_yojson result)

let list_resources_all t =
  collect_pages (fun cursor ->
    let params = Option.map (fun c -> `Assoc [("cursor", `String c)]) cursor in
    match send_request t ~method_:Notifications.resources_list ?params () with
    | Error e -> Error e
    | Ok result ->
      Mcp_protocol_eio.Handler.parse_paginated_list_field "resources" Mcp_types.resource_of_yojson result)

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

let list_resource_templates ?cursor t =
  let params = match cursor with
    | Some c -> Some (`Assoc [("cursor", `String c)])
    | None -> None
  in
  match send_request t ~method_:Notifications.resources_templates_list ?params () with
  | Error e -> Error e
  | Ok result ->
    Mcp_protocol_eio.Handler.parse_list_field "resourceTemplates" Mcp_types.resource_template_of_yojson result

(* ── prompts ─────────────────────────────────── *)

let list_prompts ?cursor t =
  let params = match cursor with
    | Some c -> Some (`Assoc [("cursor", `String c)])
    | None -> None
  in
  match send_request t ~method_:Notifications.prompts_list ?params () with
  | Error e -> Error e
  | Ok result ->
    Result.map fst
      (Mcp_protocol_eio.Handler.parse_paginated_list_field "prompts" Mcp_types.prompt_of_yojson result)

let list_prompts_all t =
  collect_pages (fun cursor ->
    let params = Option.map (fun c -> `Assoc [("cursor", `String c)]) cursor in
    match send_request t ~method_:Notifications.prompts_list ?params () with
    | Error e -> Error e
    | Ok result ->
      Mcp_protocol_eio.Handler.parse_paginated_list_field "prompts" Mcp_types.prompt_of_yojson result)

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
