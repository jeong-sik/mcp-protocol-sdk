(** MCP HTTP Server using Streamable HTTP transport (cohttp-eio). *)

open Mcp_protocol

(* ── server type ─────────────────────────────── *)

type t = {
  handler : Mcp_protocol_eio.Handler.t;
  session : Http_session.t;
  broadcaster : Sse.Broadcaster.t;
  mutable log_level : Logging.log_level;
}

let create ~name ~version ?instructions () = {
  handler = Mcp_protocol_eio.Handler.create ~name ~version ?instructions ();
  session = Http_session.create ();
  broadcaster = Sse.Broadcaster.create ();
  log_level = Logging.Warning;
}

let add_tool tool handler s =
  { s with handler = Mcp_protocol_eio.Handler.add_tool tool handler s.handler }

let add_resource resource handler s =
  { s with handler = Mcp_protocol_eio.Handler.add_resource resource handler s.handler }

let add_prompt prompt handler s =
  { s with handler = Mcp_protocol_eio.Handler.add_prompt prompt handler s.handler }

let add_completion_handler handler s =
  { s with handler = Mcp_protocol_eio.Handler.add_completion_handler handler s.handler }

(* ── helpers ─────────────────────────────────── *)

let json_content_type = "application/json"
let _sse_content_type = "text/event-stream"

let respond_json ~status body =
  let json_str = Yojson.Safe.to_string body in
  Cohttp_eio.Server.respond_string
    ~headers:(Http.Header.of_list [
      ("Content-Type", json_content_type);
    ])
    ~status ~body:json_str ()

let respond_json_with_session session ~status body =
  let headers = [("Content-Type", json_content_type)] in
  let headers = match Http_session.session_id session with
    | Some sid -> (Http_session.header_name, sid) :: headers
    | None -> headers
  in
  Cohttp_eio.Server.respond_string
    ~headers:(Http.Header.of_list headers)
    ~status ~body:(Yojson.Safe.to_string body) ()

let respond_empty ~status ?(extra_headers=[]) session =
  let headers = extra_headers in
  let headers = match Http_session.session_id session with
    | Some sid -> (Http_session.header_name, sid) :: headers
    | None -> headers
  in
  Cohttp_eio.Server.respond_string
    ~headers:(Http.Header.of_list headers)
    ~status ~body:"" ()

(** Check if JSON body contains an initialize request *)
let is_initialize_request json =
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "method" fields with
     | Some (`String m) -> m = Notifications.initialize
     | _ -> false)
  | `List items ->
    List.exists (fun item ->
      match item with
      | `Assoc fields ->
        (match List.assoc_opt "method" fields with
         | Some (`String m) -> m = Notifications.initialize
         | _ -> false)
      | _ -> false
    ) items
  | _ -> false

(* ── context builder ─────────────────────────── *)

(** Build a handler context that sends notifications via SSE broadcaster *)
let make_context s : Mcp_protocol_eio.Handler.context =
  let send_notification ~method_ ~params =
    let msg = Jsonrpc.make_notification ~method_ ?params () in
    let json_str = Yojson.Safe.to_string (Jsonrpc.message_to_yojson msg) in
    let event_id = Http_session.next_event_id s.session in
    let evt = Sse.event "message" json_str |> Sse.with_id event_id in
    Sse.Broadcaster.broadcast s.broadcaster evt;
    Ok ()
  in
  let send_log level message =
    if Logging.should_log ~min_level:s.log_level ~msg_level:level then
      let msg = Logging.{ level; logger = None; data = `String message } in
      send_notification
        ~method_:Notifications.logging_message
        ~params:(Some (Logging.logging_message_to_yojson msg))
    else
      Ok ()
  in
  let send_progress ~token ~progress ~total =
    let p = Mcp_result.{
      progress_token = token;
      progress;
      total;
      message = None;
    } in
    send_notification
      ~method_:Notifications.progress
      ~params:(Some (Mcp_result.progress_to_yojson p))
  in
  let request_sampling _params =
    Error "sampling/createMessage not supported over HTTP transport"
  in
  let request_roots_list () =
    Error "roots/list not supported over HTTP transport"
  in
  let request_elicitation _params =
    Error "elicitation/create not supported over HTTP transport"
  in
  { send_notification; send_log; send_progress;
    request_sampling; request_roots_list; request_elicitation }

(* ── dispatch logic ──────────────────────────── *)

(** Parse JSON-RPC and dispatch through handler, returning a response function *)
let dispatch_jsonrpc s json is_init : Cohttp_eio.Server.response =
  let msg = Jsonrpc.message_of_yojson json in
  match msg with
  | Error parse_err ->
    let err = Jsonrpc.make_error ~id:(Jsonrpc.Int 0)
      ~code:Error_codes.parse_error
      ~message:(Printf.sprintf "JSON-RPC parse error: %s" parse_err) () in
    respond_json_with_session s.session ~status:`OK
      (Jsonrpc.message_to_yojson err)
  | Ok parsed_msg ->
    let ctx = make_context s in
    let log_level_ref = ref s.log_level in
    let response = Mcp_protocol_eio.Handler.dispatch
      s.handler ctx log_level_ref parsed_msg in
    s.log_level <- !log_level_ref;
    if is_init then begin
      (match Http_session.state s.session with
       | Http_session.Uninitialized ->
         ignore (Http_session.initialize s.session)
       | _ -> ());
      ignore (Http_session.ready s.session)
    end;
    (match response with
     | Some resp_msg ->
       respond_json_with_session s.session ~status:`OK
         (Jsonrpc.message_to_yojson resp_msg)
     | None ->
       respond_empty ~status:`Accepted s.session)

(* ── POST handler ────────────────────────────── *)

let handle_post s request body_str : Cohttp_eio.Server.response =
  match Yojson.Safe.from_string body_str with
  | exception Yojson.Json_error msg ->
    let err = Jsonrpc.make_error ~id:(Jsonrpc.Int 0)
      ~code:Error_codes.parse_error
      ~message:(Printf.sprintf "JSON parse error: %s" msg) () in
    respond_json ~status:`OK (Jsonrpc.message_to_yojson err)
  | json ->
    let is_init = is_initialize_request json in
    if is_init then
      dispatch_jsonrpc s json true
    else
      let header_value =
        Http.Header.get (Http.Request.headers request) Http_session.header_name
      in
      match Http_session.validate s.session header_value with
      | Error (`Bad_request msg) ->
        respond_json ~status:`Bad_request (`Assoc ["error", `String msg])
      | Error `Not_found ->
        respond_json ~status:`Not_found
          (`Assoc ["error", `String "Session not found"])
      | Ok () ->
        dispatch_jsonrpc s json false

(* ── GET handler (SSE) ───────────────────────── *)

let handle_get s request : Cohttp_eio.Server.response =
  let header_value =
    Http.Header.get (Http.Request.headers request) Http_session.header_name
  in
  match Http_session.validate s.session header_value with
  | Error (`Bad_request msg) ->
    respond_json ~status:`Bad_request (`Assoc ["error", `String msg])
  | Error `Not_found ->
    respond_json ~status:`Not_found
      (`Assoc ["error", `String "Session not found"])
  | Ok () ->
    let _client_id, _stream = Sse.Broadcaster.subscribe s.broadcaster in
    (* v0.9.0: SSE streaming is simplified. Full streaming requires
       Expert mode which will be added in a future version. *)
    Cohttp_eio.Server.respond_string
      ~headers:(Http.Header.of_list [
        ("Content-Type", _sse_content_type);
        ("Cache-Control", "no-cache");
        ("Connection", "keep-alive");
        ("X-Accel-Buffering", "no");
      ])
      ~status:`OK ~body:"" ()

(* ── DELETE handler ──────────────────────────── *)

let handle_delete s request : Cohttp_eio.Server.response =
  let header_value =
    Http.Header.get (Http.Request.headers request) Http_session.header_name
  in
  match Http_session.validate s.session header_value with
  | Error (`Bad_request msg) ->
    respond_json ~status:`Bad_request (`Assoc ["error", `String msg])
  | Error `Not_found ->
    respond_json ~status:`Not_found
      (`Assoc ["error", `String "Session not found"])
  | Ok () ->
    Http_session.close s.session;
    respond_empty ~status:`OK s.session

(* ── OPTIONS handler ─────────────────────────── *)

let handle_options session : Cohttp_eio.Server.response =
  respond_empty ~status:`OK
    ~extra_headers:[
      ("Access-Control-Allow-Origin", "*");
      ("Access-Control-Allow-Methods", "GET, POST, DELETE, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type, Accept, Mcp-Session-Id");
      ("Access-Control-Expose-Headers", "Mcp-Session-Id");
    ]
    session

(* ── main callback ───────────────────────────── *)

let callback s ?(prefix="/mcp") _conn request body =
  let path = Http.Request.resource request in
  let meth = Http.Request.meth request in
  if path <> prefix && not (String.length path > String.length prefix
     && String.sub path 0 (String.length prefix) = prefix) then
    Cohttp_eio.Server.respond_string
      ~status:`Not_found ~body:"Not found" ()
  else
    match meth with
    | `POST ->
      let body_str = Eio.Buf_read.of_flow ~max_size:(10 * 1024 * 1024) body
                     |> Eio.Buf_read.take_all in
      handle_post s request body_str
    | `GET ->
      handle_get s request
    | `DELETE ->
      handle_delete s request
    | `OPTIONS ->
      handle_options s.session
    | _ ->
      Cohttp_eio.Server.respond_string
        ~status:`Method_not_allowed
        ~body:"Method not allowed" ()

(* ── standalone run ──────────────────────────── *)

let run s ~sw ~env:(env : Eio_unix.Stdenv.base) ?(port=8080) ?(prefix="/mcp") ?stop () =
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let socket = Eio.Net.listen ~sw net addr ~backlog:128 in
  let cohttp_server = Cohttp_eio.Server.make
    ~callback:(fun conn request body ->
      callback s ~prefix conn request body)
    () in
  match stop with
  | Some stop ->
    Cohttp_eio.Server.run ~stop socket cohttp_server
      ~on_error:(fun exn ->
        Printf.eprintf "[%s] HTTP error: %s\n%!"
          (Mcp_protocol_eio.Handler.name s.handler)
          (Printexc.to_string exn))
  | None ->
    let stop_promise, _stop_resolver = Eio.Promise.create () in
    Cohttp_eio.Server.run ~stop:stop_promise socket cohttp_server
      ~on_error:(fun exn ->
        Printf.eprintf "[%s] HTTP error: %s\n%!"
          (Mcp_protocol_eio.Handler.name s.handler)
          (Printexc.to_string exn))
