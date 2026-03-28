(** MCP HTTP Server using Streamable HTTP transport (cohttp-eio). *)

open Mcp_protocol

(* ── server type ─────────────────────────────── *)

type t = {
  handler : Mcp_protocol_eio.Handler.t;
  session : Http_session.t;
  broadcaster : Sse.Broadcaster.t;
  log_level : Logging.log_level Atomic.t;
  mutable sleep : (float -> unit) option;
  auth : Auth_middleware.config option;
  next_request_id : int Atomic.t;
  pending_requests : (int, Yojson.Safe.t Eio.Promise.u) Hashtbl.t;
  pending_mutex : Eio.Mutex.t;
}

let create ~name ~version ?instructions ?enable_logging ?auth () = {
  handler =
    Mcp_protocol_eio.Handler.create ~name ~version ?instructions
      ?enable_logging ();
  session = Http_session.create ();
  broadcaster = Sse.Broadcaster.create ();
  log_level = Atomic.make Logging.Warning;
  sleep = None;
  auth;
  next_request_id = Atomic.make 1;
  pending_requests = Hashtbl.create 8;
  pending_mutex = Eio.Mutex.create ();
}

let add_tool tool handler s =
  { s with handler = Mcp_protocol_eio.Handler.add_tool tool handler s.handler }

let add_resource resource handler s =
  { s with handler = Mcp_protocol_eio.Handler.add_resource resource handler s.handler }

let add_resource_template template handler s =
  { s with handler = Mcp_protocol_eio.Handler.add_resource_template template handler s.handler }

let add_prompt prompt handler s =
  { s with handler = Mcp_protocol_eio.Handler.add_prompt prompt handler s.handler }

let add_completion_handler handler s =
  { s with handler = Mcp_protocol_eio.Handler.add_completion_handler handler s.handler }

type task_handlers = Mcp_protocol_eio.Handler.task_handlers

let add_task_handlers handlers s =
  { s with handler = Mcp_protocol_eio.Handler.add_task_handlers handlers s.handler }

(* ── helpers ─────────────────────────────────── *)

let json_content_type = "application/json"
let sse_content_type = "text/event-stream"

let cors_headers = [
  ("Access-Control-Allow-Origin", "*");
  ("Access-Control-Allow-Methods", "GET, POST, DELETE, OPTIONS");
  ("Access-Control-Allow-Headers", "Content-Type, Accept, Mcp-Session-Id, Mcp-Protocol-Version");
  ("Access-Control-Expose-Headers", "Mcp-Session-Id");
]

(* ── DNS rebinding protection (MCP 2025-11-25) ── *)

(** Check if an origin is a localhost origin using URI parsing.
    Per spec: servers binding to localhost MUST validate Origin header
    to prevent DNS rebinding attacks. *)
let is_localhost_origin origin =
  let uri = Uri.of_string origin in
  match Uri.scheme uri, Uri.host uri, Uri.userinfo uri with
  | Some ("http" | "https"), Some h, None ->
    List.mem h ["localhost"; "127.0.0.1"; "::1"; "[::1]"]
  | _ -> false

(** Validate Origin header. Returns [Ok ()] if safe, [Error msg] if blocked.
    - No Origin: allowed (same-origin requests omit it)
    - Localhost origin (with any port): allowed
    - Non-localhost origin: blocked *)
let validate_origin request =
  match Http.Header.get (Http.Request.headers request) "origin" with
  | None -> Ok ()
  | Some origin ->
    if is_localhost_origin origin then Ok ()
    else Error (Printf.sprintf "Origin %S is not a localhost origin" origin)

let respond_json ~status body =
  let json_str = Yojson.Safe.to_string body in
  let headers = ("Content-Type", json_content_type) :: cors_headers in
  Cohttp_eio.Server.respond_string
    ~headers:(Http.Header.of_list headers)
    ~status ~body:json_str ()

let respond_json_with_session session ~status body =
  let headers = ("Content-Type", json_content_type)
    :: ("Mcp-Protocol-Version", Version.latest)
    :: cors_headers in
  let headers = match Http_session.session_id session with
    | Some sid -> (Http_session.header_name, sid) :: headers
    | None -> headers
  in
  Cohttp_eio.Server.respond_string
    ~headers:(Http.Header.of_list headers)
    ~status ~body:(Yojson.Safe.to_string body) ()

let respond_empty ~status ?(extra_headers=[]) session =
  let headers = extra_headers @ cors_headers in
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

(** Build a handler context.  When [request_stream] is provided,
    notifications are sent to that per-request SSE stream instead of
    the global broadcaster (used for POST-response SSE mode). *)
let make_context ?(request_stream : Sse.event option Eio.Stream.t option) s
    : Mcp_protocol_eio.Handler.context =
  let send_event evt =
    match request_stream with
    | Some stream -> Eio.Stream.add stream (Some evt); Ok ()
    | None -> Sse.Broadcaster.broadcast s.broadcaster evt; Ok ()
  in
  let send_notification ~method_ ~params =
    let msg = Jsonrpc.make_notification ~method_ ?params () in
    let json_str = Yojson.Safe.to_string (Jsonrpc.message_to_yojson msg) in
    let event_id = Http_session.next_event_id s.session in
    let evt = Sse.event "message" json_str |> Sse.with_id event_id in
    send_event evt
  in
  let send_log level message =
    if Logging.should_log ~min_level:(Atomic.get s.log_level) ~msg_level:level then
      let msg = Logging.{ level; logger = None; data = `String message } in
      send_notification
        ~method_:Notifications.logging_message
        ~params:(Some (Logging.logging_message_to_yojson msg))
    else
      Ok ()
  in
  let send_progress ~token ~progress ~message ~total =
    let p = Mcp_result.{
      progress_token = token;
      progress;
      total;
      message;
    } in
    send_notification
      ~method_:Notifications.progress
      ~params:(Some (Mcp_result.progress_to_yojson p))
  in
  (* Send a JSON-RPC request via SSE and wait for the client's POST response *)
  let server_request ~method_ ~params =
    let id_int = Atomic.fetch_and_add s.next_request_id 1 in
    let id = Jsonrpc.Int id_int in
    let msg = Jsonrpc.make_request ~id ~method_ ~params () in
    let json_str = Yojson.Safe.to_string (Jsonrpc.message_to_yojson msg) in
    let event_id = Http_session.next_event_id s.session in
    let evt = Sse.event "message" json_str |> Sse.with_id event_id in
    let promise, resolver = Eio.Promise.create () in
    Eio.Mutex.use_rw ~protect:true s.pending_mutex (fun () ->
      Hashtbl.add s.pending_requests id_int resolver);
    ignore (send_event evt);
    let result = Eio.Promise.await promise in
    Eio.Mutex.use_rw ~protect:true s.pending_mutex (fun () ->
      Hashtbl.remove s.pending_requests id_int);
    Ok result
  in
  let request_sampling params =
    let json = Sampling.create_message_params_to_yojson params in
    match server_request ~method_:Notifications.sampling_create_message ~params:json with
    | Error e -> Error e
    | Ok result -> Sampling.create_message_result_of_yojson result
  in
  let request_roots_list () =
    match server_request ~method_:Notifications.roots_list ~params:(`Assoc []) with
    | Error e -> Error e
    | Ok result ->
      begin match result with
      | `Assoc fields ->
        begin match List.assoc_opt "roots" fields with
        | Some (`List items) ->
          List.fold_left (fun acc j ->
            match acc with
            | Error _ -> acc
            | Ok lst ->
              match Mcp_types.root_of_yojson j with
              | Ok r -> Ok (r :: lst)
              | Error e -> Error (Printf.sprintf "Failed to parse root: %s" e)
          ) (Ok []) items
          |> Result.map List.rev
        | _ -> Error "Missing 'roots' in response"
        end
      | _ -> Error "Invalid roots/list response"
      end
  in
  let request_elicitation params =
    let json = Mcp_types.elicitation_params_to_yojson params in
    match server_request ~method_:Notifications.elicitation_create ~params:json with
    | Error e -> Error e
    | Ok result -> Mcp_types.elicitation_result_of_yojson result
  in
  { send_notification; send_log; send_progress;
    request_sampling; request_roots_list; request_elicitation }

(* ── session validation helper ───────────────── *)

let get_session_header request =
  Http.Header.get (Http.Request.headers request) Http_session.header_name

let validate_session_or_error session request =
  match Http_session.validate session (get_session_header request) with
  | Ok () -> Ok ()
  | Error (`Bad_request msg) ->
    Error (respond_json ~status:`Bad_request (`Assoc ["error", `String msg]))
  | Error `Not_found ->
    Error (respond_json ~status:`Not_found
      (`Assoc ["error", `String "Session not found"]))

(* ── dispatch logic ──────────────────────────── *)

(** Check if the client accepts SSE responses for POST requests. *)
let accepts_sse request =
  match Http.Header.get (Http.Request.headers request) "accept" with
  | Some accept -> String.contains (String.lowercase_ascii accept) 'e'
    && (let a = String.lowercase_ascii accept in
        let p = "text/event-stream" in
        let rec find i = i + String.length p <= String.length a &&
          (String.sub a i (String.length p) = p || find (i + 1))
        in find 0)
  | None -> false

(** Parse JSON-RPC and dispatch through handler, returning a direct JSON response. *)
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
    let log_level_ref = ref (Atomic.get s.log_level) in
    let response = Mcp_protocol_eio.Handler.dispatch
      s.handler ctx log_level_ref parsed_msg in
    Atomic.set s.log_level !log_level_ref;
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

(** Dispatch via POST-response SSE streaming.
    Notifications and the final result are sent as SSE events in the response body.
    The response closure is returned immediately; Switch.run + Fiber.fork run
    inside it so cohttp can start streaming before the handler blocks on
    server-to-client round-trips (sampling, elicitation).
    Previous structure had Switch.run outside the closure, causing a deadlock:
    Switch waited for the handler fiber (blocked on Promise.await) before
    returning the closure to cohttp, so streaming never started. *)
let dispatch_jsonrpc_sse s json is_init : Cohttp_eio.Server.response =
  let msg = Jsonrpc.message_of_yojson json in
  match msg with
  | Error parse_err ->
    let err = Jsonrpc.make_error ~id:(Jsonrpc.Int 0)
      ~code:Error_codes.parse_error
      ~message:(Printf.sprintf "JSON-RPC parse error: %s" parse_err) () in
    respond_json_with_session s.session ~status:`OK
      (Jsonrpc.message_to_yojson err)
  | Ok parsed_msg ->
    let request_stream : Sse.event option Eio.Stream.t = Eio.Stream.create 64 in
    let ctx = make_context ~request_stream s in
    let sse_headers =
      [ ("Content-Type", sse_content_type);
        ("Cache-Control", "no-cache");
        ("Connection", "keep-alive");
        ("Mcp-Protocol-Version", Version.latest);
      ] @ cors_headers
    in
    let sse_headers = match Http_session.session_id s.session with
      | Some sid -> (Http_session.header_name, sid) :: sse_headers
      | None -> sse_headers
    in
    let flow = Sse_flow.create request_stream in
    let source = Sse_flow.as_source flow in
    fun writer ->
      Eio.Switch.run (fun sw ->
        Eio.Fiber.fork ~sw (fun () ->
          Fun.protect
            (fun () ->
              let log_level_ref = ref (Atomic.get s.log_level) in
              let response = Mcp_protocol_eio.Handler.dispatch
                s.handler ctx log_level_ref parsed_msg in
              Atomic.set s.log_level !log_level_ref;
              if is_init then begin
                (match Http_session.state s.session with
                 | Http_session.Uninitialized ->
                   ignore (Http_session.initialize s.session)
                 | _ -> ());
                ignore (Http_session.ready s.session)
              end;
              (match response with
               | Some resp_msg ->
                 let json_str = Yojson.Safe.to_string (Jsonrpc.message_to_yojson resp_msg) in
                 let event_id = Http_session.next_event_id s.session in
                 let evt = Sse.event "message" json_str |> Sse.with_id event_id in
                 Eio.Stream.add request_stream (Some evt)
               | None -> ()))
            ~finally:(fun () ->
              Eio.Stream.add request_stream None));
        let base = Cohttp_eio.Server.respond
          ~headers:(Http.Header.of_list sse_headers)
          ~status:`OK ~body:source () in
        base writer)

(* ── POST handler ────────────────────────────── *)

(** H5 fix: Accept pre-parsed JSON to avoid double-parsing in callback. *)
let handle_post_parsed s request json_result : Cohttp_eio.Server.response =
  match json_result with
  | Error msg ->
    let err = Jsonrpc.make_error ~id:(Jsonrpc.Int 0)
      ~code:Error_codes.parse_error
      ~message:(Printf.sprintf "JSON parse error: %s" msg) () in
    respond_json ~status:`OK (Jsonrpc.message_to_yojson err)
  | Ok (`List _) ->
    let err = Jsonrpc.make_error ~id:(Jsonrpc.Null)
      ~code:Error_codes.invalid_request
      ~message:"JSON-RPC batching is not supported" () in
    respond_json ~status:`OK (Jsonrpc.message_to_yojson err)
  | Ok json ->
    (* Check if this is a client response to a server-initiated request *)
    let resolved = match Jsonrpc.message_of_yojson json with
      | Ok (Jsonrpc.Response resp) ->
        let id_int = match resp.id with Jsonrpc.Int i -> Some i | _ -> None in
        (match id_int with
         | Some i ->
           let resolver = Eio.Mutex.use_rw ~protect:true s.pending_mutex (fun () ->
             match Hashtbl.find_opt s.pending_requests i with
             | Some r -> Hashtbl.remove s.pending_requests i; Some r
             | None -> None) in
           (match resolver with
            | Some r -> Eio.Promise.resolve r resp.result; true
            | None -> false)
         | None -> false)
      | Ok (Jsonrpc.Error err) ->
        let id_int = match err.id with Jsonrpc.Int i -> Some i | _ -> None in
        (match id_int with
         | Some i ->
           let resolver = Eio.Mutex.use_rw ~protect:true s.pending_mutex (fun () ->
             match Hashtbl.find_opt s.pending_requests i with
             | Some r -> Hashtbl.remove s.pending_requests i; Some r
             | None -> None) in
           (match resolver with
            | Some r ->
              Eio.Promise.resolve r (`Assoc [("error", `String err.error.message)]);
              true
            | None -> false)
         | None -> false)
      | _ -> false
    in
    if resolved then
      respond_empty ~status:`Accepted s.session
    else begin
      let is_init = is_initialize_request json in
      let dispatch = if accepts_sse request && not is_init
        then dispatch_jsonrpc_sse else dispatch_jsonrpc in
      if is_init then
        dispatch s json true
      else
        match validate_session_or_error s.session request with
        | Error resp -> resp
        | Ok () -> dispatch s json false
    end

let _handle_post s request body_str : Cohttp_eio.Server.response =
  let json_result =
    try Ok (Yojson.Safe.from_string body_str)
    with Yojson.Json_error msg -> Error msg
  in
  handle_post_parsed s request json_result

(* ── GET handler (SSE) ───────────────────────── *)

let handle_get s request : Cohttp_eio.Server.response =
  match validate_session_or_error s.session request with
  | Error resp -> resp
  | Ok () ->
    let client_id, stream = Sse.Broadcaster.subscribe s.broadcaster in
    let flow = Sse_flow.create ?sleep:s.sleep stream in
    let sse_headers =
      [ ("Content-Type", sse_content_type);
        ("Cache-Control", "no-cache");
        ("Connection", "keep-alive");
        ("X-Accel-Buffering", "no");
      ] @ cors_headers
    in
    let sse_headers = match Http_session.session_id s.session with
      | Some sid -> (Http_session.header_name, sid) :: sse_headers
      | None -> sse_headers
    in
    let base = Cohttp_eio.Server.respond
      ~headers:(Http.Header.of_list sse_headers)
      ~status:`OK ~body:(Sse_flow.as_source flow) ()
    in
    (* Wrap to guarantee unsubscribe on client disconnect or cancellation. *)
    fun writer ->
      Fun.protect
        (fun () -> base writer)
        ~finally:(fun () ->
          Sse.Broadcaster.unsubscribe s.broadcaster client_id)

(* ── DELETE handler ──────────────────────────── *)

let handle_delete s request : Cohttp_eio.Server.response =
  match validate_session_or_error s.session request with
  | Error resp -> resp
  | Ok () ->
    Sse.Broadcaster.shutdown s.broadcaster;
    Http_session.close s.session;
    respond_empty ~status:`OK s.session

(* ── OPTIONS handler ─────────────────────────── *)

let handle_options session : Cohttp_eio.Server.response =
  respond_empty ~status:`OK session

(* ── main callback ───────────────────────────── *)

(* ── well-known endpoint (RFC 9728) ─────────── *)

let handle_well_known_resource s : Cohttp_eio.Server.response =
  match s.auth with
  | None ->
    Cohttp_eio.Server.respond_string
      ~status:`Not_found ~body:"OAuth not configured" ()
  | Some auth_config ->
    let metadata = Auth_middleware.resource_metadata auth_config in
    let json = Auth.protected_resource_metadata_to_yojson metadata in
    let headers = Http.Header.of_list
      (("Content-Type", json_content_type) :: cors_headers) in
    Cohttp_eio.Server.respond_string
      ~headers ~status:`OK
      ~body:(Yojson.Safe.to_string json) ()

(* ── auth check helper ──────────────────────── *)

let check_auth_if_configured s request =
  match s.auth with
  | None -> Ok ()
  | Some auth_config ->
    match Auth_middleware.check_auth ~required:true auth_config request with
    | Ok _ -> Ok ()
    | Error resp -> Error resp

let callback s ?(prefix="/mcp") _conn request body =
  let path = Http.Request.resource request in
  let meth = Http.Request.meth request in
  (* RFC 9728: Protected Resource Metadata *)
  if path = "/.well-known/oauth-protected-resource" && meth = `GET then
    handle_well_known_resource s
  else if path <> prefix && not (String.length path > String.length prefix
     && String.sub path 0 (String.length prefix) = prefix) then
    Cohttp_eio.Server.respond_string
      ~status:`Not_found ~body:"Not found" ()
  else
    (* MCP 2025-11-25: DNS rebinding protection. *)
    match validate_origin request with
    | Error msg ->
      respond_json ~status:`Forbidden
        (`Assoc ["error", `String msg])
    | Ok () ->
    (* MCP 2025-11-25: Mcp-Protocol-Version header validation for Streamable HTTP.
       The header is required on POST (except initialize) and GET requests. *)
    let version_ok =
      match Http.Header.get (Http.Request.headers request) "mcp-protocol-version" with
      | None -> true  (* absent = allowed for backward compat and initialize *)
      | Some v -> Version.is_supported v
    in
    if not version_ok then
      Cohttp_eio.Server.respond_string
        ~status:`Not_acceptable
        ~headers:(Http.Header.of_list cors_headers)
        ~body:(Printf.sprintf {|{"error":"Unsupported MCP protocol version. Supported: %s"}|}
          Version.latest) ()
    else
    match meth with
    | `POST ->
      (* H5 fix: Parse JSON once here, pass the parsed result to handle_post
         to avoid double-parsing (previously parsed for is_init check in
         callback AND again in handle_post). *)
      begin match
        Eio.Buf_read.of_flow ~max_size:(10 * 1024 * 1024) body
        |> Eio.Buf_read.take_all
      with
      | body_str ->
        let json_result =
          (try Ok (Yojson.Safe.from_string body_str)
           with Yojson.Json_error msg -> Error msg)
        in
        let is_init = match json_result with
          | Ok json -> is_initialize_request json
          | Error _ -> false
        in
        if is_init then
          handle_post_parsed s request json_result
        else begin
          match check_auth_if_configured s request with
          | Error resp -> resp
          | Ok () -> handle_post_parsed s request json_result
        end
      | exception Eio.Buf_read.Buffer_limit_exceeded ->
        respond_json ~status:`Request_entity_too_large
          (`Assoc ["error", `String "Request body too large (max 10MB)"])
      end
    | `GET ->
      begin match check_auth_if_configured s request with
      | Error resp -> resp
      | Ok () -> handle_get s request
      end
    | `DELETE ->
      begin match check_auth_if_configured s request with
      | Error resp -> resp
      | Ok () -> handle_delete s request
      end
    | `OPTIONS ->
      handle_options s.session
    | _ ->
      Cohttp_eio.Server.respond_string
        ~status:`Method_not_allowed
        ~body:"Method not allowed" ()

(* ── standalone run ──────────────────────────── *)

let run s ~sw ~env:(env : Eio_unix.Stdenv.base) ?(port=8080) ?(prefix="/mcp") ?stop () =
  let clock = Eio.Stdenv.clock env in
  s.sleep <- Some (Eio.Time.sleep clock);
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
