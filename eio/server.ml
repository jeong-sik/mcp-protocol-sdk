(** MCP Server framework over Eio stdio transport.

    Delegates request handling to {!Handler} for transport-agnostic dispatch. *)

open Mcp_protocol

(* ── re-export context and handler types from Handler ── *)

type context = Handler.context = {
  send_notification : method_:string -> params:Yojson.Safe.t option -> (unit, string) result;
  send_log : Logging.log_level -> string -> (unit, string) result;
  send_progress : token:Mcp_result.progress_token -> progress:float -> total:float option -> (unit, string) result;
  request_sampling : Sampling.create_message_params -> (Sampling.create_message_result, string) result;
  request_roots_list : unit -> (Mcp_types.root list, string) result;
  request_elicitation : Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result;
}

type tool_handler = Handler.tool_handler
type resource_handler = Handler.resource_handler
type prompt_handler = Handler.prompt_handler
type completion_handler = Handler.completion_handler

(* ── server type ─────────────────────────────────────── *)

type t = {
  handler: Handler.t;
  (* Runtime state: set during run, used for send_notification *)
  mutable transport_ref: Stdio_transport.t option;
  mutable log_level: Logging.log_level;
  mutable next_request_id: int;
}

let create ~name ~version ?instructions () =
  { handler = Handler.create ~name ~version ?instructions ();
    transport_ref = None;
    log_level = Logging.Warning;
    next_request_id = 1 }

let add_tool tool handler s =
  { s with handler = Handler.add_tool tool handler s.handler }

let add_resource resource handler s =
  { s with handler = Handler.add_resource resource handler s.handler }

let add_prompt prompt handler s =
  { s with handler = Handler.add_prompt prompt handler s.handler }

let add_completion_handler handler s =
  { s with handler = Handler.add_completion_handler handler s.handler }

(* ── notification sending ────────────────────────────── *)

let send_notification_via_transport transport ~method_ ~params =
  let msg = Jsonrpc.make_notification ~method_ ?params () in
  Stdio_transport.write transport msg

let send_notification s ~method_ ~params =
  match s.transport_ref with
  | None -> Error "Server is not running (no transport)"
  | Some transport -> send_notification_via_transport transport ~method_ ~params

(* ── server request sending ──────────────────────────── *)

let default_timeout = 60.0

let server_read_response transport ?clock expected_id =
  let do_read () =
    let rec loop () =
      match Stdio_transport.read transport with
      | None -> Error "Connection closed while waiting for client response"
      | Some (Error e) -> Error (Printf.sprintf "Read error: %s" e)
      | Some (Ok msg) ->
        begin match msg with
        | Jsonrpc.Response resp when resp.id = expected_id ->
          Ok resp.result
        | Jsonrpc.Error err when err.id = expected_id ->
          Error err.error.message
        | Jsonrpc.Notification _ ->
          loop ()
        | Jsonrpc.Response _ | Jsonrpc.Error _ ->
          loop ()
        | Jsonrpc.Request _ ->
          loop ()
        end
    in
    loop ()
  in
  match clock with
  | None -> do_read ()
  | Some c ->
    try Eio.Time.with_timeout_exn c default_timeout do_read
    with Eio.Time.Timeout ->
      Error (Printf.sprintf "Server request timed out after %.1fs" default_timeout)

let server_send_request transport next_id_ref ?clock ~method_ ?params () =
  let id = Jsonrpc.Int !next_id_ref in
  next_id_ref := !next_id_ref + 1;
  let msg = Jsonrpc.make_request ~id ~method_ ?params () in
  match Stdio_transport.write transport msg with
  | Error e -> Error e
  | Ok () -> server_read_response transport ?clock id

(* ── context builder ─────────────────────────────────── *)

let make_context transport log_level_ref next_id_ref ?clock () =
  let send_notification ~method_ ~params =
    send_notification_via_transport transport ~method_ ~params
  in
  let send_log level message =
    if Logging.should_log ~min_level:!log_level_ref ~msg_level:level then
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
  let request_sampling params =
    let json = Sampling.create_message_params_to_yojson params in
    match server_send_request transport next_id_ref ?clock
            ~method_:Notifications.sampling_create_message ~params:json () with
    | Error e -> Error e
    | Ok result -> Sampling.create_message_result_of_yojson result
  in
  let request_roots_list () =
    match server_send_request transport next_id_ref ?clock
            ~method_:Notifications.roots_list () with
    | Error e -> Error e
    | Ok result ->
      begin match result with
      | `Assoc fields ->
        begin match List.assoc_opt "roots" fields with
        | Some (`List items) ->
          let roots = List.fold_left (fun acc j ->
            match acc with
            | Error _ -> acc
            | Ok lst ->
              match Mcp_types.root_of_yojson j with
              | Ok r -> Ok (lst @ [r])
              | Error e -> Error (Printf.sprintf "Failed to parse root: %s" e)
          ) (Ok []) items in
          roots
        | _ -> Error "Missing 'roots' in response"
        end
      | _ -> Error "Invalid roots/list response"
      end
  in
  let request_elicitation params =
    let json = Mcp_types.elicitation_params_to_yojson params in
    match server_send_request transport next_id_ref ?clock
            ~method_:Notifications.elicitation_create ~params:json () with
    | Error e -> Error e
    | Ok result -> Mcp_types.elicitation_result_of_yojson result
  in
  { send_notification; send_log; send_progress;
    request_sampling; request_roots_list; request_elicitation }

(* ── main loop ────────────────────────────────────────── *)

let run s ~stdin ~stdout ?clock () =
  let transport = Stdio_transport.create ~stdin ~stdout () in
  s.transport_ref <- Some transport;
  let log_level_ref = ref s.log_level in
  let next_id_ref = ref s.next_request_id in
  let ctx = make_context transport log_level_ref next_id_ref ?clock () in
  let rec loop () =
    match Stdio_transport.read transport with
    | None -> ()
    | Some (Error e) ->
      Printf.eprintf "[%s] Read error: %s\n%!" (Handler.name s.handler) e;
      loop ()
    | Some (Ok msg) ->
      begin match Handler.dispatch s.handler ctx log_level_ref msg with
      | Some response ->
        begin match Stdio_transport.write transport response with
        | Ok () -> ()
        | Error e -> Printf.eprintf "[%s] Write error: %s\n%!" (Handler.name s.handler) e
        end
      | None -> ()
      end;
      loop ()
  in
  Fun.protect (fun () -> loop ())
    ~finally:(fun () ->
      s.log_level <- !log_level_ref;
      s.next_request_id <- !next_id_ref;
      s.transport_ref <- None;
      Stdio_transport.close transport)
