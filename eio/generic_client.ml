(** Transport-agnostic MCP Client via functor.

    [Make(T)] produces a client that communicates over any transport
    satisfying {!Mcp_protocol.Transport.S}. *)

open Mcp_protocol

module Make (T : Mcp_protocol.Transport.S) = struct

  type sampling_handler =
    Sampling.create_message_params -> (Sampling.create_message_result, string) result
  type roots_handler =
    unit -> (Mcp_types.root list, string) result
  type elicitation_handler =
    Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result
  type notification_handler =
    string -> Yojson.Safe.t option -> unit

  type timeout_fn = { run : 'a. float -> (unit -> 'a) -> 'a }

  type t = {
    transport: T.t;
    mutable next_id: int;
    sampling_handler: sampling_handler option;
    roots_handler: roots_handler option;
    elicitation_handler: elicitation_handler option;
    notification_handler: notification_handler option;
    timeout_fn: timeout_fn option;
  }

  let default_timeout = 60.0

  let create ~transport ?clock () =
    let timeout_fn = match clock with
      | Some c -> Some { run = fun d f -> Eio.Time.with_timeout_exn c d f }
      | None -> None
    in
    {
      transport;
      next_id = 1;
      sampling_handler = None;
      roots_handler = None;
      elicitation_handler = None;
      notification_handler = None;
      timeout_fn;
    }

  let on_sampling handler t = { t with sampling_handler = Some handler }
  let on_roots_list handler t = { t with roots_handler = Some handler }
  let on_elicitation handler t = { t with elicitation_handler = Some handler }
  let on_notification handler t = { t with notification_handler = Some handler }

  (* ── server request dispatch ─────────────────────── *)

  let send_response t ~id ~result =
    let msg = Jsonrpc.make_response ~id ~result in
    match T.write t.transport msg with
    | Ok () -> ()
    | Error e -> Printf.eprintf "Client: failed to send response: %s\n%!" e

  let send_error_response t ~id ~code ~message =
    let msg = Jsonrpc.make_error ~id ~code ~message () in
    match T.write t.transport msg with
    | Ok () -> ()
    | Error e -> Printf.eprintf "Client: failed to send error response: %s\n%!" e

  let dispatch_server_request t (req : Jsonrpc.request) =
    match req.method_ with
    | m when m = Notifications.sampling_create_message ->
      begin match t.sampling_handler with
      | None ->
        send_error_response t ~id:req.id
          ~code:Error_codes.method_not_found
          ~message:"No sampling handler registered"
      | Some handler ->
        begin match req.params with
        | Some json ->
          begin match Sampling.create_message_params_of_yojson json with
          | Ok params ->
            begin match handler params with
            | Ok result ->
              send_response t ~id:req.id
                ~result:(Sampling.create_message_result_to_yojson result)
            | Error msg ->
              send_error_response t ~id:req.id
                ~code:Error_codes.internal_error ~message:msg
            end
          | Error msg ->
            send_error_response t ~id:req.id
              ~code:Error_codes.invalid_params ~message:msg
          end
        | None ->
          send_error_response t ~id:req.id
            ~code:Error_codes.invalid_params
            ~message:"Missing params for sampling/createMessage"
        end
      end
    | m when m = Notifications.roots_list ->
      begin match t.roots_handler with
      | None ->
        send_error_response t ~id:req.id
          ~code:Error_codes.method_not_found
          ~message:"No roots handler registered"
      | Some handler ->
        begin match handler () with
        | Ok roots ->
          let roots_json = List.map Mcp_types.root_to_yojson roots in
          send_response t ~id:req.id
            ~result:(`Assoc [("roots", `List roots_json)])
        | Error msg ->
          send_error_response t ~id:req.id
            ~code:Error_codes.internal_error ~message:msg
        end
      end
    | m when m = Notifications.elicitation_create ->
      begin match t.elicitation_handler with
      | None ->
        send_error_response t ~id:req.id
          ~code:Error_codes.method_not_found
          ~message:"No elicitation handler registered"
      | Some handler ->
        begin match req.params with
        | Some json ->
          begin match Mcp_types.elicitation_params_of_yojson json with
          | Ok params ->
            begin match handler params with
            | Ok result ->
              send_response t ~id:req.id
                ~result:(Mcp_types.elicitation_result_to_yojson result)
            | Error msg ->
              send_error_response t ~id:req.id
                ~code:Error_codes.internal_error ~message:msg
            end
          | Error msg ->
            send_error_response t ~id:req.id
              ~code:Error_codes.invalid_params ~message:msg
          end
        | None ->
          send_error_response t ~id:req.id
            ~code:Error_codes.invalid_params
            ~message:"Missing params for elicitation/create"
        end
      end
    | _ ->
      send_error_response t ~id:req.id
        ~code:Error_codes.method_not_found
        ~message:(Printf.sprintf "Unknown server request: %s" req.method_)

  (* ── request/response ─────────────────────────────── *)

  let read_response t expected_id =
    let rec loop () =
      match T.read t.transport with
      | None -> Error "Connection closed"
      | Some (Error e) -> Error (Printf.sprintf "Read error: %s" e)
      | Some (Ok msg) ->
        begin match msg with
        | Jsonrpc.Response resp when resp.id = expected_id ->
          Ok resp.result
        | Jsonrpc.Error err when err.id = expected_id ->
          Error err.error.message
        | Jsonrpc.Request req ->
          dispatch_server_request t req;
          loop ()
        | Jsonrpc.Notification notif ->
          (match t.notification_handler with
           | Some handler ->
             (try handler notif.method_ notif.params
              with
              | Out_of_memory | Stack_overflow as exn -> raise exn
              | exn ->
                Printf.eprintf "Client: notification handler raised: %s\n%!"
                  (Printexc.to_string exn))
           | None -> ());
          loop ()
        | _ -> loop ()
        end
    in
    loop ()

  let send_notification t ~method_ ?params () =
    let msg = Jsonrpc.make_notification ~method_ ?params () in
    T.write t.transport msg

  let send_request t ~method_ ?params ?(timeout = default_timeout) () =
    let id = Jsonrpc.Int t.next_id in
    t.next_id <- t.next_id + 1;
    let msg = Jsonrpc.make_request ~id ~method_ ?params () in
    match T.write t.transport msg with
    | Error e -> Error e
    | Ok () ->
      match t.timeout_fn with
      | None -> read_response t id
      | Some tf ->
        begin try
          tf.run timeout (fun () -> read_response t id)
        with Eio.Time.Timeout ->
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

  (* ── initialize ───────────────────────────────────── *)

  let initialize t ~client_name ~client_version =
    let params = Handler.build_initialize_params
      ~has_sampling:(Option.is_some t.sampling_handler)
      ~has_roots:(Option.is_some t.roots_handler)
      ~has_elicitation:(Option.is_some t.elicitation_handler)
      ~client_name ~client_version
    in
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

  let list_tools ?cursor t =
    let params = match cursor with
      | Some c -> Some (`Assoc [("cursor", `String c)])
      | None -> None
    in
    match send_request t ~method_:Notifications.tools_list ?params () with
    | Error e -> Error e
    | Ok result -> Handler.parse_list_field "tools" Mcp_types.tool_of_yojson result

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

  let list_resources ?cursor t =
    let params = match cursor with
      | Some c -> Some (`Assoc [("cursor", `String c)])
      | None -> None
    in
    match send_request t ~method_:Notifications.resources_list ?params () with
    | Error e -> Error e
    | Ok result -> Handler.parse_list_field "resources" Mcp_types.resource_of_yojson result

  let read_resource t ~uri =
    let params = `Assoc [("uri", `String uri)] in
    match send_request t ~method_:Notifications.resources_read ~params () with
    | Error e -> Error e
    | Ok result -> Handler.parse_list_field "contents" Mcp_types.resource_contents_of_yojson result

  (* ── prompts ──────────────────────────────────────── *)

  let list_prompts ?cursor t =
    let params = match cursor with
      | Some c -> Some (`Assoc [("cursor", `String c)])
      | None -> None
    in
    match send_request t ~method_:Notifications.prompts_list ?params () with
    | Error e -> Error e
    | Ok result -> Handler.parse_list_field "prompts" Mcp_types.prompt_of_yojson result

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

  let close t = T.close t.transport

end
