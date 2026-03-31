(** Transport-agnostic MCP request handler.

    Provides handler registration and dispatch logic shared by
    both stdio and HTTP transports.

    Usage:
    {[
      let h =
        Handler.create ~name:"my-server" ~version:"1.0.0" ()
        |> Handler.add_tool echo_tool (fun _ctx _name args -> Ok result)
        |> Handler.add_resource res (fun _ctx uri -> Ok [contents])
      in
      (* Use h in Server.run or Http_server.run *)
    ]}
*)

open Mcp_protocol

(** {2 Handler Context}

    Passed to every handler at runtime. Provides the ability to send
    notifications (logging, progress) back to the client during request handling. *)

type context = {
  send_notification : method_:string -> params:Yojson.Safe.t option -> (unit, string) result;
  (** Send a raw JSON-RPC notification to the client. *)
  send_log : Logging.log_level -> string -> (unit, string) result;
  (** Send a logging/message notification. Only sent if the level
      is at or above the current log level set by the client. *)
  send_progress : token:Mcp_result.progress_token -> progress:float -> message:string option -> total:float option -> (unit, string) result;
  (** Send a progress notification for a long-running operation. *)
  request_sampling : Sampling.create_message_params -> (Sampling.create_message_result, string) result;
  (** Send a [sampling/createMessage] request to the client and wait for the response. *)
  request_roots_list : unit -> (Mcp_types.root list, string) result;
  (** Send a [roots/list] request to the client and wait for the response. *)
  request_elicitation : Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result;
  (** Send an [elicitation/create] request to the client and wait for the response. *)
}

(** {2 Handler Types} *)

type tool_handler = context -> string -> Yojson.Safe.t option -> (Mcp_types.tool_result, string) result
type resource_handler = context -> string -> (Mcp_types.resource_contents list, string) result
type prompt_handler = context -> string -> (string * string) list -> (Mcp_types.prompt_result, string) result
type completion_handler =
  Mcp_types.completion_reference -> string -> string -> context:Mcp_types.completion_context option -> Mcp_types.completion_result

(** Callbacks for serving tasks/get, tasks/result, tasks/list, tasks/cancel.
    Each handler receives a {!context} for sending notifications/progress
    during task operations, consistent with other handler types. *)
type task_handlers = {
  get: context -> string -> (Mcp_types.task, string) result;
  result: context -> string -> (Yojson.Safe.t, string) result;
  list: context -> string option -> (Mcp_types.task list * string option, string) result;
  cancel: context -> string -> (Mcp_types.task, string) result;
}

(** {2 Handler Registry} *)

type t

val create :
  name:string ->
  version:string ->
  ?instructions:string ->
  ?enable_logging:bool ->
  unit ->
  t
val add_tool : Mcp_types.tool -> tool_handler -> t -> t
val add_resource : Mcp_types.resource -> resource_handler -> t -> t
val add_resource_template : Mcp_types.resource_template -> resource_handler -> t -> t
val add_prompt : Mcp_types.prompt -> prompt_handler -> t -> t

(** {2 Ergonomic Registration}

    Convenience functions that combine definition + registration in one call. *)

(** Register a tool by name. Creates the tool definition and registers the handler.
    {[
      handler
      |> Handler.tool "echo" ~description:"Echo input"
           (fun _ctx _name args ->
              let open Tool_arg in
              let* text = required args "text" string in
              Ok (Mcp_types.tool_result_of_text text))
    ]}
*)
val tool : string -> ?description:string -> ?input_schema:Yojson.Safe.t ->
  tool_handler -> t -> t

(** Register a resource by URI and name. *)
val resource : uri:string -> string -> ?description:string -> ?mime_type:string ->
  resource_handler -> t -> t

(** Register a resource template by URI template and name.
    The handler receives the resolved URI when the template is matched. *)
val resource_template : uri_template:string -> string -> ?description:string -> ?mime_type:string ->
  resource_handler -> t -> t

(** Register a prompt by name. *)
val prompt : string -> ?description:string -> ?arguments:Mcp_types.prompt_argument list ->
  prompt_handler -> t -> t
val add_completion_handler : completion_handler -> t -> t
val add_task_handlers : task_handlers -> t -> t

(** {2 Shared Client Helpers} *)

(** Parse a JSON list field from a response, applying [parser] to each element.
    Returns [Error] on the first invalid item instead of silently dropping it. *)
val parse_list_field :
  string ->
  (Yojson.Safe.t -> ('a, string) result) ->
  Yojson.Safe.t ->
  ('a list, string) result

val parse_paginated_list_field :
  string ->
  (Yojson.Safe.t -> ('a, string) result) ->
  Yojson.Safe.t ->
  ('a list * string option, string) result

(** Build the JSON params for an [initialize] request.
    Used by both stdio and HTTP clients. *)
val build_initialize_params :
  has_sampling:bool ->
  has_roots:bool ->
  has_elicitation:bool ->
  client_name:string ->
  client_version:string ->
  Yojson.Safe.t

(** {2 Accessors} *)

val name : t -> string
val version : t -> string
val instructions : t -> string option
val tools : t -> Mcp_types.tool list
val resources : t -> Mcp_types.resource list
val resource_templates : t -> Mcp_types.resource_template list
val prompts : t -> Mcp_types.prompt list

(** Return URIs currently subscribed via [resources/subscribe]. *)
val subscribed_uris : t -> string list

(** {2 Capabilities} *)

val server_capabilities : t -> Mcp_types.server_capabilities

(** {2 Dispatch}

    Process an incoming JSON-RPC message and return an optional response.
    The [log_level_ref] is updated by logging/setLevel requests. *)

val dispatch : t -> context -> Logging.log_level ref -> Jsonrpc.message -> Jsonrpc.message option
