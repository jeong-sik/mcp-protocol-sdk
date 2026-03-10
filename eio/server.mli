(** MCP Server framework over Eio stdio transport.

    Provides handler registration, automatic lifecycle management
    (initialize/initialized handshake, ping/pong), and method dispatch.

    Usage:
    {[
      let server =
        Server.create ~name:"my-server" ~version:"1.0.0" ()
        |> Server.add_tool echo_tool (fun _ctx _name args -> Ok (tool_result_of_text "hello"))
        |> Server.add_resource res (fun _ctx uri -> Ok [contents])
      in
      Server.run server
        ~stdin:(Eio.Stdenv.stdin env)
        ~stdout:(Eio.Stdenv.stdout env)
    ]}
*)

open Mcp_protocol

(** {2 Handler Context}

    Passed to every handler at runtime. Provides the ability to send
    notifications (logging, progress) back to the client during request handling. *)

type context = Handler.context = {
  send_notification : method_:string -> params:Yojson.Safe.t option -> (unit, string) result;
  (** Send a raw JSON-RPC notification to the client. *)
  send_log : Logging.log_level -> string -> (unit, string) result;
  (** Send a logging/message notification. Only sent if the level
      is at or above the current log level set by the client. *)
  send_progress : token:Mcp_result.progress_token -> progress:float -> total:float option -> (unit, string) result;
  (** Send a progress notification for a long-running operation. *)
  request_sampling : Sampling.create_message_params -> (Sampling.create_message_result, string) result;
  (** Send a [sampling/createMessage] request to the client and wait for the response.
      Returns [Ok result] on success or [Error reason] on failure. *)
  request_roots_list : unit -> (Mcp_types.root list, string) result;
  (** Send a [roots/list] request to the client and wait for the response.
      Returns [Ok roots] on success or [Error reason] on failure. *)
  request_elicitation : Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result;
  (** Send an [elicitation/create] request to the client and wait for the response.
      Returns [Ok result] on success or [Error reason] on failure. *)
}

(** {2 Handler Types} *)

(** Tool handler: receives context, tool name and JSON arguments, returns a tool_result or error string. *)
type tool_handler = context -> string -> Yojson.Safe.t option -> (Mcp_types.tool_result, string) result

(** Resource handler: receives context and resource URI, returns resource contents or error string. *)
type resource_handler = context -> string -> (Mcp_types.resource_contents list, string) result

(** Prompt handler: receives context, prompt name and argument pairs, returns prompt result or error string. *)
type prompt_handler = context -> string -> (string * string) list -> (Mcp_types.prompt_result, string) result

(** Completion handler: receives reference, argument name, partial value, returns completion result. *)
type completion_handler =
  Mcp_types.completion_reference -> string -> string -> Mcp_types.completion_result

(** {2 Server} *)

type t

(** Create a new server with the given name and version.
    @param instructions Optional server instructions for clients. *)
val create : name:string -> version:string -> ?instructions:string -> unit -> t

(** Register a tool and its handler. *)
val add_tool : Mcp_types.tool -> tool_handler -> t -> t

(** Register a resource and its handler. *)
val add_resource : Mcp_types.resource -> resource_handler -> t -> t

(** Register a prompt and its handler. *)
val add_prompt : Mcp_types.prompt -> prompt_handler -> t -> t

(** Register a completion handler. Only one handler is supported;
    registering a new one replaces the previous. *)
val add_completion_handler : completion_handler -> t -> t

(** Task handlers for serving task lifecycle methods.
    See {!Handler.task_handlers} for field details. *)
type task_handlers = Handler.task_handlers

(** Register task handlers for tasks/get, tasks/result, tasks/list, tasks/cancel. *)
val add_task_handlers : task_handlers -> t -> t

(** Send a notification to the connected client.
    Only usable after [run] has been called (returns [Error] otherwise). *)
val send_notification : t -> method_:string -> params:Yojson.Safe.t option -> (unit, string) result

(** Run the server's main loop over stdio.
    Handles the MCP lifecycle automatically:
    - initialize/initialized handshake with version negotiation
    - ping/pong auto-response
    - tools/list, tools/call dispatch
    - resources/list, resources/read dispatch
    - prompts/list, prompts/get dispatch
    - logging/setLevel handling
    - completion/complete dispatch
    - Graceful shutdown on EOF or shutdown request.

    Returns when the transport reaches EOF or is closed. *)
val run : t -> stdin:_ Eio.Flow.source -> stdout:_ Eio.Flow.sink -> ?clock:_ Eio.Time.clock -> unit -> unit
