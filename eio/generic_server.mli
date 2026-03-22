(** Transport-agnostic MCP Server via functor.

    [Make(T)] produces a server that runs over any transport satisfying
    {!Mcp_protocol.Transport.S}. The server handles the full MCP lifecycle:
    initialize handshake, method dispatch, notifications, and shutdown.

    {[
      (* Run over in-memory transport for testing *)
      module Test_server = Generic_server.Make(Memory_transport)

      (* Run over stdio *)
      module Stdio_server = Generic_server.Make(Stdio_transport)

      (* Compose with middleware *)
      module Logged = Middleware.Logging(Stdio_transport)
      module Debug_server = Generic_server.Make(Logged)
    ]}
*)

module Make (T : Mcp_protocol.Transport.S) : sig
  open Mcp_protocol

  (** {2 Handler Context} *)

  type context = Handler.context = {
    send_notification : method_:string -> params:Yojson.Safe.t option -> (unit, string) result;
    send_log : Logging.log_level -> string -> (unit, string) result;
    send_progress : token:Mcp_result.progress_token -> progress:float -> total:float option -> (unit, string) result;
    request_sampling : Sampling.create_message_params -> (Sampling.create_message_result, string) result;
    request_roots_list : unit -> (Mcp_types.root list, string) result;
    request_elicitation : Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result;
  }

  (** {2 Handler Types} *)

  type tool_handler = Handler.tool_handler
  type resource_handler = Handler.resource_handler
  type prompt_handler = Handler.prompt_handler
  type completion_handler = Handler.completion_handler
  type task_handlers = Handler.task_handlers

  (** {2 Server} *)

  type t

  val create : name:string -> version:string -> ?instructions:string -> unit -> t
  val add_tool : Mcp_types.tool -> tool_handler -> t -> t
  val add_resource : Mcp_types.resource -> resource_handler -> t -> t
  val add_prompt : Mcp_types.prompt -> prompt_handler -> t -> t
  val add_completion_handler : completion_handler -> t -> t
  val add_task_handlers : task_handlers -> t -> t

  (** {2 Ergonomic Registration} *)

  val tool : string -> ?description:string -> ?input_schema:Yojson.Safe.t ->
    tool_handler -> t -> t
  val resource : uri:string -> string -> ?description:string -> ?mime_type:string ->
    resource_handler -> t -> t
  val prompt : string -> ?description:string -> ?arguments:Mcp_types.prompt_argument list ->
    prompt_handler -> t -> t

  val send_notification : t -> method_:string -> params:Yojson.Safe.t option -> (unit, string) result

  (** Run the server's main loop over the given transport.
      Handles the MCP lifecycle automatically. Returns on EOF or close. *)
  val run : t -> transport:T.t -> ?clock:_ Eio.Time.clock -> unit -> unit
end
