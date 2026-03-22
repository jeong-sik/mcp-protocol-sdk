(** MCP HTTP Server using Streamable HTTP transport (cohttp-eio).

    Implements MCP spec 2025-03-26 Streamable HTTP endpoints:
    - POST /mcp - JSON-RPC requests
    - GET /mcp - SSE stream for server-initiated notifications
    - DELETE /mcp - Session termination
    - OPTIONS /mcp - CORS preflight

    Usage:
    {[
      Eio_main.run @@ fun env ->
      let server =
        Http_server.create ~name:"my-server" ~version:"1.0.0" ()
        |> Http_server.add_tool echo_tool (fun _ctx _name args ->
             Ok (Mcp_types.tool_result_of_text "hello"))
      in
      Http_server.run server
        ~sw ~env ~port:8080 ()
    ]}
*)

open Mcp_protocol

(** {2 Types} *)

type t

(** {2 Construction} *)

(** Create a new HTTP MCP server.
    @param auth Optional OAuth 2.1 auth middleware configuration.
    When provided, all requests (except initialize) require a valid bearer token. *)
val create : name:string -> version:string -> ?instructions:string ->
  ?auth:Auth_middleware.config -> unit -> t

(** Register a tool and its handler. *)
val add_tool : Mcp_types.tool -> Mcp_protocol_eio.Handler.tool_handler -> t -> t

(** Register a resource and its handler. *)
val add_resource : Mcp_types.resource -> Mcp_protocol_eio.Handler.resource_handler -> t -> t

(** Register a resource template and its handler. *)
val add_resource_template : Mcp_types.resource_template -> Mcp_protocol_eio.Handler.resource_handler -> t -> t

(** Register a prompt and its handler. *)
val add_prompt : Mcp_types.prompt -> Mcp_protocol_eio.Handler.prompt_handler -> t -> t

(** Register a completion handler. *)
val add_completion_handler : Mcp_protocol_eio.Handler.completion_handler -> t -> t

(** Task handler callbacks. See {!Mcp_protocol_eio.Handler.task_handlers}. *)
type task_handlers = Mcp_protocol_eio.Handler.task_handlers

(** Register task handlers for tasks/get, tasks/result, tasks/list, tasks/cancel. *)
val add_task_handlers : task_handlers -> t -> t

(** {2 Running} *)

(** [callback t ~prefix] returns a cohttp-eio server callback that routes
    MCP requests under [prefix] (default ["/mcp"]).

    Can be composed with other cohttp-eio routes. *)
val callback : t ->
  ?prefix:string ->
  Cohttp_eio.Server.conn ->
  Http.Request.t ->
  Cohttp_eio.Body.t ->
  Cohttp_eio.Server.response

(** [run t ~sw ~env ~port ()] starts a standalone HTTP server.
    Blocks until [stop] is resolved or the switch is cancelled.
    @param port TCP port to listen on (default 8080)
    @param prefix URL prefix for MCP endpoints (default "/mcp")
    @param stop Optional Eio promise to stop the server *)
val run : t ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?port:int ->
  ?prefix:string ->
  ?stop:unit Eio.Promise.t ->
  unit -> unit
