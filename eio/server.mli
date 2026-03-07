(** MCP Server framework over Eio stdio transport.

    Provides handler registration, automatic lifecycle management
    (initialize/initialized handshake, ping/pong), and method dispatch.

    Usage:
    {[
      let server =
        Server.create ~name:"my-server" ~version:"1.0.0" ()
        |> Server.add_tool echo_tool (fun _name args -> Ok (tool_result_of_text "hello"))
        |> Server.add_resource res (fun uri -> Ok [contents])
      in
      Server.run server
        ~stdin:(Eio.Stdenv.stdin env)
        ~stdout:(Eio.Stdenv.stdout env)
    ]}
*)

open Mcp_protocol

(** {2 Handler Types} *)

(** Tool handler: receives tool name and JSON arguments, returns a tool_result or error string. *)
type tool_handler = string -> Yojson.Safe.t option -> (Mcp_types.tool_result, string) result

(** Resource handler: receives resource URI, returns resource contents or error string. *)
type resource_handler = string -> (Mcp_types.resource_contents list, string) result

(** Prompt handler: receives prompt name and argument pairs, returns prompt result or error string. *)
type prompt_handler = string -> (string * string) list -> (Mcp_types.prompt_result, string) result

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

(** Run the server's main loop over stdio.
    Handles the MCP lifecycle automatically:
    - initialize/initialized handshake with version negotiation
    - ping/pong auto-response
    - tools/list, tools/call dispatch
    - resources/list, resources/read dispatch
    - prompts/list, prompts/get dispatch
    - Graceful shutdown on EOF or shutdown request.

    Returns when the transport reaches EOF or is closed. *)
val run : t -> stdin:_ Eio.Flow.source -> stdout:_ Eio.Flow.sink -> unit
