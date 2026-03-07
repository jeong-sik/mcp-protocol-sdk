(** MCP HTTP Client using Streamable HTTP transport (cohttp-eio).

    Provides a typed API for connecting to MCP servers over HTTP.
    Mirrors the stdio {!Mcp_protocol_eio.Client} API with the same
    lifecycle and callback registration patterns.

    Usage:
    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let net = Eio.Stdenv.net env in
      let client =
        Http_client.create ~endpoint:"http://localhost:8080/mcp" ~net ~sw ()
      in
      match Http_client.initialize client
              ~client_name:"my-client" ~client_version:"1.0.0" with
      | Ok result ->
        Printf.printf "Connected to %s\n" result.server_info.name
      | Error e ->
        Printf.eprintf "Init failed: %s\n" e
    ]}
*)

open Mcp_protocol

(** {2 Types} *)

type t

(** {2 Construction} *)

(** Create a new HTTP MCP client.
    @param endpoint Full URL to the MCP HTTP endpoint (e.g. "http://host:port/mcp")
    @param net Eio network capability
    @param sw Eio switch for the client lifetime *)
val create : endpoint:string -> net:_ Eio.Net.t -> sw:Eio.Switch.t -> unit -> t

(** {2 Callback Registration} *)

(** Register a handler for incoming notifications (via SSE). *)
val on_notification : (string -> Yojson.Safe.t option -> unit) -> t -> t

(** {2 Lifecycle} *)

(** Perform the MCP initialize handshake. *)
val initialize : t -> client_name:string -> client_version:string ->
  (Mcp_types.initialize_result, string) result

(** Send a ping. *)
val ping : t -> (unit, string) result

(** {2 Tools} *)

val list_tools : t -> (Mcp_types.tool list, string) result

val call_tool : t -> name:string -> ?arguments:Yojson.Safe.t -> unit ->
  (Mcp_types.tool_result, string) result

(** {2 Resources} *)

val list_resources : t -> (Mcp_types.resource list, string) result

val read_resource : t -> uri:string ->
  (Mcp_types.resource_contents list, string) result

(** {2 Prompts} *)

val list_prompts : t -> (Mcp_types.prompt list, string) result

val get_prompt : t -> name:string -> ?arguments:(string * string) list -> unit ->
  (Mcp_types.prompt_result, string) result

(** {2 Cleanup} *)

(** Close the client. Sends DELETE to terminate the server session. *)
val close : t -> (unit, string) result
