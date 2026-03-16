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
    @param sw Eio switch for the client lifetime
    @param clock Optional Eio clock for request timeouts.
      When provided, each request times out after 60 seconds.
      On timeout a [notifications/cancelled] notification is sent to the server.
    @param access_token Optional OAuth 2.1 bearer token.
      When provided, all requests include an Authorization: Bearer header. *)
val create : endpoint:string -> net:_ Eio.Net.t -> sw:Eio.Switch.t ->
  ?clock:_ Eio.Time.clock -> ?access_token:string -> unit -> t

(** {2 Callback Registration}

    Register handlers for server-initiated requests.
    Registered handlers are advertised as client capabilities during [initialize]. *)

(** Handler for sampling/createMessage requests from the server. *)
val on_sampling :
  (Sampling.create_message_params -> (Sampling.create_message_result, string) result) ->
  t -> t

(** Handler for roots/list requests from the server. *)
val on_roots_list :
  (unit -> (Mcp_types.root list, string) result) ->
  t -> t

(** Handler for elicitation/create requests from the server. *)
val on_elicitation :
  (Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result) ->
  t -> t

(** Register a handler for incoming notifications (via SSE). *)
val on_notification : (string -> Yojson.Safe.t option -> unit) -> t -> t

(** {2 Lifecycle} *)

(** Perform the MCP initialize handshake. *)
val initialize : t -> client_name:string -> client_version:string ->
  (Mcp_types.initialize_result, string) result

(** Send a ping. *)
val ping : t -> (unit, string) result

(** {2 Tools} *)

val list_tools : ?cursor:string -> t -> (Mcp_types.tool list, string) result

val call_tool : t -> name:string -> ?arguments:Yojson.Safe.t -> unit ->
  (Mcp_types.tool_result, string) result

(** {2 Resources} *)

val list_resources : ?cursor:string -> t -> (Mcp_types.resource list, string) result

val read_resource : t -> uri:string ->
  (Mcp_types.resource_contents list, string) result

(** Subscribe to change notifications for a resource URI. *)
val subscribe_resource : t -> uri:string -> (unit, string) result

(** Unsubscribe from change notifications for a resource URI. *)
val unsubscribe_resource : t -> uri:string -> (unit, string) result

(** {2 Prompts} *)

val list_prompts : ?cursor:string -> t -> (Mcp_types.prompt list, string) result

val get_prompt : t -> name:string -> ?arguments:(string * string) list -> unit ->
  (Mcp_types.prompt_result, string) result

(** {2 Tasks (experimental, 2025-11-25)} *)

(** Get the current state of a task. *)
val get_task : t -> task_id:string -> (Mcp_types.task, string) result

(** Get the result of a completed task. Blocks until the task completes. *)
val get_task_result : t -> task_id:string -> (Yojson.Safe.t, string) result

(** List tasks on the server.
    @param cursor Optional pagination cursor from a previous response. *)
val list_tasks : ?cursor:string -> t -> (Mcp_types.task list, string) result

(** Cancel a running task. *)
val cancel_task : t -> task_id:string -> (Mcp_types.task, string) result

(** {2 Cleanup} *)

(** Close the client. Sends DELETE to terminate the server session. *)
val close : t -> (unit, string) result
