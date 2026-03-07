(** MCP Client over Eio stdio transport.

    Provides a typed API for connecting to MCP servers via
    stdin/stdout flows using NDJSON (JSON-RPC 2.0).

    For subprocess-based connections, create pipes and spawn the server
    process externally, then pass the flows to {!create}:
    {[
      let r1, w1 = Eio_unix.pipe sw in  (* child stdin pipe *)
      let r2, w2 = Eio_unix.pipe sw in  (* child stdout pipe *)
      let _proc = Eio.Process.spawn ~sw mgr ~stdin:r1 ~stdout:w2 ["server"] in
      Eio.Flow.close r1;
      Eio.Flow.close w2;
      let client = Client.create ~stdin:r2 ~stdout:w1 in
      match Client.initialize client ~client_name:"test" ~client_version:"1.0" with
      | Ok result -> Printf.printf "Connected to %s\n" result.server_info.name
      | Error e -> Printf.eprintf "Init failed: %s\n" e
    ]}
*)

open Mcp_protocol

(** {2 Client} *)

type t

(** Create a client connected to the given I/O flows.
    @param stdin  Source to read server responses from.
    @param stdout Sink to write requests to the server. *)
val create : stdin:_ Eio.Flow.source -> stdout:_ Eio.Flow.sink -> t

(** {2 Callback Registration}

    Register handlers for server-initiated requests.
    The client automatically dispatches incoming requests
    during [read_response] and sends responses back. *)

(** Handler for sampling/createMessage requests from the server. *)
type sampling_handler =
  Sampling.create_message_params -> (Sampling.create_message_result, string) result

(** Handler for roots/list requests from the server. *)
type roots_handler =
  unit -> (Mcp_types.root list, string) result

(** Handler for elicitation/create requests from the server. *)
type elicitation_handler =
  Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result

(** Handler for incoming notifications from the server. *)
type notification_handler =
  string -> Yojson.Safe.t option -> unit

(** Register a sampling handler. *)
val on_sampling : sampling_handler -> t -> t

(** Register a roots/list handler. *)
val on_roots_list : roots_handler -> t -> t

(** Register an elicitation handler. *)
val on_elicitation : elicitation_handler -> t -> t

(** Register a notification handler. *)
val on_notification : notification_handler -> t -> t

(** {2 Lifecycle} *)

(** Perform the MCP initialize handshake.
    Sends an [initialize] request and, on success, a
    [notifications/initialized] notification. *)
val initialize : t -> client_name:string -> client_version:string ->
  (Mcp_types.initialize_result, string) result

(** Send a ping and wait for the server's response. *)
val ping : t -> (unit, string) result

(** {2 Tools} *)

(** List all tools available on the server. *)
val list_tools : t -> (Mcp_types.tool list, string) result

(** Call a tool by name with optional JSON arguments. *)
val call_tool : t -> name:string -> ?arguments:Yojson.Safe.t -> unit ->
  (Mcp_types.tool_result, string) result

(** {2 Resources} *)

(** List all resources available on the server. *)
val list_resources : t -> (Mcp_types.resource list, string) result

(** Read a resource by its URI. *)
val read_resource : t -> uri:string ->
  (Mcp_types.resource_contents list, string) result

(** {2 Prompts} *)

(** List all prompts available on the server. *)
val list_prompts : t -> (Mcp_types.prompt list, string) result

(** Get a prompt by name with optional string arguments. *)
val get_prompt : t -> name:string -> ?arguments:(string * string) list -> unit ->
  (Mcp_types.prompt_result, string) result

(** {2 Cleanup} *)

(** Close the transport. Does not terminate any external process. *)
val close : t -> unit
