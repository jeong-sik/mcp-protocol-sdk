(** Transport-agnostic MCP Client via functor.

    [Make(T)] produces a client that communicates over any transport
    satisfying {!Mcp_protocol.Transport.S}.

    {[
      (* Test client over memory transport *)
      module Test_client = Generic_client.Make(Memory_transport)
      let c = Test_client.create ~transport:client_t ()
      match Test_client.initialize c ~client_name:"test" ~client_version:"1.0" with
      | Ok result -> ...
    ]}
*)

module Make (T : Mcp_protocol.Transport.S) : sig
  open Mcp_protocol

  type t

  (** Create a client connected to the given transport.
      @param clock Optional Eio clock for request timeouts. *)
  val create : transport:T.t -> ?clock:_ Eio.Time.clock -> unit -> t

  (** {2 Callback Registration} *)

  type sampling_handler =
    Sampling.create_message_params -> (Sampling.create_message_result, string) result
  type roots_handler =
    unit -> (Mcp_types.root list, string) result
  type elicitation_handler =
    Mcp_types.elicitation_params -> (Mcp_types.elicitation_result, string) result
  type notification_handler =
    string -> Yojson.Safe.t option -> unit

  val on_sampling : sampling_handler -> t -> t
  val on_roots_list : roots_handler -> t -> t
  val on_elicitation : elicitation_handler -> t -> t
  val on_notification : notification_handler -> t -> t

  (** {2 Lifecycle} *)

  val initialize : t -> client_name:string -> client_version:string ->
    (Mcp_types.initialize_result, string) result
  val ping : t -> (unit, string) result

  (** {2 Tools} *)

  val list_tools : ?cursor:string -> t -> (Mcp_types.tool list, string) result
  val call_tool : t -> name:string -> ?arguments:Yojson.Safe.t -> unit ->
    (Mcp_types.tool_result, string) result

  (** {2 Resources} *)

  val list_resources : ?cursor:string -> t -> (Mcp_types.resource list, string) result
  val read_resource : t -> uri:string ->
    (Mcp_types.resource_contents list, string) result

  (** {2 Prompts} *)

  val list_prompts : ?cursor:string -> t -> (Mcp_types.prompt list, string) result
  val get_prompt : t -> name:string -> ?arguments:(string * string) list -> unit ->
    (Mcp_types.prompt_result, string) result

  (** {2 Low-level} *)

  val send_request : t -> method_:string -> ?params:Yojson.Safe.t ->
    ?timeout:float -> unit -> (Yojson.Safe.t, string) result
  val send_notification : t -> method_:string -> ?params:Yojson.Safe.t ->
    unit -> (unit, string) result

  (** {2 Cleanup} *)

  val close : t -> unit
end
