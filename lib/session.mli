(** Session management patterns inspired by Python SDK.

    Request tracking, session lifecycle, and connection errors.

    v0.3.0: request_id aliased to [Jsonrpc.request_id], session_info made immutable,
    [check_timeouts] uses fold instead of ref.
*)

(** {2 Request ID} *)

(** Request ID type — alias for [Jsonrpc.request_id]. *)
type request_id = Jsonrpc.request_id =
  | String_id of string
  | Int_id of int

val request_id_to_yojson : request_id -> Yojson.Safe.t
val request_id_of_yojson : Yojson.Safe.t -> (request_id, string) result

(** {2 Request Tracking} *)

type request_state =
  | Pending
  | Completed
  | Cancelled
  | Timed_out
  | Error of string

type 'a tracked_request = {
  id: request_id;
  method_: string;
  sent_at: float;
  timeout: float option;
  state: request_state;
  response: 'a option;
}

module RequestId : sig
  type t = request_id
  val compare : t -> t -> int
end

module RequestMap : Map.S with type key = RequestId.t

module Request_tracker : sig
  type 'a t

  val create : unit -> 'a t
  val next_id : 'a t -> request_id * 'a t
  val track : 'a t -> id:request_id -> method_:string -> ?timeout:float -> unit -> 'a tracked_request * 'a t
  val complete : 'a t -> id:request_id -> response:'a -> 'a tracked_request option * 'a t
  val cancel : 'a t -> id:request_id -> reason:string option -> 'a tracked_request option * 'a t
  val pending_count : 'a t -> int
  val cancel_all : 'a t -> reason:string -> 'a tracked_request list * 'a t
  val check_timeouts : 'a t -> 'a tracked_request list * 'a t
end

(** {2 Session State} *)

type lifecycle =
  | Created
  | Initializing
  | Ready
  | Closing
  | Closed

val lifecycle_to_string : lifecycle -> string

(** Session info — immutable. State transitions return new values. *)
type session_info = {
  id: string;
  lifecycle: lifecycle;
  created_at: float;
  initialized_at: float option;
  closed_at: float option;
  protocol_version: string;
  server_info: Mcp_types.server_info option;
  client_info: Mcp_types.client_info option;
}

val create_session :
  id:string ->
  protocol_version:string ->
  ?server_info:Mcp_types.server_info ->
  ?client_info:Mcp_types.client_info ->
  unit -> session_info

(** Transition session to Ready state. Returns updated session_info. *)
val session_ready : session_info -> session_info

(** Transition session to Closed state. Returns updated session_info. *)
val session_close : session_info -> session_info

(** {2 Connection Errors} *)

type connection_error =
  | Connection_closed
  | Read_timeout
  | Write_timeout
  | Protocol_error of string

val connection_error_to_string : connection_error -> string
