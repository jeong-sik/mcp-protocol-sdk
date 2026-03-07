(** HTTP session management for MCP Streamable HTTP transport.

    Handles session ID generation, state machine transitions,
    and header validation for the Mcp-Session-Id lifecycle. *)

(** {2 Session State} *)

(** Session lifecycle states. *)
type state =
  | Uninitialized  (** No initialize request received yet *)
  | Initializing   (** Initialize request received, response pending *)
  | Ready          (** Initialized notification received, session active *)
  | Closing        (** Shutdown requested *)
  | Closed         (** Session terminated *)

(** {2 Session} *)

type t

(** [create ()] makes a new session in [Uninitialized] state.
    No session ID is assigned until [initialize] is called. *)
val create : unit -> t

(** {2 Session ID} *)

(** [generate_session_id ()] reads 16 bytes from [/dev/urandom]
    and returns a 32-character lowercase hex string. *)
val generate_session_id : unit -> string

(** [session_id t] returns the session ID, if one has been assigned. *)
val session_id : t -> string option

(** {2 State Transitions} *)

(** [state t] returns the current session state. *)
val state : t -> state

(** [initialize t] transitions from [Uninitialized] to [Initializing]
    and generates a session ID. Returns [Error] if not in [Uninitialized]. *)
val initialize : t -> (string, string) result

(** [ready t] transitions from [Initializing] to [Ready].
    Returns [Error] if not in [Initializing]. *)
val ready : t -> (unit, string) result

(** [close t] transitions to [Closed] from any non-[Closed] state. *)
val close : t -> unit

(** {2 Validation} *)

(** [validate t header_value] checks whether the given [Mcp-Session-Id]
    header value (if any) matches the current session.
    - Before initialization: any header is accepted.
    - After initialization: the header must match the session ID.
    Returns [Ok ()] on success or [Error] with a reason. *)
val validate : t -> string option -> (unit, [`Bad_request of string | `Not_found]) result

(** {2 Event IDs} *)

(** [next_event_id t] returns a monotonically increasing string
    suitable for SSE [id:] fields and [Last-Event-ID] tracking. *)
val next_event_id : t -> string

(** {2 Constants} *)

(** The HTTP header name for MCP session identification: ["mcp-session-id"]. *)
val header_name : string
