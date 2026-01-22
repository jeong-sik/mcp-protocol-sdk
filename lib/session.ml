(** Session management patterns inspired by Python SDK.

    Python SDK uses:
    - Context manager for lifecycle
    - Request-response ID mapping
    - Timeout management
    - Graceful shutdown with pending request cleanup

    Reference: github.com/modelcontextprotocol/python-sdk/src/mcp/shared/session.py
*)

(** {2 Request ID} *)

(** Request ID type (same as Jsonrpc.id, defined here to avoid circular dependency) *)
type request_id =
  | String_id of string
  | Int_id of int

let request_id_to_yojson = function
  | String_id s -> `String s
  | Int_id i -> `Int i

let request_id_of_yojson = function
  | `String s -> Ok (String_id s)
  | `Int i -> Ok (Int_id i)
  | _ -> Error "Invalid request id"

(** {2 Request Tracking} *)

(** Request state *)
type request_state =
  | Pending
  | Completed
  | Cancelled
  | Timed_out
  | Error of string

(** Tracked request info *)
type 'a tracked_request = {
  id: request_id;
  method_: string;
  sent_at: float;  (** Unix timestamp *)
  timeout: float option;  (** Seconds *)
  mutable state: request_state;
  mutable response: 'a option;
}

(** Request tracker (in-memory) *)
module Request_tracker = struct
  type 'a t = {
    mutable requests: (request_id, 'a tracked_request) Hashtbl.t;
    mutable next_id: int;
  }

  let create () = {
    requests = Hashtbl.create 16;
    next_id = 1;
  }

  let next_id t =
    let id = t.next_id in
    t.next_id <- id + 1;
    Int_id id

  let track t ~id ~method_ ?timeout () =
    let req = {
      id;
      method_;
      sent_at = Unix.gettimeofday ();
      timeout;
      state = Pending;
      response = None;
    } in
    Hashtbl.add t.requests id req;
    req

  let complete t ~id ~response =
    match Hashtbl.find_opt t.requests id with
    | Some req ->
      req.state <- Completed;
      req.response <- Some response;
      Hashtbl.remove t.requests id;
      Some req
    | None -> None

  let cancel t ~id ~reason =
    match Hashtbl.find_opt t.requests id with
    | Some req ->
      req.state <- (match reason with Some r -> Error r | None -> Cancelled);
      Hashtbl.remove t.requests id;
      true
    | None -> false

  let pending_count t = Hashtbl.length t.requests

  let cancel_all t ~reason =
    let pending = Hashtbl.fold (fun _ req acc -> req :: acc) t.requests [] in
    Hashtbl.clear t.requests;
    List.iter (fun req -> req.state <- Error reason) pending;
    pending

  let check_timeouts t =
    let now = Unix.gettimeofday () in
    let timed_out = ref [] in
    Hashtbl.filter_map_inplace (fun _ req ->
      match req.timeout with
      | Some timeout when now -. req.sent_at > timeout ->
        req.state <- Timed_out;
        timed_out := req :: !timed_out;
        None
      | _ -> Some req
    ) t.requests;
    !timed_out
end

(** {2 Session State} *)

(** Session lifecycle state *)
type lifecycle =
  | Created
  | Initializing
  | Ready
  | Closing
  | Closed

let lifecycle_to_string = function
  | Created -> "created"
  | Initializing -> "initializing"
  | Ready -> "ready"
  | Closing -> "closing"
  | Closed -> "closed"

(** Session info *)
type session_info = {
  id: string;
  mutable lifecycle: lifecycle;
  created_at: float;
  mutable initialized_at: float option;
  mutable closed_at: float option;
  protocol_version: string;
  server_info: Mcp_types.server_info option;
  client_info: Mcp_types.client_info option;
}

let create_session ~id ~protocol_version ?server_info ?client_info () = {
  id;
  lifecycle = Created;
  created_at = Unix.gettimeofday ();
  initialized_at = None;
  closed_at = None;
  protocol_version;
  server_info;
  client_info;
}

let session_ready session =
  session.lifecycle <- Ready;
  session.initialized_at <- Some (Unix.gettimeofday ())

let session_close session =
  session.lifecycle <- Closed;
  session.closed_at <- Some (Unix.gettimeofday ())

(** {2 Connection Errors} *)

(** Connection-level error codes (Python SDK pattern) *)
type connection_error =
  | Connection_closed
  | Read_timeout
  | Write_timeout
  | Protocol_error of string

let connection_error_to_string = function
  | Connection_closed -> "CONNECTION_CLOSED"
  | Read_timeout -> "READ_TIMEOUT"
  | Write_timeout -> "WRITE_TIMEOUT"
  | Protocol_error msg -> Printf.sprintf "PROTOCOL_ERROR: %s" msg
