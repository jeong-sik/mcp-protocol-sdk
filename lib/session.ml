(** Session management patterns inspired by Python SDK.

    Python SDK uses:
    - Context manager for lifecycle
    - Request-response ID mapping
    - Timeout management
    - Graceful shutdown with pending request cleanup

    Reference: github.com/modelcontextprotocol/python-sdk/src/mcp/shared/session.py

    v0.3.0: request_id aliased to Jsonrpc.request_id, session_info made immutable,
    check_timeouts uses fold instead of ref.

    v0.2.2: Refactored Request_tracker to use immutable Map instead of
    mutable Hashtbl for OCaml 5.x idiomatic patterns.
*)

(** {2 Request ID} *)

(** Request ID type — alias for [Jsonrpc.request_id]. *)
type request_id = Jsonrpc.request_id =
  | String_id of string
  | Int_id of int

let request_id_to_yojson = Jsonrpc.request_id_to_yojson

let request_id_of_yojson = Jsonrpc.request_id_of_yojson

(** {2 Request Tracking} *)

(** Request state *)
type request_state =
  | Pending
  | Completed
  | Cancelled
  | Timed_out
  | Error of string

(** Tracked request info — immutable record. State transitions produce new values. *)
type 'a tracked_request = {
  id: request_id;
  method_: string;
  sent_at: float;  (** Unix timestamp *)
  timeout: float option;  (** Seconds *)
  state: request_state;
  response: 'a option;
}

(** Comparison module for request IDs *)
module RequestId = struct
  type t = request_id

  let compare a b =
    match a, b with
    | Int_id x, Int_id y -> Int.compare x y
    | String_id x, String_id y -> String.compare x y
    | Int_id _, String_id _ -> -1
    | String_id _, Int_id _ -> 1
end

module RequestMap = Map.Make(RequestId)

(** Request tracker — immutable. Each operation returns a new tracker. *)
module Request_tracker = struct
  type 'a t = {
    requests: 'a tracked_request RequestMap.t;
    next_id: int;
  }

  let create () = {
    requests = RequestMap.empty;
    next_id = 1;
  }

  let next_id t =
    let id = Int_id t.next_id in
    let t' = { t with next_id = t.next_id + 1 } in
    (id, t')

  let track t ~id ~method_ ?timeout () =
    let req = {
      id;
      method_;
      sent_at = Unix.gettimeofday ();
      timeout;
      state = Pending;
      response = None;
    } in
    let t' = { t with requests = RequestMap.add id req t.requests } in
    (req, t')

  let complete t ~id ~response =
    match RequestMap.find_opt id t.requests with
    | Some req ->
      let req' = { req with state = Completed; response = Some response } in
      let t' = { t with requests = RequestMap.remove id t.requests } in
      (Some req', t')
    | None -> (None, t)

  let cancel t ~id ~reason =
    match RequestMap.find_opt id t.requests with
    | Some req ->
      let state = match reason with Some r -> Error r | None -> Cancelled in
      let req' = { req with state } in
      let t' = { t with requests = RequestMap.remove id t.requests } in
      (Some req', t')
    | None -> (None, t)

  let pending_count t = RequestMap.cardinal t.requests

  let cancel_all t ~reason =
    let pending = RequestMap.fold (fun _ req acc ->
      { req with state = Error reason } :: acc
    ) t.requests [] in
    let t' = { t with requests = RequestMap.empty } in
    (pending, t')

  let check_timeouts t =
    let now = Unix.gettimeofday () in
    let timed_out, requests' =
      RequestMap.fold (fun key req (expired, remaining) ->
        match req.timeout with
        | Some timeout when now -. req.sent_at > timeout ->
          ({ req with state = Timed_out } :: expired, remaining)
        | _ ->
          (expired, RequestMap.add key req remaining)
      ) t.requests ([], RequestMap.empty)
    in
    let t' = { t with requests = requests' } in
    (timed_out, t')
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

(** Transition session to Ready state. Returns updated session_info. *)
let session_ready session =
  { session with
    lifecycle = Ready;
    initialized_at = Some (Unix.gettimeofday ()) }

(** Transition session to Closed state. Returns updated session_info. *)
let session_close session =
  { session with
    lifecycle = Closed;
    closed_at = Some (Unix.gettimeofday ()) }

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
