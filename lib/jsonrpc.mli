(** JSON-RPC 2.0 types for MCP protocol.

    Based on JSON-RPC 2.0 specification: {{: https://www.jsonrpc.org/specification}}

    MCP uses JSON-RPC 2.0 as its wire protocol with these message types:
    - Request: expects a response
    - Notification: no response expected
    - Response: successful result
    - Error: error result
*)

(** {2 Types} *)

(** JSON-RPC message ID - can be string, integer, or null. *)
type id =
  | String of string
  | Int of int
  | Null

val id_to_yojson : id -> Yojson.Safe.t
val id_of_yojson : Yojson.Safe.t -> (id, string) result

(** JSON-RPC Request - expects a response. *)
type request = {
  jsonrpc: string;
  id: id;
  method_: string;
  params: Yojson.Safe.t option;
}

val request_to_yojson : request -> Yojson.Safe.t
val request_of_yojson : Yojson.Safe.t -> (request, string) result

(** JSON-RPC Notification - no response expected. *)
type notification = {
  jsonrpc: string;
  method_: string;
  params: Yojson.Safe.t option;
}

val notification_to_yojson : notification -> Yojson.Safe.t
val notification_of_yojson : Yojson.Safe.t -> (notification, string) result

(** Error data in JSON-RPC error response. *)
type error_data = {
  code: int;
  message: string;
  data: Yojson.Safe.t option;
}

val error_data_to_yojson : error_data -> Yojson.Safe.t
val error_data_of_yojson : Yojson.Safe.t -> (error_data, string) result

(** JSON-RPC successful response. *)
type response = {
  jsonrpc: string;
  id: id;
  result: Yojson.Safe.t;
}

val response_to_yojson : response -> Yojson.Safe.t
val response_of_yojson : Yojson.Safe.t -> (response, string) result

(** JSON-RPC error response. *)
type error_response = {
  jsonrpc: string;
  id: id;
  error: error_data;
}

val error_response_to_yojson : error_response -> Yojson.Safe.t
val error_response_of_yojson : Yojson.Safe.t -> (error_response, string) result

(** Union type for all JSON-RPC messages. *)
type message =
  | Request of request
  | Notification of notification
  | Response of response
  | Error of error_response

val message_of_yojson : Yojson.Safe.t -> (message, string) result
val message_to_yojson : message -> Yojson.Safe.t

(** Normalized inbound JSON-RPC message.
    Represents only requests and notifications, which is the common shape used
    by MCP server adapters. [id = None] means "notification". *)
type inbound = {
  jsonrpc: string;
  id: id option;
  method_: string;
  params: Yojson.Safe.t option;
}

val inbound_of_yojson : Yojson.Safe.t -> (inbound, string) result
val inbound_of_string : string -> (inbound, string) result

(** {2 Constructors} *)

val make_request : id:id -> method_:string -> ?params:Yojson.Safe.t -> unit -> message
val make_notification : method_:string -> ?params:Yojson.Safe.t -> unit -> message
val make_response : id:id -> result:Yojson.Safe.t -> message
val make_error : id:id -> code:int -> message:string -> ?data:Yojson.Safe.t -> unit -> message
val make_request_json : id:id -> method_:string -> ?params:Yojson.Safe.t -> unit -> Yojson.Safe.t
val make_notification_json : method_:string -> ?params:Yojson.Safe.t -> unit -> Yojson.Safe.t
val make_response_json : id:id -> result:Yojson.Safe.t -> Yojson.Safe.t
val make_error_json : id:id -> code:int -> message:string -> ?data:Yojson.Safe.t -> unit -> Yojson.Safe.t

(** {2 Request ID} *)

(** Non-null request identifier. Canonical definition — [Session.request_id]
    and [Mcp_result.request_id] are aliases for this type. *)
type request_id =
  | String_id of string
  | Int_id of int

val request_id_to_yojson : request_id -> Yojson.Safe.t
val request_id_of_yojson : Yojson.Safe.t -> (request_id, string) result

(** {2 Parsing} *)

val message_of_string : string -> (message, string) result
