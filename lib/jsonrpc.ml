(** JSON-RPC 2.0 types for MCP protocol.

    Based on JSON-RPC 2.0 specification: https://www.jsonrpc.org/specification

    MCP uses JSON-RPC 2.0 as its wire protocol with these message types:
    - Request: expects a response
    - Notification: no response expected
    - Response: successful result
    - Error: error result
*)

(** JSON-RPC message ID - can be string or integer *)
type id =
  | String of string
  | Int of int
  | Null
[@@deriving yojson]

let id_to_yojson = function
  | String s -> `String s
  | Int i -> `Int i
  | Null -> `Null

let id_of_yojson = function
  | `String s -> Ok (String s)
  | `Int i -> Ok (Int i)
  | `Null -> Ok Null
  | _ -> Error "Invalid JSON-RPC id"

(** JSON-RPC Request - expects a response *)
type request = {
  jsonrpc: string; [@default "2.0"]
  id: id;
  method_: string; [@key "method"]
  params: Yojson.Safe.t option; [@default None]
}
[@@deriving yojson]

(** JSON-RPC Notification - no response expected *)
type notification = {
  jsonrpc: string; [@default "2.0"]
  method_: string; [@key "method"]
  params: Yojson.Safe.t option; [@default None]
}
[@@deriving yojson]

(** Error data in JSON-RPC error response *)
type error_data = {
  code: int;
  message: string;
  data: Yojson.Safe.t option; [@default None]
}
[@@deriving yojson]

(** JSON-RPC successful response *)
type response = {
  jsonrpc: string; [@default "2.0"]
  id: id;
  result: Yojson.Safe.t;
}
[@@deriving yojson]

(** JSON-RPC error response *)
type error_response = {
  jsonrpc: string; [@default "2.0"]
  id: id;
  error: error_data;
}
[@@deriving yojson]

(** Union type for all JSON-RPC messages *)
type message =
  | Request of request
  | Notification of notification
  | Response of response
  | Error of error_response

(** Parse a JSON-RPC message from JSON *)
let message_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let has_id = member "id" json <> `Null in
    let has_method = member "method" json <> `Null in
    let has_result = member "result" json <> `Null in
    let has_error = member "error" json <> `Null in
    match (has_id, has_method, has_result, has_error) with
    | (true, true, false, false) ->
      (* Request: has id and method *)
      Result.map (fun r -> Request r) (request_of_yojson json)
    | (false, true, false, false) ->
      (* Notification: has method, no id *)
      Result.map (fun n -> Notification n) (notification_of_yojson json)
    | (true, false, true, false) ->
      (* Response: has id and result *)
      Result.map (fun r -> Response r) (response_of_yojson json)
    | (true, false, false, true) ->
      (* Error: has id and error *)
      Result.map (fun e -> Error e) (error_response_of_yojson json)
    | _ ->
      Error "Invalid JSON-RPC message structure"
  with _ ->
    Error "Failed to parse JSON-RPC message"

(** Convert a JSON-RPC message to JSON *)
let message_to_yojson = function
  | Request r -> request_to_yojson r
  | Notification n -> notification_to_yojson n
  | Response r -> response_to_yojson r
  | Error e -> error_response_to_yojson e

(** Create a request message *)
let make_request ~id ~method_ ?params () =
  Request { jsonrpc = "2.0"; id; method_; params }

(** Create a notification message *)
let make_notification ~method_ ?params () =
  Notification { jsonrpc = "2.0"; method_; params }

(** Create a success response *)
let make_response ~id ~result =
  Response { jsonrpc = "2.0"; id; result }

(** Create an error response *)
let make_error ~id ~code ~message ?data () =
  Error {
    jsonrpc = "2.0";
    id;
    error = { code; message; data }
  }

(** {2 Request ID} *)

(** Non-null request identifier. Unlike [id], this cannot be [Null].
    Canonical definition — [Session.request_id] and [Mcp_result.request_id]
    are aliases for this type. *)
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

(** Parse a JSON-RPC message from a JSON string *)
let message_of_string s =
  match Yojson.Safe.from_string s with
  | json -> message_of_yojson json
  | exception Yojson.Json_error msg ->
    Error (Printf.sprintf "JSON parse error: %s" msg)
