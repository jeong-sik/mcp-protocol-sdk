(** Result type patterns inspired by Go SDK.

    Go SDK uses marker interfaces (isResult, isParams) for compile-time verification.
    In OCaml, we use phantom types and module signatures for the same safety.

    Reference: github.com/modelcontextprotocol/go-sdk/mcp/protocol.go
*)

(** {2 Marker Types} *)

(** Phantom type for params *)
type params = private Params

(** Phantom type for results *)
type result = private Result

(** {2 Tagged Message Types} *)

(** A message tagged with its role (params or result) *)
type ('a, 'role) tagged = {
  data: 'a;
  meta: Yojson.Safe.t option; (** Extension metadata field (Go SDK pattern) *)
}

let make_params data = { data; meta = None }
let make_result data = { data; meta = None }
let with_meta tagged meta = { tagged with meta = Some meta }
let get_meta tagged = tagged.meta

(** {2 Progress Token Support} *)

(** Progress token for long-running operations *)
type progress_token =
  | String_token of string
  | Int_token of int

let progress_token_to_yojson = function
  | String_token s -> `String s
  | Int_token i -> `Int i

let progress_token_of_yojson = function
  | `String s -> Ok (String_token s)
  | `Int i -> Ok (Int_token i)
  | _ -> Error "Invalid progress token"

(** Progress notification *)
type progress = {
  progress_token: progress_token;
  progress: float;  (** 0.0 to 1.0 *)
  total: float option;
  message: string option;
}

let progress_to_yojson p =
  let fields = [
    ("progressToken", progress_token_to_yojson p.progress_token);
    ("progress", `Float p.progress);
  ] in
  let fields = match p.total with
    | Some t -> ("total", `Float t) :: fields
    | None -> fields
  in
  let fields = match p.message with
    | Some m -> ("message", `String m) :: fields
    | None -> fields
  in
  `Assoc fields

(** {2 Cancellation Support} *)

(** Request ID — alias for [Jsonrpc.request_id]. *)
type request_id = Jsonrpc.request_id =
  | String_id of string
  | Int_id of int

let request_id_to_yojson = Jsonrpc.request_id_to_yojson

let request_id_of_yojson = Jsonrpc.request_id_of_yojson

(** Cancellation request *)
type cancel_request = {
  request_id: request_id;
  reason: string option;
}

let cancel_request_to_yojson c =
  let id_json = request_id_to_yojson c.request_id in
  match c.reason with
  | Some r -> `Assoc [("requestId", id_json); ("reason", `String r)]
  | None -> `Assoc [("requestId", id_json)]
