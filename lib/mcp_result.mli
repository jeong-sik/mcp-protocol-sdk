(** Result type patterns inspired by Go SDK.

    Phantom types and progress tracking for compile-time safety.
*)

(** {2 Marker Types} *)

type params = private Params
type result = private Result

(** {2 Tagged Message Types} *)

type ('a, 'role) tagged = {
  data: 'a;
  meta: Yojson.Safe.t option;
}

val make_params : 'a -> ('a, params) tagged
val make_result : 'a -> ('a, result) tagged
val with_meta : ('a, 'role) tagged -> Yojson.Safe.t -> ('a, 'role) tagged
val get_meta : ('a, 'role) tagged -> Yojson.Safe.t option

(** {2 Progress Token Support} *)

type progress_token =
  | String_token of string
  | Int_token of int

val progress_token_to_yojson : progress_token -> Yojson.Safe.t
val progress_token_of_yojson : Yojson.Safe.t -> (progress_token, string) Stdlib.result

type progress = {
  progress_token: progress_token;
  progress: float;
  total: float option;
  message: string option;
}

val progress_to_yojson : progress -> Yojson.Safe.t

(** {2 Cancellation Support} *)

(** Request ID — alias for [Jsonrpc.request_id]. *)
type request_id = Jsonrpc.request_id =
  | String_id of string
  | Int_id of int

val request_id_to_yojson : request_id -> Yojson.Safe.t
val request_id_of_yojson : Yojson.Safe.t -> (request_id, string) Stdlib.result

type cancel_request = {
  request_id: request_id;
  reason: string option;
}

val cancel_request_to_yojson : cancel_request -> Yojson.Safe.t
