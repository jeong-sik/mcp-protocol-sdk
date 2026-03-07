(** Typed error hierarchy for mcp-protocol-sdk.

    Two error families mirroring the Rust/TypeScript SDK split:
    - [Protocol]: JSON-RPC errors that cross the wire.
    - [Sdk]: Local errors that never cross the wire. *)

(** Protocol errors: cross the wire as JSON-RPC error responses. *)
type protocol_error = {
  code: int;
  message: string;
  data: Yojson.Safe.t option;
}

(** SDK errors: local errors that never cross the wire. *)
type sdk_error =
  | Not_connected
  | Connection_closed of string
  | Request_timeout of float
  | Cancelled of string option
  | Send_failed of string
  | Parse_error of string
  | Internal of string

(** Combined error type. *)
type t =
  | Protocol of protocol_error
  | Sdk of sdk_error

val to_string : t -> string

(** {2 Constructors} *)

val protocol : code:int -> message:string -> ?data:Yojson.Safe.t -> unit -> t
val timeout : float -> t
val cancelled : ?reason:string -> unit -> t
val connection_closed : string -> t
val send_failed : string -> t
val parse_error : string -> t
val not_connected : t
val internal : string -> t

(** {2 Bridge} *)

(** Convert typed error result to string error result for backward compatibility. *)
val to_string_result : ('a, t) result -> ('a, string) result
