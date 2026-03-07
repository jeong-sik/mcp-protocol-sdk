(** Model Context Protocol SDK for OCaml.

    Main entry point — re-exports all submodules.
*)

(** {1 Re-exported Modules} *)

module Jsonrpc = Jsonrpc
module Mcp_types = Mcp_types
module Error_codes = Error_codes
module Mcp_error = Mcp_error
module Http_negotiation = Http_negotiation
module Version = Version
module Mcp_result = Mcp_result
module Session = Session
module Logging = Logging
module Sampling = Sampling
module Notifications = Notifications

(** {1 Convenience Re-exports} *)

val protocol_version : string
val is_version_supported : string -> bool
val make_request : id:Jsonrpc.id -> method_:string -> ?params:Yojson.Safe.t -> unit -> Jsonrpc.message
val make_notification : method_:string -> ?params:Yojson.Safe.t -> unit -> Jsonrpc.message
val make_response : id:Jsonrpc.id -> result:Yojson.Safe.t -> Jsonrpc.message
val make_error : id:Jsonrpc.id -> code:int -> message:string -> ?data:Yojson.Safe.t -> unit -> Jsonrpc.message
val make_request_json : id:Jsonrpc.id -> method_:string -> ?params:Yojson.Safe.t -> unit -> Yojson.Safe.t
val make_notification_json : method_:string -> ?params:Yojson.Safe.t -> unit -> Yojson.Safe.t
val make_response_json : id:Jsonrpc.id -> result:Yojson.Safe.t -> Yojson.Safe.t
val make_error_json : id:Jsonrpc.id -> code:int -> message:string -> ?data:Yojson.Safe.t -> unit -> Yojson.Safe.t
