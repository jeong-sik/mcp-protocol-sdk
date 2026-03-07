(** HTTP Content Negotiation for MCP.

    Handles Accept header parsing and content type negotiation for
    MCP transport modes.
*)

(** {2 Content Types} *)

val json_content_type : string
val sse_content_type : string
val mcp_sse_content_type : string

(** {2 Accept Header Parsing} *)

type media_type = {
  type_: string;
  subtype: string;
  quality: float;
  params: (string * string) list;
}

val parse_media_type : string -> media_type option
val parse_accept_header : string -> media_type list
val media_type_matches : pattern:string -> actual:media_type -> bool

(** {2 Accept Header Predicates} *)

val accepts_sse : string -> bool
val accepts_json : string -> bool
val accepts_streamable_mcp : string -> bool

(** {2 Transport Negotiation} *)

type transport_mode =
  | Streamable_http
  | Sse_only
  | Stateless_http

val negotiate_transport : accept_header:string -> transport_mode
val content_type_for_transport : transport_mode -> string

(** {2 SSE Formatting} *)

val format_sse_event : ?event:string -> ?id:string -> string -> string
val format_sse_json : ?event:string -> ?id:string -> Yojson.Safe.t -> string

(** {2 Backward Compatibility} *)

val accepts_sse_header : string -> bool
