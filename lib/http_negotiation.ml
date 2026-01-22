(** HTTP Content Negotiation for MCP.

    MCP supports multiple transport modes via HTTP:
    - Streamable HTTP (recommended): Application/JSON with optional SSE
    - SSE-only (legacy): Server-Sent Events
    - HTTP POST (stateless): Standard request/response

    This module handles Accept header parsing and content type negotiation.
*)

(** {2 Content Types} *)

(** Standard content types used in MCP *)
let json_content_type = "application/json"
let sse_content_type = "text/event-stream"
let mcp_sse_content_type = "text/event-stream; charset=utf-8"

(** {2 Accept Header Parsing} *)

(** Media type with optional quality parameter *)
type media_type = {
  type_: string;
  subtype: string;
  quality: float;
  params: (string * string) list;
}

(** Parse a single media type from Accept header *)
let parse_media_type s =
  let s = String.trim s in
  let parts = String.split_on_char ';' s in
  match parts with
  | [] -> None
  | type_part :: rest ->
    let type_parts = String.split_on_char '/' (String.trim type_part) in
    match type_parts with
    | [type_; subtype] ->
      let quality = ref 1.0 in
      let params = List.filter_map (fun param ->
        let param = String.trim param in
        match String.split_on_char '=' param with
        | [k; v] ->
          let k = String.trim k in
          let v = String.trim v in
          if k = "q" then begin
            quality := (try float_of_string v with _ -> 1.0);
            None
          end else
            Some (k, v)
        | _ -> None
      ) rest in
      Some { type_; subtype; quality = !quality; params }
    | _ -> None

(** Parse Accept header into sorted list of media types *)
let parse_accept_header header =
  let parts = String.split_on_char ',' header in
  let media_types = List.filter_map parse_media_type parts in
  (* Sort by quality descending *)
  List.sort (fun a b -> compare b.quality a.quality) media_types

(** Check if media type matches a pattern (supports wildcards) *)
let media_type_matches ~pattern ~actual =
  let pattern_parts = String.split_on_char '/' pattern in
  match pattern_parts with
  | [ptype; psubtype] ->
    (ptype = "*" || ptype = actual.type_) &&
    (psubtype = "*" || psubtype = actual.subtype)
  | _ -> false

(** {2 Accept Header Predicates} *)

(** Check if client accepts SSE *)
let accepts_sse header =
  let media_types = parse_accept_header header in
  List.exists (fun mt ->
    mt.type_ = "text" && mt.subtype = "event-stream"
  ) media_types

(** Check if client accepts JSON *)
let accepts_json header =
  let media_types = parse_accept_header header in
  List.exists (fun mt ->
    (mt.type_ = "application" && mt.subtype = "json") ||
    (mt.type_ = "*" && mt.subtype = "*")
  ) media_types

(** Check if client accepts streamable MCP (JSON or SSE) *)
let accepts_streamable_mcp header =
  accepts_json header || accepts_sse header

(** {2 Response Content Type Selection} *)

(** Transport mode for MCP *)
type transport_mode =
  | Streamable_http (** Modern: JSON request/response with optional SSE notifications *)
  | Sse_only (** Legacy: All messages via SSE *)
  | Stateless_http (** No persistent connection *)

(** Determine best transport mode based on Accept header *)
let negotiate_transport ~accept_header =
  if accepts_sse accept_header then
    Streamable_http
  else if accepts_json accept_header then
    Stateless_http
  else
    (* Default to stateless if no recognized type *)
    Stateless_http

(** Get response content type for a transport mode *)
let content_type_for_transport = function
  | Streamable_http -> json_content_type
  | Sse_only -> sse_content_type
  | Stateless_http -> json_content_type

(** {2 SSE Formatting} *)

(** Format a message as an SSE event *)
let format_sse_event ?event ?id data =
  let buf = Buffer.create 256 in
  Option.iter (fun e -> Buffer.add_string buf (Printf.sprintf "event: %s\n" e)) event;
  Option.iter (fun i -> Buffer.add_string buf (Printf.sprintf "id: %s\n" i)) id;
  Buffer.add_string buf (Printf.sprintf "data: %s\n\n" data);
  Buffer.contents buf

(** Format a JSON value as an SSE event *)
let format_sse_json ?event ?id json =
  let data = Yojson.Safe.to_string json in
  format_sse_event ?event ?id data
