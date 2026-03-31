open Mcp_protocol

(* --- parse_media_type --- *)

let test_parse_simple () =
  match Http_negotiation.parse_media_type "application/json" with
  | Some mt ->
    Alcotest.(check string) "type" "application" mt.type_;
    Alcotest.(check string) "subtype" "json" mt.subtype;
    Alcotest.(check (float 0.001)) "quality" 1.0 mt.quality
  | None -> Alcotest.fail "Failed to parse"

let test_parse_with_quality () =
  match Http_negotiation.parse_media_type "text/event-stream;q=0.9" with
  | Some mt ->
    Alcotest.(check string) "type" "text" mt.type_;
    Alcotest.(check string) "subtype" "event-stream" mt.subtype;
    Alcotest.(check (float 0.001)) "quality" 0.9 mt.quality
  | None -> Alcotest.fail "Failed to parse"

let test_parse_case_insensitive () =
  match Http_negotiation.parse_media_type "Application/JSON;Q=0.5" with
  | Some mt ->
    Alcotest.(check string) "type normalized" "application" mt.type_;
    Alcotest.(check string) "subtype normalized" "json" mt.subtype;
    Alcotest.(check (float 0.001)) "quality parsed" 0.5 mt.quality
  | None -> Alcotest.fail "Failed to parse"

let test_parse_with_params () =
  match Http_negotiation.parse_media_type "text/event-stream; charset=utf-8" with
  | Some mt ->
    Alcotest.(check string) "type" "text" mt.type_;
    Alcotest.(check int) "params count" 1 (List.length mt.params);
    Alcotest.(check string) "charset" "utf-8" (List.assoc "charset" mt.params)
  | None -> Alcotest.fail "Failed to parse"

let test_parse_wildcard () =
  match Http_negotiation.parse_media_type "*/*" with
  | Some mt ->
    Alcotest.(check string) "type" "*" mt.type_;
    Alcotest.(check string) "subtype" "*" mt.subtype
  | None -> Alcotest.fail "Failed to parse"

let test_parse_invalid () =
  Alcotest.(check (option pass)) "no slash"
    None (Http_negotiation.parse_media_type "invalid")

(* --- parse_accept_header --- *)

let test_parse_accept_header () =
  let mts = Http_negotiation.parse_accept_header
    "text/event-stream, application/json;q=0.8" in
  Alcotest.(check int) "2 types" 2 (List.length mts);
  let first = List.hd mts in
  Alcotest.(check string) "first is SSE" "event-stream" first.subtype;
  Alcotest.(check (float 0.001)) "first quality" 1.0 first.quality

let test_parse_accept_empty () =
  let mts = Http_negotiation.parse_accept_header "" in
  Alcotest.(check int) "empty" 0 (List.length mts)

(* --- media_type_matches --- *)

let test_media_type_matches_exact () =
  match Http_negotiation.parse_media_type "application/json" with
  | Some mt ->
    Alcotest.(check bool) "exact match"
      true (Http_negotiation.media_type_matches ~pattern:"application/json" ~actual:mt)
  | None -> Alcotest.fail "parse failed"

let test_media_type_matches_wildcard () =
  match Http_negotiation.parse_media_type "application/json" with
  | Some mt ->
    Alcotest.(check bool) "wildcard match"
      true (Http_negotiation.media_type_matches ~pattern:"*/*" ~actual:mt);
    Alcotest.(check bool) "partial wildcard"
      true (Http_negotiation.media_type_matches ~pattern:"application/*" ~actual:mt)
  | None -> Alcotest.fail "parse failed"

let test_media_type_no_match () =
  match Http_negotiation.parse_media_type "text/html" with
  | Some mt ->
    Alcotest.(check bool) "no match"
      false (Http_negotiation.media_type_matches ~pattern:"application/json" ~actual:mt)
  | None -> Alcotest.fail "parse failed"

(* --- accepts_* predicates --- *)

let test_accepts_sse () =
  Alcotest.(check bool) "sse header"
    true (Http_negotiation.accepts_sse "text/event-stream, application/json");
  Alcotest.(check bool) "sse header case-insensitive"
    true (Http_negotiation.accepts_sse "Text/Event-Stream, Application/JSON");
  Alcotest.(check bool) "no sse"
    false (Http_negotiation.accepts_sse "application/json");
  Alcotest.(check bool) "sse q=0 rejected (RFC 7231)"
    false (Http_negotiation.accepts_sse "text/event-stream;q=0");
  Alcotest.(check bool) "sse q=0.0 rejected"
    false (Http_negotiation.accepts_sse "text/event-stream;q=0.0")

let test_accepts_json () =
  Alcotest.(check bool) "json header"
    true (Http_negotiation.accepts_json "application/json");
  Alcotest.(check bool) "json header case-insensitive"
    true (Http_negotiation.accepts_json "Application/JSON");
  Alcotest.(check bool) "wildcard accepts json"
    true (Http_negotiation.accepts_json "*/*");
  Alcotest.(check bool) "sse only"
    false (Http_negotiation.accepts_json "text/event-stream");
  Alcotest.(check bool) "json q=0 rejected (RFC 7231)"
    false (Http_negotiation.accepts_json "application/json;q=0");
  Alcotest.(check bool) "wildcard q=0 rejected"
    false (Http_negotiation.accepts_json "*/*;q=0")

let test_accepts_streamable_mcp () =
  Alcotest.(check bool) "json + sse"
    true
    (Http_negotiation.accepts_streamable_mcp
       "application/json, text/event-stream");
  Alcotest.(check bool) "json only"
    false (Http_negotiation.accepts_streamable_mcp "application/json");
  Alcotest.(check bool) "sse only"
    false (Http_negotiation.accepts_streamable_mcp "text/event-stream");
  Alcotest.(check bool) "html only"
    false (Http_negotiation.accepts_streamable_mcp "text/html")

(* --- negotiate_transport --- *)

let transport_testable = Alcotest.testable
  (fun fmt t -> Format.pp_print_string fmt
    (match t with
     | Http_negotiation.Streamable_http -> "Streamable_http"
     | Sse_only -> "Sse_only"
     | Stateless_http -> "Stateless_http"))
  (=)

let test_negotiate_transport_sse () =
  Alcotest.(check transport_testable) "SSE → Streamable"
    Http_negotiation.Streamable_http
    (Http_negotiation.negotiate_transport ~accept_header:"text/event-stream, application/json")

let test_negotiate_transport_json_only () =
  Alcotest.(check transport_testable) "JSON only → Stateless"
    Http_negotiation.Stateless_http
    (Http_negotiation.negotiate_transport ~accept_header:"application/json")

let test_negotiate_transport_unknown () =
  Alcotest.(check transport_testable) "unknown → Stateless"
    Http_negotiation.Stateless_http
    (Http_negotiation.negotiate_transport ~accept_header:"text/html")

(* --- content_type_for_transport --- *)

let test_content_type_for_transport () =
  Alcotest.(check string) "streamable"
    "application/json"
    (Http_negotiation.content_type_for_transport Streamable_http);
  Alcotest.(check string) "sse"
    "text/event-stream"
    (Http_negotiation.content_type_for_transport Sse_only);
  Alcotest.(check string) "stateless"
    "application/json"
    (Http_negotiation.content_type_for_transport Stateless_http)

(* --- format_sse_event --- *)

let test_format_sse_event_basic () =
  let result = Http_negotiation.format_sse_event "hello" in
  Alcotest.(check string) "basic"
    "data: hello\n\n" result

let contains s sub =
  let len_s = String.length s and len_sub = String.length sub in
  if len_sub > len_s then false
  else
    let rec loop i =
      if i > len_s - len_sub then false
      else if String.sub s i len_sub = sub then true
      else loop (i + 1)
    in
    loop 0

let test_format_sse_event_with_event_and_id () =
  let result = Http_negotiation.format_sse_event ~event:"message" ~id:"42" "payload" in
  Alcotest.(check bool) "has event field"
    true (contains result "event: message");
  Alcotest.(check bool) "has id field"
    true (contains result "id: 42");
  Alcotest.(check bool) "has data field"
    true (contains result "data: payload")

let test_format_sse_json () =
  let j = `Assoc [("key", `String "value")] in
  let result = Http_negotiation.format_sse_json j in
  Alcotest.(check bool) "contains json key"
    true (contains result "\"key\"")

(* --- backward compat alias --- *)

let test_accepts_sse_header_alias () =
  Alcotest.(check bool) "alias works"
    true (Http_negotiation.accepts_sse_header "text/event-stream")

(* --- Suite --- *)

let () =
  Alcotest.run "Http_negotiation" [
    "parse_media_type", [
      Alcotest.test_case "simple" `Quick test_parse_simple;
      Alcotest.test_case "with quality" `Quick test_parse_with_quality;
      Alcotest.test_case "case-insensitive" `Quick test_parse_case_insensitive;
      Alcotest.test_case "with params" `Quick test_parse_with_params;
      Alcotest.test_case "wildcard" `Quick test_parse_wildcard;
      Alcotest.test_case "invalid" `Quick test_parse_invalid;
    ];
    "parse_accept_header", [
      Alcotest.test_case "multi-type" `Quick test_parse_accept_header;
      Alcotest.test_case "empty" `Quick test_parse_accept_empty;
    ];
    "media_type_matches", [
      Alcotest.test_case "exact" `Quick test_media_type_matches_exact;
      Alcotest.test_case "wildcard" `Quick test_media_type_matches_wildcard;
      Alcotest.test_case "no match" `Quick test_media_type_no_match;
    ];
    "accepts_predicates", [
      Alcotest.test_case "accepts_sse" `Quick test_accepts_sse;
      Alcotest.test_case "accepts_json" `Quick test_accepts_json;
      Alcotest.test_case "accepts_streamable" `Quick test_accepts_streamable_mcp;
    ];
    "negotiate_transport", [
      Alcotest.test_case "sse" `Quick test_negotiate_transport_sse;
      Alcotest.test_case "json only" `Quick test_negotiate_transport_json_only;
      Alcotest.test_case "unknown" `Quick test_negotiate_transport_unknown;
    ];
    "content_type", [
      Alcotest.test_case "for_transport" `Quick test_content_type_for_transport;
    ];
    "format_sse", [
      Alcotest.test_case "basic" `Quick test_format_sse_event_basic;
      Alcotest.test_case "with event+id" `Quick test_format_sse_event_with_event_and_id;
      Alcotest.test_case "json" `Quick test_format_sse_json;
    ];
    "backward_compat", [
      Alcotest.test_case "accepts_sse_header" `Quick test_accepts_sse_header_alias;
    ];
  ]
