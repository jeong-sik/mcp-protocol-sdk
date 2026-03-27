(** Server-side OAuth 2.1 bearer token authentication middleware.

    Implements RFC 6750 (Bearer Token Usage) for MCP servers.
    The middleware extracts tokens from the Authorization header,
    delegates validation to a user-provided callback, and checks
    required scopes. *)

open Mcp_protocol

(* ── types ──────────────────────────────────── *)

type token_verifier = string -> Http.Request.t -> (Auth.token_info, string) result

type config = {
  verifier: token_verifier;
  required_scopes: string list;
  resource_server: string;
  authorization_servers: string list;
}

let create ~verifier ?(required_scopes=[]) ~resource_server ~authorization_servers () =
  { verifier; required_scopes; resource_server; authorization_servers }

(* ── bearer token extraction ────────────────── *)

let extract_bearer_token request =
  match Http.Header.get (Http.Request.headers request) "authorization" with
  | None -> None
  | Some value ->
    let prefix = "Bearer " in
    let prefix_len = String.length prefix in
    if String.length value > prefix_len
       && String.sub (String.lowercase_ascii value) 0 (String.length "bearer ") = "bearer " then
      Some (String.sub value prefix_len (String.length value - prefix_len))
    else
      None

(* ── WWW-Authenticate response (RFC 6750 Section 3) ── *)

(** Escape double quotes and backslashes for RFC 7230 quoted-string. *)
let escape_quoted_string s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | c when Char.code c < 0x20 || Char.code c = 0x7F -> () (* H6 fix: strip control chars including DEL *)
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let www_authenticate_header config ~error ~description =
  let parts = [
    Printf.sprintf "Bearer realm=\"%s\"" (escape_quoted_string config.resource_server);
    Printf.sprintf "error=\"%s\"" (escape_quoted_string error);
    Printf.sprintf "error_description=\"%s\"" (escape_quoted_string description);
  ] in
  let parts = if config.required_scopes <> [] then
    parts @ [Printf.sprintf "scope=\"%s\"" (String.concat " " config.required_scopes)]
  else parts
  in
  String.concat ", " parts

let respond_401 config ~error ~description =
  let www_auth = www_authenticate_header config ~error ~description in
  let headers = Http.Header.of_list [
    ("WWW-Authenticate", www_auth);
    ("Content-Type", "application/json");
  ] in
  let body = Yojson.Safe.to_string
    (Auth.oauth_error_to_yojson {
      error;
      error_description = Some description;
      error_uri = None;
    })
  in
  Cohttp_eio.Server.respond_string ~headers ~status:`Unauthorized ~body ()

(* ── incremental consent (G11, MCP spec 2025-11-25) ── *)

(** Build a WWW-Authenticate header for incremental consent (403 response).
    Includes the specific scopes the client needs to request. *)
let www_authenticate_scope_header config ~required_scopes =
  let parts = [
    Printf.sprintf "Bearer realm=\"%s\"" (escape_quoted_string config.resource_server);
    Printf.sprintf "error=\"insufficient_scope\"";
    Printf.sprintf "error_description=\"Additional scopes required\"";
    Printf.sprintf "scope=\"%s\"" (String.concat " " required_scopes);
  ] in
  String.concat ", " parts

(** Respond with 403 Forbidden when a valid token lacks required scopes.
    Per MCP spec 2025-11-25, the server returns 403 with a WWW-Authenticate
    header listing the required scopes, prompting the client to request
    additional consent from the user. *)
let respond_403_insufficient_scope config ~missing_scopes =
  let www_auth = www_authenticate_scope_header config ~required_scopes:missing_scopes in
  let headers = Http.Header.of_list [
    ("WWW-Authenticate", www_auth);
    ("Content-Type", "application/json");
  ] in
  let body = Yojson.Safe.to_string
    (Auth.oauth_error_to_yojson {
      error = "insufficient_scope";
      error_description = Some (Printf.sprintf "Missing scopes: %s"
        (String.concat ", " missing_scopes));
      error_uri = None;
    })
  in
  Cohttp_eio.Server.respond_string ~headers ~status:`Forbidden ~body ()

(* ── scope checking ─────────────────────────── *)

module StringSet = Set.Make(String)

(** L3 fix: O(n + m) via StringSet instead of O(n * m) via List.mem.
    Builds a set from granted scopes once, then checks membership. *)
let check_scopes ~required ~granted =
  let granted_set = StringSet.of_list granted in
  List.for_all (fun s -> StringSet.mem s granted_set) required

(** Return the list of required scopes not present in granted. *)
let find_missing_scopes ~required ~granted =
  let granted_set = StringSet.of_list granted in
  List.filter (fun s -> not (StringSet.mem s granted_set)) required

(* ── main check ─────────────────────────────── *)

let check_auth ?(required=true) config request =
  match extract_bearer_token request with
  | None ->
    if required then
      Error (respond_401 config
        ~error:"invalid_request"
        ~description:"Missing bearer token")
    else
      Ok None
  | Some token ->
    match config.verifier token request with
    | Error msg ->
      Error (respond_401 config ~error:"invalid_token" ~description:msg)
    | Ok token_info ->
      if token_info.Auth.expires_at < Unix.gettimeofday () then
        Error (respond_401 config
          ~error:"invalid_token"
          ~description:"Token has expired")
      else
        let missing = find_missing_scopes
          ~required:config.required_scopes
          ~granted:token_info.Auth.scopes in
        if missing <> [] then
          (* G11: Return 403 with specific missing scopes for incremental consent.
             The client can use this to request additional authorization. *)
          Error (respond_403_insufficient_scope config ~missing_scopes:missing)
        else
          Ok (Some token_info)

(* ── resource metadata ──────────────────────── *)

let resource_metadata config : Auth.protected_resource_metadata = {
  resource = config.resource_server;
  authorization_servers = config.authorization_servers;
  scopes_supported =
    if config.required_scopes = [] then None
    else Some config.required_scopes;
  bearer_methods_supported = Some ["header"];
}
