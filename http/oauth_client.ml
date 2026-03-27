(** OAuth 2.1 client for MCP.

    Implements PKCE (RFC 7636), token exchange, refresh,
    and a pluggable credential store. *)

open Mcp_protocol

(* ── credential store ───────────────────────── *)

type credential_store = {
  load: unit -> Auth.stored_credentials option;
  save: Auth.stored_credentials -> unit;
  clear: unit -> unit;
}

(** C3 note: This store uses a plain [ref] and is safe only within a single
    Eio domain (where fibers are cooperatively scheduled and cannot preempt
    each other mid-operation). Do NOT share across multiple OCaml 5 domains. *)
let in_memory_store () =
  let state = ref None in
  {
    load = (fun () -> !state);
    save = (fun creds -> state := Some creds);
    clear = (fun () -> state := None);
  }

(* ── PKCE (RFC 7636) ───────────────────────── *)

(** URL-safe base64 encoding (no padding, +/ -> -_) *)
let base64url_encode bytes =
  let b64 = Base64.encode_exn ~pad:false bytes in
  String.map (function '+' -> '-' | '/' -> '_' | c -> c) b64

(** C4 fix: Reuse Tls_helpers.ensure_rng instead of maintaining a separate
    initialization flag. Consolidates CSPRNG init to a single location. *)
let ensure_rng_initialized = Tls_helpers.ensure_rng

(** Generate 32 cryptographically secure random bytes for a code verifier.
    Uses Mirage_crypto_rng (CSPRNG) — Random.int is NOT safe for OAuth. *)
let generate_random_bytes () =
  ensure_rng_initialized ();
  Mirage_crypto_rng.generate 32

let generate_pkce () =
  let verifier = base64url_encode (generate_random_bytes ()) in
  let challenge =
    let hash = Digestif.SHA256.digest_string verifier in
    base64url_encode (Digestif.SHA256.to_raw_string hash)
  in
  (verifier, challenge)

(* ── authorization URL ──────────────────────── *)

let build_authorization_url
    ~authorization_endpoint ~client_id ~redirect_uri
    ~scopes ~state ~code_challenge ?resource () =
  let params = [
    ("response_type", "code");
    ("client_id", client_id);
    ("redirect_uri", redirect_uri);
    ("scope", String.concat " " scopes);
    ("state", state);
    ("code_challenge", code_challenge);
    ("code_challenge_method", "S256");
  ] in
  let params = match resource with
    | Some r -> ("resource", r) :: params
    | None -> params
  in
  let query = List.map (fun (k, v) ->
    Printf.sprintf "%s=%s" (Uri.pct_encode k) (Uri.pct_encode v)
  ) params |> String.concat "&" in
  Printf.sprintf "%s?%s" authorization_endpoint query

(* ── constants ─────────────────────────────── *)

(** L1 fix: Maximum response body size for OAuth HTTP responses (1 MB).
    Extracted from 3 hardcoded occurrences. Override by passing ~max_response_size
    to individual functions if needed in the future. *)
let default_max_response_size = 1024 * 1024

(* ── HTTP helpers ───────────────────────────── *)

let post_form ~net ~sw ~url ~params =
  let body_str = List.map (fun (k, v) ->
    Printf.sprintf "%s=%s" (Uri.pct_encode k) (Uri.pct_encode v)
  ) params |> String.concat "&" in
  let uri = Uri.of_string url in
  let headers = Http.Header.of_list [
    ("Content-Type", "application/x-www-form-urlencoded");
    ("Accept", "application/json");
  ] in
  let body = Cohttp_eio.Body.of_string body_str in
  let client = Tls_helpers.make_client net in
  let resp, resp_body = Cohttp_eio.Client.post client ~sw
    ~headers ~body uri in
  let status = Http.Response.status resp in
  let body_str = Eio.Buf_read.of_flow ~max_size:default_max_response_size resp_body
    |> Eio.Buf_read.take_all in
  (status, body_str)

let parse_token_response body_str =
  match Yojson.Safe.from_string body_str with
  | exception Yojson.Json_error msg ->
    Error (Printf.sprintf "JSON parse error: %s" msg)
  | json ->
    (* C2 fix: Check for "error" key presence in JSON, not just non-empty
       string value. Previously {"error":"","access_token":""} would pass
       the error check and produce an empty access_token. *)
    let has_error_key = match json with
      | `Assoc fields -> List.assoc_opt "error" fields <> None
      | _ -> false
    in
    match Auth.oauth_error_of_yojson json with
    | Ok err when has_error_key && err.error <> "" ->
      let desc = Option.value ~default:"" err.error_description in
      Error (Printf.sprintf "OAuth error: %s - %s" err.error desc)
    | _ ->
      match Auth.oauth_token_response_of_yojson json with
      | Ok resp when resp.Auth.access_token = "" ->
        Error "OAuth error: empty access_token in response"
      | result -> result

(* ── token exchange ─────────────────────────── *)

let exchange_code ~net ~sw ~token_endpoint ~client_id
    ~code ~redirect_uri ~code_verifier =
  let params = [
    ("grant_type", "authorization_code");
    ("client_id", client_id);
    ("code", code);
    ("redirect_uri", redirect_uri);
    ("code_verifier", code_verifier);
  ] in
  let status, body = post_form ~net ~sw ~url:token_endpoint ~params in
  match status with
  | `OK -> parse_token_response body
  | _ ->
    match parse_token_response body with
    | Error e -> Error e
    | Ok _ -> Error (Printf.sprintf "Unexpected status: %s" (Http.Status.to_string status))

(* ── token refresh ──────────────────────────── *)

let refresh_token ~net ~sw ~token_endpoint ~client_id ~refresh_token:rt =
  let params = [
    ("grant_type", "refresh_token");
    ("client_id", client_id);
    ("refresh_token", rt);
  ] in
  let status, body = post_form ~net ~sw ~url:token_endpoint ~params in
  match status with
  | `OK -> parse_token_response body
  | _ ->
    match parse_token_response body with
    | Error e -> Error e
    | Ok _ -> Error (Printf.sprintf "Unexpected status: %s" (Http.Status.to_string status))

(* ── HTTP GET helper ─────────────────────────── *)

let get_json ~net ~sw ~url =
  let uri = Uri.of_string url in
  let headers = Http.Header.of_list [("Accept", "application/json")] in
  let client = Tls_helpers.make_client net in
  let resp, resp_body = Cohttp_eio.Client.get client ~sw ~headers uri in
  let status = Http.Response.status resp in
  let body_str = Eio.Buf_read.of_flow ~max_size:default_max_response_size resp_body
    |> Eio.Buf_read.take_all in
  (status, body_str)

(* ── OAuth Discovery (RFC 8414) ─────────────── *)

(** Enforce HTTPS scheme for security-sensitive OAuth endpoints.
    RFC 8414 and RFC 7591 require TLS. *)
let require_https url context =
  let uri = Uri.of_string url in
  match Uri.scheme uri with
  | Some "https" -> Ok ()
  | Some "http" when Uri.host uri = Some "localhost"
                   || Uri.host uri = Some "127.0.0.1" -> Ok () (* loopback exception for dev *)
  | Some scheme -> Error (Printf.sprintf "%s requires HTTPS, got %s://" context scheme)
  | None -> Error (Printf.sprintf "%s: missing URL scheme" context)

(** Strip trailing slash from a URL for well-known path construction. *)
let strip_trailing_slash url =
  if String.length url > 0 && url.[String.length url - 1] = '/'
  then String.sub url 0 (String.length url - 1)
  else url

(** Parse a discovery response body into authorization_server_metadata. *)
let parse_discovery_response body =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error msg ->
    Error (Printf.sprintf "Discovery JSON parse error: %s" msg)
  | json ->
    Auth.authorization_server_metadata_of_yojson json

let discover ~net ~sw ~issuer =
  match require_https issuer "OAuth Discovery" with
  | Error e -> Error e
  | Ok () ->
  let base = strip_trailing_slash issuer in
  let well_known_url = base ^ "/.well-known/oauth-authorization-server" in
  let status, body = get_json ~net ~sw ~url:well_known_url in
  match status with
  | `OK -> parse_discovery_response body
  | _ ->
    Error (Printf.sprintf "Discovery failed: HTTP %s" (Http.Status.to_string status))

(* ── OpenID Connect Discovery (G10) ───────────── *)

(** Discover authorization server metadata via OpenID Connect Discovery.
    Fetches [{issuer}/.well-known/openid-configuration] as an alternative
    to RFC 8414's [oauth-authorization-server] endpoint. The OIDC discovery
    response is largely compatible with OAuth 2.0 metadata. *)
let discover_oidc ~net ~sw ~issuer =
  match require_https issuer "OIDC Discovery" with
  | Error e -> Error e
  | Ok () ->
  let base = strip_trailing_slash issuer in
  let well_known_url = base ^ "/.well-known/openid-configuration" in
  let status, body = get_json ~net ~sw ~url:well_known_url in
  match status with
  | `OK -> parse_discovery_response body
  | _ ->
    Error (Printf.sprintf "OIDC Discovery failed: HTTP %s" (Http.Status.to_string status))

(** Discover authorization server metadata with fallback.
    Tries RFC 8414 [oauth-authorization-server] first, then falls back
    to OpenID Connect Discovery [openid-configuration]. *)
let discover_with_fallback ~net ~sw ~issuer =
  match discover ~net ~sw ~issuer with
  | Ok _ as result -> result
  | Error _ ->
    discover_oidc ~net ~sw ~issuer

(* ── Dynamic Client Registration (RFC 7591) ─── *)

type client_registration_request = {
  client_name: string;
  redirect_uris: string list;
  grant_types: string list;
  response_types: string list;
  token_endpoint_auth_method: string;
}

let register_client ~net ~sw ~registration_endpoint ~request =
  match require_https registration_endpoint "Dynamic Client Registration" with
  | Error e -> Error e
  | Ok () ->
  let json = `Assoc [
    ("client_name", `String request.client_name);
    ("redirect_uris", `List (List.map (fun s -> `String s) request.redirect_uris));
    ("grant_types", `List (List.map (fun s -> `String s) request.grant_types));
    ("response_types", `List (List.map (fun s -> `String s) request.response_types));
    ("token_endpoint_auth_method", `String request.token_endpoint_auth_method);
  ] in
  let uri = Uri.of_string registration_endpoint in
  let headers = Http.Header.of_list [
    ("Content-Type", "application/json");
    ("Accept", "application/json");
  ] in
  let body = Cohttp_eio.Body.of_string (Yojson.Safe.to_string json) in
  let client = Tls_helpers.make_client net in
  let resp, resp_body = Cohttp_eio.Client.post client ~sw ~headers ~body uri in
  let status = Http.Response.status resp in
  let body_str = Eio.Buf_read.of_flow ~max_size:default_max_response_size resp_body
    |> Eio.Buf_read.take_all in
  match status with
  | `Created | `OK ->
    (match Yojson.Safe.from_string body_str with
     | exception Yojson.Json_error msg ->
       Error (Printf.sprintf "Registration JSON parse error: %s" msg)
     | json ->
       (* Extract client_id from registration response *)
       (match json with
        | `Assoc fields ->
          (match List.assoc_opt "client_id" fields with
           | Some (`String cid) -> Ok cid
           | _ -> Error "Registration response missing client_id")
        | _ -> Error "Invalid registration response"))
  | _ ->
    Error (Printf.sprintf "Registration failed: HTTP %s - %s"
      (Http.Status.to_string status) body_str)

(* ── Client ID Metadata Document (G9, MCP spec 2025-11-25) ── *)

(** Fetch a Client ID Metadata Document from a URL.
    This is an alternative to Dynamic Client Registration (RFC 7591).
    The client publishes a JSON metadata document at a well-known URL,
    and the server fetches it to learn about the client.
    The [client_id_url] must use HTTPS (loopback exception for dev). *)
let fetch_client_metadata ~net ~sw ~client_id_url =
  match require_https client_id_url "Client ID Metadata Document" with
  | Error e -> Error e
  | Ok () ->
  let status, body = get_json ~net ~sw ~url:client_id_url in
  match status with
  | `OK ->
    (match Yojson.Safe.from_string body with
     | exception Yojson.Json_error msg ->
       Error (Printf.sprintf "Client metadata JSON parse error: %s" msg)
     | json ->
       match Auth.client_id_metadata_document_of_yojson json with
       | Error e -> Error (Printf.sprintf "Client metadata decode error: %s" e)
       | Ok doc ->
         (* Validate that client_id in the document matches the URL *)
         if doc.Auth.client_id <> client_id_url then
           Error (Printf.sprintf
             "Client ID mismatch: document says %s but fetched from %s"
             doc.Auth.client_id client_id_url)
         else
           Ok doc)
  | _ ->
    Error (Printf.sprintf "Client metadata fetch failed: HTTP %s - %s"
      (Http.Status.to_string status) body)

(* ── CSRF state parameter (L2) ─────────────── *)

(** Generate a cryptographically random state parameter for CSRF protection.
    Returns a 32-character URL-safe base64 string. *)
let generate_state () =
  ensure_rng_initialized ();
  base64url_encode (Mirage_crypto_rng.generate 24)

(** Validate that the received state matches the expected state.
    Constant-time comparison to prevent timing attacks.
    Compares over max(len_expected, len_received) to avoid leaking
    the expected string's length through early return. *)
let validate_state ~expected ~received =
  let len_e = String.length expected in
  let len_r = String.length received in
  let result = ref (len_e lxor len_r) in
  let max_len = max len_e len_r in
  for i = 0 to max_len - 1 do
    let ce = if i < len_e then Char.code expected.[i] else 0 in
    let cr = if i < len_r then Char.code received.[i] else 0 in
    result := !result lor (ce lxor cr)
  done;
  !result = 0

(* ── bearer token injection ─────────────────── *)

let inject_bearer_token headers ~access_token =
  Http.Header.add headers "Authorization" (Printf.sprintf "Bearer %s" access_token)
