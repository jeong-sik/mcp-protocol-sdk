(** Server-side OAuth 2.1 bearer token authentication middleware.

    Extracts bearer tokens from the Authorization header, delegates validation
    to a user-provided verifier, and returns 401 with WWW-Authenticate on failure
    (RFC 6750 Section 3). *)

(** Token verifier: given a bearer token string and the HTTP request,
    return either [token_info] or an error string. *)
type token_verifier =
  string -> Http.Request.t -> (Mcp_protocol.Auth.token_info, string) result

(** Auth middleware configuration. *)
type config = {
  verifier: token_verifier;
  required_scopes: string list;
  resource_server: string;
  authorization_servers: string list;
}

(** Create a middleware configuration. *)
val create :
  verifier:token_verifier ->
  ?required_scopes:string list ->
  resource_server:string ->
  authorization_servers:string list ->
  unit -> config

(** Check authorization on an incoming request.

    Returns [Ok (Some token_info)] if a valid token is present,
    [Ok None] if no Authorization header is present (for optional auth),
    or [Error response] with a 401 response if the token is invalid.

    When [required:true] (default), a missing token also returns 401. *)
val check_auth :
  ?required:bool ->
  config ->
  Http.Request.t ->
  (Mcp_protocol.Auth.token_info option, Cohttp_eio.Server.response) result

(** Extract the Bearer token string from an Authorization header value.
    Returns [None] if the header is missing or not a Bearer token. *)
val extract_bearer_token : Http.Request.t -> string option

(** Check that all [required] scopes are present in [granted].
    Uses StringSet for O(n + m) performance. *)
val check_scopes : required:string list -> granted:string list -> bool

(** Return the list of required scopes not present in granted.
    Useful for incremental consent to tell the client exactly which
    scopes to request. *)
val find_missing_scopes : required:string list -> granted:string list -> string list

(** Respond with 403 Forbidden when a valid token lacks required scopes.
    Per MCP spec 2025-11-25 (incremental consent), the server returns 403
    with a WWW-Authenticate header listing the missing scopes, prompting
    the client to request additional consent.
    @param missing_scopes The scopes the token is lacking. *)
val respond_403_insufficient_scope :
  config ->
  missing_scopes:string list ->
  Cohttp_eio.Server.response

(** Escape double quotes and backslashes for RFC 7230 quoted-string.
    Also strips control characters (0x00-0x1F) and DEL (0x7F). *)
val escape_quoted_string : string -> string

(** Build the Protected Resource Metadata for this server's configuration. *)
val resource_metadata : config -> Mcp_protocol.Auth.protected_resource_metadata
