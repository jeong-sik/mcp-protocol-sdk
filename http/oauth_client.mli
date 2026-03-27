(** OAuth 2.1 client for MCP (RFC 6749, RFC 7636 PKCE).

    Provides PKCE code generation, token exchange, refresh,
    and a pluggable credential store for MCP HTTP clients. *)

open Mcp_protocol

(** {2 Credential Store} *)

(** Pluggable credential persistence. *)
type credential_store = {
  load: unit -> Auth.stored_credentials option;
  save: Auth.stored_credentials -> unit;
  clear: unit -> unit;
}

(** In-memory credential store (lost on process exit). *)
val in_memory_store : unit -> credential_store

(** {2 PKCE (RFC 7636)} *)

(** Generate a PKCE code verifier and code challenge (S256).
    @return [(code_verifier, code_challenge)] *)
val generate_pkce : unit -> string * string

(** {2 Authorization URL} *)

(** Build the authorization URL for the user to visit.
    @return Full URL with query parameters for OAuth authorization. *)
val build_authorization_url :
  authorization_endpoint:string ->
  client_id:string ->
  redirect_uri:string ->
  scopes:string list ->
  state:string ->
  code_challenge:string ->
  ?resource:string ->
  unit -> string

(** {2 Token Exchange} *)

(** Exchange an authorization code for tokens.
    @param net Eio network capability for HTTP requests.
    @param sw Eio switch.
    @param token_endpoint OAuth token endpoint URL.
    @param client_id The client ID.
    @param code The authorization code received from the redirect.
    @param redirect_uri Must match the redirect_uri used in authorization.
    @param code_verifier The PKCE code verifier. *)
val exchange_code :
  net:_ Eio.Net.t -> sw:Eio.Switch.t ->
  token_endpoint:string ->
  client_id:string ->
  code:string ->
  redirect_uri:string ->
  code_verifier:string ->
  (Auth.oauth_token_response, string) result

(** Refresh an access token using a refresh token.
    @param net Eio network capability.
    @param sw Eio switch.
    @param token_endpoint OAuth token endpoint URL.
    @param client_id The client ID.
    @param refresh_token The refresh token. *)
val refresh_token :
  net:_ Eio.Net.t -> sw:Eio.Switch.t ->
  token_endpoint:string ->
  client_id:string ->
  refresh_token:string ->
  (Auth.oauth_token_response, string) result

(** {2 OAuth Discovery (RFC 8414)} *)

(** Discover OAuth authorization server metadata.
    Fetches [{issuer}/.well-known/oauth-authorization-server] and parses
    the response into {!Auth.authorization_server_metadata}. *)
val discover :
  net:_ Eio.Net.t -> sw:Eio.Switch.t ->
  issuer:string ->
  (Auth.authorization_server_metadata, string) result

(** {2 OpenID Connect Discovery (G10, MCP spec 2025-11-25)} *)

(** Discover authorization server metadata via OIDC.
    Fetches [{issuer}/.well-known/openid-configuration]. The OIDC discovery
    response is compatible with OAuth 2.0 authorization server metadata. *)
val discover_oidc :
  net:_ Eio.Net.t -> sw:Eio.Switch.t ->
  issuer:string ->
  (Auth.authorization_server_metadata, string) result

(** Discover with fallback: tries RFC 8414 first, then OIDC discovery. *)
val discover_with_fallback :
  net:_ Eio.Net.t -> sw:Eio.Switch.t ->
  issuer:string ->
  (Auth.authorization_server_metadata, string) result

(** {2 Dynamic Client Registration (RFC 7591)} *)

(** Client registration request parameters. *)
type client_registration_request = {
  client_name: string;
  redirect_uris: string list;
  grant_types: string list;
  response_types: string list;
  token_endpoint_auth_method: string;
}

(** Register a new OAuth client dynamically.
    @return [Ok client_id] on success. *)
val register_client :
  net:_ Eio.Net.t -> sw:Eio.Switch.t ->
  registration_endpoint:string ->
  request:client_registration_request ->
  (string, string) result

(** {2 Client ID Metadata Document (G9, MCP spec 2025-11-25)} *)

(** Fetch a Client ID Metadata Document from a URL.
    An alternative to Dynamic Client Registration: the client publishes
    a JSON metadata document at a well-known URL.
    Validates that the [client_id] field in the document matches [client_id_url].
    @param client_id_url The URL where the metadata document is hosted
    (must be HTTPS, loopback exception for dev). *)
val fetch_client_metadata :
  net:_ Eio.Net.t -> sw:Eio.Switch.t ->
  client_id_url:string ->
  (Auth.client_id_metadata_document, string) result

(** {2 CSRF State Parameter} *)

(** Generate a cryptographically random state parameter for CSRF protection.
    Returns a 32-character URL-safe base64 string. *)
val generate_state : unit -> string

(** Validate that the received state matches the expected state.
    Uses constant-time comparison to prevent timing attacks. *)
val validate_state : expected:string -> received:string -> bool

(** {2 Configuration} *)

(** Default maximum response body size for OAuth HTTP responses (1 MB). *)
val default_max_response_size : int

(** {2 Bearer Token Injection} *)

(** Add a Bearer Authorization header to an HTTP request.
    Returns the headers with the authorization added. *)
val inject_bearer_token : Http.Header.t -> access_token:string -> Http.Header.t
