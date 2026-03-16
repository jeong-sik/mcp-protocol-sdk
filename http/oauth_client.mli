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

(** {2 Bearer Token Injection} *)

(** Add a Bearer Authorization header to an HTTP request.
    Returns the headers with the authorization added. *)
val inject_bearer_token : Http.Header.t -> access_token:string -> Http.Header.t
