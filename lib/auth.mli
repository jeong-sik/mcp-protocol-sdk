(** OAuth 2.1 types for MCP authentication (RFC 6749, RFC 7636, RFC 8707, RFC 9728).

    Provides shared types used by both server-side auth middleware
    and client-side OAuth flows. *)

(** {2 Token Types} *)

(** Information extracted from a validated bearer token. *)
type token_info = {
  scopes: string list;
  expires_at: float;
  user_id: string option;
  extra: Yojson.Safe.t;
}

(** {2 Protected Resource Metadata (RFC 9728)} *)

(** Metadata for a protected resource, served at
    [/.well-known/oauth-protected-resource]. *)
type protected_resource_metadata = {
  resource: string;
  authorization_servers: string list;
  scopes_supported: string list option;
  bearer_methods_supported: string list option;
}

val protected_resource_metadata_to_yojson : protected_resource_metadata -> Yojson.Safe.t
val protected_resource_metadata_of_yojson : Yojson.Safe.t -> (protected_resource_metadata, string) result

(** {2 OAuth Token Response} *)

(** Response from the token endpoint (RFC 6749 Section 5.1). *)
type oauth_token_response = {
  access_token: string;
  token_type: string;
  expires_in: int option;
  refresh_token: string option;
  scope: string option;
}

val oauth_token_response_to_yojson : oauth_token_response -> Yojson.Safe.t
val oauth_token_response_of_yojson : Yojson.Safe.t -> (oauth_token_response, string) result

(** {2 Stored Credentials} *)

(** Persisted client credentials and tokens. *)
type stored_credentials = {
  client_id: string;
  token_response: oauth_token_response option;
  granted_scopes: string list;
}

val stored_credentials_to_yojson : stored_credentials -> Yojson.Safe.t
val stored_credentials_of_yojson : Yojson.Safe.t -> (stored_credentials, string) result

(** {2 Authorization Server Metadata (RFC 8414)} *)

(** Subset of OAuth Authorization Server Metadata relevant to MCP. *)
type authorization_server_metadata = {
  issuer: string;
  authorization_endpoint: string;
  token_endpoint: string;
  registration_endpoint: string option;
  scopes_supported: string list option;
  response_types_supported: string list option;
  code_challenge_methods_supported: string list option;
}

val authorization_server_metadata_to_yojson : authorization_server_metadata -> Yojson.Safe.t
val authorization_server_metadata_of_yojson : Yojson.Safe.t -> (authorization_server_metadata, string) result

(** {2 Error Response} *)

(** OAuth error response (RFC 6749 Section 5.2). *)
type oauth_error = {
  error: string;
  error_description: string option;
  error_uri: string option;
}

val oauth_error_to_yojson : oauth_error -> Yojson.Safe.t
val oauth_error_of_yojson : Yojson.Safe.t -> (oauth_error, string) result
