(** OAuth 2.1 types for MCP authentication.

    Shared types for server-side bearer token validation
    and client-side OAuth 2.1 + PKCE flows. *)

(** {2 Token Types} *)

type token_info = {
  scopes: string list;
  expires_at: float;
  user_id: string option; [@default None]
  extra: Yojson.Safe.t;   [@default `Null]
}

(** {2 Protected Resource Metadata (RFC 9728)} *)

type protected_resource_metadata = {
  resource: string;
  authorization_servers: string list; [@key "authorization_servers"]
  scopes_supported: string list option; [@default None] [@key "scopes_supported"]
  bearer_methods_supported: string list option; [@default None] [@key "bearer_methods_supported"]
}
[@@deriving yojson]

(** {2 OAuth Token Response (RFC 6749 Section 5.1)} *)

type oauth_token_response = {
  access_token: string; [@key "access_token"]
  token_type: string;   [@key "token_type"]
  expires_in: int option; [@default None] [@key "expires_in"]
  refresh_token: string option; [@default None] [@key "refresh_token"]
  scope: string option; [@default None]
}
[@@deriving yojson]

(** {2 Stored Credentials} *)

type stored_credentials = {
  client_id: string; [@key "client_id"]
  token_response: oauth_token_response option; [@default None] [@key "token_response"]
  granted_scopes: string list; [@key "granted_scopes"]
}
[@@deriving yojson]

(** {2 Authorization Server Metadata (RFC 8414)} *)

type authorization_server_metadata = {
  issuer: string;
  authorization_endpoint: string; [@key "authorization_endpoint"]
  token_endpoint: string; [@key "token_endpoint"]
  registration_endpoint: string option; [@default None] [@key "registration_endpoint"]
  scopes_supported: string list option; [@default None] [@key "scopes_supported"]
  response_types_supported: string list option; [@default None] [@key "response_types_supported"]
  code_challenge_methods_supported: string list option; [@default None] [@key "code_challenge_methods_supported"]
}
[@@deriving yojson]

(** {2 Error Response (RFC 6749 Section 5.2)} *)

type oauth_error = {
  error: string;
  error_description: string option; [@default None] [@key "error_description"]
  error_uri: string option; [@default None] [@key "error_uri"]
}
[@@deriving yojson]
