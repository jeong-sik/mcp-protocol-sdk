(** Shared HTTP buffer size limits.

    Centralizes max body sizes for HTTP request/response parsing.
    All HTTP-layer modules reference these instead of hardcoding values. *)

(** Maximum response body size for general HTTP responses (10 MB). *)
let default_max_response_size = 10 * 1024 * 1024

(** Maximum request body size for incoming HTTP requests (10 MB). *)
let default_max_request_size = 10 * 1024 * 1024

(** Maximum response body size for OAuth HTTP responses (1 MB).
    Intentionally smaller than the general limit: OAuth token/discovery
    responses are small JSON payloads and a 1 MB cap provides defense
    against unexpected large responses from authorization servers. *)
let oauth_max_response_size = 1024 * 1024
