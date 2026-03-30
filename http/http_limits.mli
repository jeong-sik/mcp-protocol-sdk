(** Shared HTTP buffer size limits.

    Centralizes max body sizes for HTTP request/response parsing.
    All HTTP-layer modules reference these instead of hardcoding values. *)

val default_max_response_size : int
(** Maximum response body size for general HTTP responses (10 MB). *)

val default_max_request_size : int
(** Maximum request body size for incoming HTTP requests (10 MB). *)

val oauth_max_response_size : int
(** Maximum response body size for OAuth HTTP responses (1 MB).
    Intentionally smaller than the general limit: OAuth token/discovery
    responses are small JSON payloads, so a tighter cap provides defense
    against unexpected large responses from authorization servers. *)
