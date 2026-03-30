(** SDK-wide default values shared across transport implementations. *)

val default_timeout : float
(** Default request timeout in seconds (60.0). Used by stdio, generic, and
    HTTP client/server implementations. *)
