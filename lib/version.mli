(** MCP Protocol Version handling.

    MCP uses date-based versioning (YYYY-MM-DD format).
    Servers and clients negotiate a compatible version during initialize.
*)

val supported_versions : string list
val latest : string
val default : string

val is_supported : string -> bool
val compare : string -> string -> int
val negotiate : requested:string -> string option
val is_compatible : server_version:string -> client_version:string -> bool

(** Feature flags for a given protocol version. *)
type version_features = {
  has_tools: bool;
  has_resources: bool;
  has_prompts: bool;
  has_sampling: bool;
  has_elicitation: bool;
  has_streamable_http: bool;
}

val features_of_version : string -> version_features
