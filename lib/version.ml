(** MCP Protocol Version handling.

    MCP uses date-based versioning (YYYY-MM-DD format).
    Servers and clients negotiate a compatible version during initialize.
*)

(** Supported protocol versions in chronological order *)
let supported_versions = [
  "2024-11-05"; (* Initial stable version *)
  "2025-03-26"; (* Streamable HTTP, OAuth 2.1, tool annotations, audio *)
  "2025-06-18"; (* Structured output, elicitation, resource links, title *)
  "2025-11-25"; (* Latest: sampling, tasks, icons, extensions *)
]

(** The latest supported protocol version *)
let latest = "2025-11-25"

(** Default version for backwards compatibility *)
let default = "2024-11-05"

(** Check if a version string is supported *)
let is_supported version =
  List.mem version supported_versions

(** Compare two version strings.
    Returns: negative if v1 < v2, zero if equal, positive if v1 > v2 *)
let compare v1 v2 =
  String.compare v1 v2

(** Negotiate the best compatible version.
    Given a client's requested version, returns the highest supported version
    that is <= the requested version. *)
let negotiate ~requested =
  if not (is_supported requested) then
    (* If exact match not found, find highest supported <= requested *)
    let compatible =
      List.filter (fun v -> compare v requested <= 0) supported_versions
      |> List.sort (fun a b -> compare b a) (* Sort descending *)
    in
    match compatible with
    | best :: _ -> Some best
    | [] -> None
  else
    Some requested

(** Check if version v1 is compatible with v2 (v1 can serve v2) *)
let is_compatible ~server_version ~client_version =
  compare server_version client_version >= 0

(** Get features available in a version *)
type version_features = {
  has_tools: bool;
  has_resources: bool;
  has_prompts: bool;
  has_sampling: bool;
  has_elicitation: bool;
  has_streamable_http: bool;
  has_structured_output: bool;
  has_resource_links: bool;
  has_tasks: bool;
  has_icons: bool;
  has_extensions: bool;
}

let features_of_version version =
  let base = {
    has_tools = true;
    has_resources = true;
    has_prompts = true;
    has_sampling = false;
    has_elicitation = false;
    has_streamable_http = false;
    has_structured_output = false;
    has_resource_links = false;
    has_tasks = false;
    has_icons = false;
    has_extensions = false;
  } in
  match version with
  | "2024-11-05" -> base
  | "2025-03-26" -> { base with has_streamable_http = true }
  | "2025-06-18" -> { base with has_streamable_http = true; has_elicitation = true; has_structured_output = true; has_resource_links = true }
  | "2025-11-25" -> { base with has_sampling = true; has_elicitation = true; has_streamable_http = true; has_structured_output = true; has_resource_links = true; has_tasks = true; has_icons = true; has_extensions = true }
  | _ -> base (* Unknown versions get base features only (fail-safe) *)
