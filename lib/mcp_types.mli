(** MCP Protocol Types - Core primitives.

    Tools, Resources, Prompts, and Initialize types.
    Spec version: 2025-11-25.
*)

(** {2 Protocol Version} *)

type protocol_version =
  | V_2024_11_05
  | V_2025_03_26
  | V_2025_11_25

val protocol_version_to_string : protocol_version -> string
val protocol_version_of_string : string -> protocol_version option
val latest_version : protocol_version

(** {2 Roles} *)

type role = User | Assistant

val role_to_yojson : role -> Yojson.Safe.t
val role_of_yojson : Yojson.Safe.t -> (role, string) result

(** {2 Content Annotations} *)

type content_annotations = {
  audience: role list option;
  priority: float option;
}

val content_annotations_to_yojson : content_annotations -> Yojson.Safe.t
val content_annotations_of_yojson : Yojson.Safe.t -> (content_annotations, string) result

(** {2 Tool Annotations} *)

type tool_annotations = {
  title: string option;
  read_only_hint: bool option;
  destructive_hint: bool option;
  idempotent_hint: bool option;
  open_world_hint: bool option;
}

val tool_annotations_to_yojson : tool_annotations -> Yojson.Safe.t
val tool_annotations_of_yojson : Yojson.Safe.t -> (tool_annotations, string) result

(** {2 Tools} *)

type tool = {
  name: string;
  description: string option;
  input_schema: Yojson.Safe.t;
  title: string option;
  annotations: tool_annotations option;
}

val tool_to_yojson : tool -> Yojson.Safe.t
val tool_of_yojson : Yojson.Safe.t -> (tool, string) result

type tool_def = tool

(** {2 Tool Content} *)

type tool_content =
  | TextContent of {
      type_: string;
      text: string;
      annotations: content_annotations option;
    }
  | ImageContent of {
      type_: string;
      data: string;
      mime_type: string;
      annotations: content_annotations option;
    }
  | AudioContent of {
      type_: string;
      data: string;
      mime_type: string;
      annotations: content_annotations option;
    }
  | ResourceContent of {
      type_: string;
      resource: embedded_resource;
      annotations: content_annotations option;
    }
  | ResourceLinkContent of {
      type_: string;
      uri: string;
      name: string option;
      description: string option;
      mime_type: string option;
      annotations: content_annotations option;
    }
and embedded_resource = {
  uri: string;
  text: string option;
  blob: string option;
  mime_type: string option;
}

val embedded_resource_to_yojson : embedded_resource -> Yojson.Safe.t
val embedded_resource_of_yojson : Yojson.Safe.t -> (embedded_resource, string) result
val tool_content_to_yojson : tool_content -> Yojson.Safe.t
val tool_content_of_yojson : Yojson.Safe.t -> (tool_content, string) result
val tool_content_list_to_yojson : tool_content list -> Yojson.Safe.t
val tool_content_list_of_yojson : Yojson.Safe.t -> (tool_content list, string) result

(** {2 Tool Result} *)

type tool_result = {
  content: tool_content list;
  is_error: bool option;
  structured_content: Yojson.Safe.t option;
}

val tool_result_to_yojson : tool_result -> Yojson.Safe.t
val tool_result_of_yojson : Yojson.Safe.t -> (tool_result, string) result

(** {2 Resources} *)

type resource = {
  uri: string;
  name: string;
  description: string option;
  mime_type: string option;
}

val resource_to_yojson : resource -> Yojson.Safe.t
val resource_of_yojson : Yojson.Safe.t -> (resource, string) result

type resource_template = {
  uri_template: string;
  name: string;
  description: string option;
  mime_type: string option;
}

val resource_template_to_yojson : resource_template -> Yojson.Safe.t
val resource_template_of_yojson : Yojson.Safe.t -> (resource_template, string) result

type resource_contents = {
  uri: string;
  mime_type: string option;
  text: string option;
  blob: string option;
}

val resource_contents_to_yojson : resource_contents -> Yojson.Safe.t
val resource_contents_of_yojson : Yojson.Safe.t -> (resource_contents, string) result

(** {2 Prompts} *)

type prompt_argument = {
  name: string;
  description: string option;
  required: bool option;
}

val prompt_argument_to_yojson : prompt_argument -> Yojson.Safe.t
val prompt_argument_of_yojson : Yojson.Safe.t -> (prompt_argument, string) result

type prompt = {
  name: string;
  description: string option;
  arguments: prompt_argument list option;
}

val prompt_to_yojson : prompt -> Yojson.Safe.t
val prompt_of_yojson : Yojson.Safe.t -> (prompt, string) result

type prompt_content =
  | PromptText of { type_: string; text: string }
  | PromptImage of { type_: string; data: string; mime_type: string }
  | PromptResource of { type_: string; resource: embedded_resource }

val prompt_content_to_yojson : prompt_content -> Yojson.Safe.t
val prompt_content_of_yojson : Yojson.Safe.t -> (prompt_content, string) result

type prompt_message = {
  role: role;
  content: prompt_content;
}

val prompt_message_to_yojson : prompt_message -> Yojson.Safe.t
val prompt_message_of_yojson : Yojson.Safe.t -> (prompt_message, string) result

type prompt_result = {
  description: string option;
  messages: prompt_message list;
}

val prompt_result_to_yojson : prompt_result -> Yojson.Safe.t
val prompt_result_of_yojson : Yojson.Safe.t -> (prompt_result, string) result

(** {2 Capabilities} *)

type server_capabilities = {
  tools: Yojson.Safe.t option;
  resources: Yojson.Safe.t option;
  prompts: Yojson.Safe.t option;
  logging: Yojson.Safe.t option;
  experimental: Yojson.Safe.t option;
}

val server_capabilities_to_yojson : server_capabilities -> Yojson.Safe.t
val server_capabilities_of_yojson : Yojson.Safe.t -> (server_capabilities, string) result

type client_capabilities = {
  roots: Yojson.Safe.t option;
  sampling: Yojson.Safe.t option;
  elicitation: Yojson.Safe.t option;
  experimental: Yojson.Safe.t option;
}

val client_capabilities_to_yojson : client_capabilities -> Yojson.Safe.t
val client_capabilities_of_yojson : Yojson.Safe.t -> (client_capabilities, string) result

(** {2 Initialize} *)

type client_info = {
  name: string;
  version: string;
}

val client_info_to_yojson : client_info -> Yojson.Safe.t
val client_info_of_yojson : Yojson.Safe.t -> (client_info, string) result

type server_info = {
  name: string;
  version: string;
}

val server_info_to_yojson : server_info -> Yojson.Safe.t
val server_info_of_yojson : Yojson.Safe.t -> (server_info, string) result

type initialize_params = {
  protocol_version: string;
  capabilities: client_capabilities;
  client_info: client_info;
}

val initialize_params_to_yojson : initialize_params -> Yojson.Safe.t
val initialize_params_of_yojson : Yojson.Safe.t -> (initialize_params, string) result

type initialize_result = {
  protocol_version: string;
  capabilities: server_capabilities;
  server_info: server_info;
  instructions: string option;
}

val initialize_result_to_yojson : initialize_result -> Yojson.Safe.t
val initialize_result_of_yojson : Yojson.Safe.t -> (initialize_result, string) result

(** {2 Pagination} *)

type cursor = string option

val cursor_to_yojson : cursor -> Yojson.Safe.t
val cursor_of_yojson : Yojson.Safe.t -> (cursor, string) result

type paginated_params = {
  cursor: cursor;
}

val paginated_params_to_yojson : paginated_params -> Yojson.Safe.t
val paginated_params_of_yojson : Yojson.Safe.t -> (paginated_params, string) result

type 'a paginated_result = {
  items: 'a list;
  next_cursor: cursor;
}

val paginated_result_to_yojson : ('a -> Yojson.Safe.t) -> 'a paginated_result -> Yojson.Safe.t
val paginated_result_of_yojson : (Yojson.Safe.t -> ('a, string) result) -> Yojson.Safe.t -> ('a paginated_result, string) result

(** {2 Convenience Constructors} *)

val make_tool : name:string -> ?description:string -> ?title:string -> ?annotations:tool_annotations -> ?input_schema:Yojson.Safe.t -> unit -> tool
val make_resource : uri:string -> name:string -> ?description:string -> ?mime_type:string -> unit -> resource
val make_prompt : name:string -> ?description:string -> ?arguments:prompt_argument list -> unit -> prompt

(** {2 Tool Result Helpers} *)

val tool_result_of_text : string -> tool_result
val tool_result_of_error : string -> tool_result

(** {2 Roots} *)

type root = {
  uri: string;
  name: string option;
}

val root_to_yojson : root -> Yojson.Safe.t
val root_of_yojson : Yojson.Safe.t -> (root, string) result

type roots_capability = {
  list_changed: bool option;
}

val roots_capability_to_yojson : roots_capability -> Yojson.Safe.t
val roots_capability_of_yojson : Yojson.Safe.t -> (roots_capability, string) result

val make_root : uri:string -> ?name:string -> unit -> root

(** {2 Completion} *)

type completion_reference =
  | Prompt_ref of { name: string }
  | Resource_ref of { uri: string }

val completion_reference_to_yojson : completion_reference -> Yojson.Safe.t
val completion_reference_of_yojson : Yojson.Safe.t -> (completion_reference, string) result

type completion_argument = {
  name: string;
  value: string;
}

val completion_argument_to_yojson : completion_argument -> Yojson.Safe.t
val completion_argument_of_yojson : Yojson.Safe.t -> (completion_argument, string) result

val make_completion_argument : name:string -> value:string -> completion_argument

type completion_result = {
  values: string list;
  total: int option;
  has_more: bool option;
}

val completion_result_to_yojson : completion_result -> Yojson.Safe.t
val completion_result_of_yojson : Yojson.Safe.t -> (completion_result, string) result

val make_completion_result : values:string list -> ?total:int -> ?has_more:bool -> unit -> completion_result

type completion_context = {
  arguments: (string * string) list option;
}

val completion_context_to_yojson : completion_context -> Yojson.Safe.t
val completion_context_of_yojson : Yojson.Safe.t -> (completion_context, string) result

(** {2 Elicitation} *)

type elicitation_schema = {
  type_: string;
  properties: (string * Yojson.Safe.t) list;
  required: string list option;
}

val elicitation_schema_to_yojson : elicitation_schema -> Yojson.Safe.t
val elicitation_schema_of_yojson : Yojson.Safe.t -> (elicitation_schema, string) result

type elicitation_params = {
  message: string;
  requested_schema: elicitation_schema option;
}

val elicitation_params_to_yojson : elicitation_params -> Yojson.Safe.t
val elicitation_params_of_yojson : Yojson.Safe.t -> (elicitation_params, string) result

type elicitation_action = Accept | Decline | Cancel

val elicitation_action_to_yojson : elicitation_action -> Yojson.Safe.t
val elicitation_action_of_yojson : Yojson.Safe.t -> (elicitation_action, string) result

type elicitation_result = {
  action: elicitation_action;
  content: (string * Yojson.Safe.t) list option;
}

val elicitation_result_to_yojson : elicitation_result -> Yojson.Safe.t
val elicitation_result_of_yojson : Yojson.Safe.t -> (elicitation_result, string) result
