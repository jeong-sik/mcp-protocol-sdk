(** MCP Protocol Types - Core primitives.

    The Model Context Protocol defines three core primitives:
    - Tools: Functions that can be called by the LLM
    - Resources: Data sources that can be read
    - Prompts: Reusable prompt templates

    Reference: https://modelcontextprotocol.io/docs/concepts/
    Spec version: 2025-11-25
*)

(** {2 Protocol Version} *)

(** Supported MCP protocol versions *)
type protocol_version =
  | V_2024_11_05
  | V_2025_03_26
  | V_2025_06_18
  | V_2025_11_25

let protocol_version_to_string = function
  | V_2024_11_05 -> "2024-11-05"
  | V_2025_03_26 -> "2025-03-26"
  | V_2025_06_18 -> "2025-06-18"
  | V_2025_11_25 -> "2025-11-25"

let protocol_version_of_string = function
  | "2024-11-05" -> Some V_2024_11_05
  | "2025-03-26" -> Some V_2025_03_26
  | "2025-06-18" -> Some V_2025_06_18
  | "2025-11-25" -> Some V_2025_11_25
  | _ -> None

(** Latest protocol version *)
let latest_version = V_2025_11_25

(** {2 Roles} *)

(** Message role *)
type role = User | Assistant

let role_to_yojson = function
  | User -> `String "user"
  | Assistant -> `String "assistant"

let role_of_yojson = function
  | `String "user" -> Ok User
  | `String "assistant" -> Ok Assistant
  | _ -> Error "Invalid role"

(** {2 Content Annotations} *)

(** Annotations on content items for client display hints.
    MCP 2025-11-25: audience restricts who sees the content,
    priority (0.0=low .. 1.0=high) suggests ordering. *)
type content_annotations = {
  audience: role list option; [@default None]
  priority: float option; [@default None]
}

(** H4 fix: Custom yojson for content_annotations because ppx_deriving_yojson
    rejects JSON integer values for float fields. MCP spec allows priority
    values like 0 and 1 (integers), not just 0.0 and 1.0. *)
let content_annotations_to_yojson (a : content_annotations) =
  let fields = [] in
  let fields = match a.audience with
    | Some roles -> ("audience", `List (List.map role_to_yojson roles)) :: fields
    | None -> fields
  in
  let fields = match a.priority with
    | Some p -> ("priority", `Float p) :: fields
    | None -> fields
  in
  `Assoc fields

let content_annotations_of_yojson = function
  | `Assoc fields ->
    let audience = match List.assoc_opt "audience" fields with
      | Some (`List items) ->
        let roles = List.filter_map (fun j ->
          match role_of_yojson j with Ok r -> Some r | Error _ -> None
        ) items in
        Some roles
      | _ -> None
    in
    let priority = match List.assoc_opt "priority" fields with
      | Some (`Float f) -> Some f
      | Some (`Int i) -> Some (float_of_int i)
      | _ -> None
    in
    Ok { audience; priority }
  | _ -> Error "content_annotations: expected object"

(** {2 Tool Annotations} *)

(** Behavioral hints for tools (MCP 2025-11-25).
    Clients use these to optimize UX — e.g. showing confirmation
    dialogs for destructive tools, or caching results of read-only tools. *)
type tool_annotations = {
  title: string option;            [@default None]
  read_only_hint: bool option;     [@default None] [@key "readOnlyHint"]
  destructive_hint: bool option;   [@default None] [@key "destructiveHint"]
  idempotent_hint: bool option;    [@default None] [@key "idempotentHint"]
  open_world_hint: bool option;    [@default None] [@key "openWorldHint"]
}
[@@deriving yojson]

(** {2 Tool Execution} *)

(** Tool execution configuration (MCP 2025-11-25).
    Declares whether the tool supports task-augmented requests. *)
type tool_execution = {
  task_support: Mcp_types_tasks.task_execution_support;
}

let tool_execution_to_yojson (e : tool_execution) =
  `Assoc [("taskSupport", Mcp_types_tasks.task_execution_support_to_yojson e.task_support)]

let tool_execution_of_yojson = function
  | `Assoc fields ->
    (match List.assoc_opt "taskSupport" fields with
     | Some j ->
       Result.map (fun ts -> { task_support = ts })
         (Mcp_types_tasks.task_execution_support_of_yojson j)
     | None -> Error "tool_execution: missing 'taskSupport'")
  | _ -> Error "tool_execution: expected object"

(** {2 Tools} *)

(** Tool definition - a function that can be called.
    MCP 2025-11-25 adds optional title, annotations, outputSchema, and execution fields. *)
type tool = {
  name: string;
  description: string option;    [@default None]
  input_schema: Yojson.Safe.t;   [@key "inputSchema"]
  output_schema: Yojson.Safe.t option; [@default None] [@key "outputSchema"]
  title: string option;          [@default None]
  annotations: tool_annotations option; [@default None]
  icon: string option;           [@default None]
  execution: tool_execution option; [@default None]
}
[@@deriving yojson]

(** Alias for figma-mcp compatibility *)
type tool_def = tool

let tool_of_yojson_generated = tool_of_yojson

let tool_of_yojson = function
  | `Assoc fields ->
    let normalized_fields =
      match List.assoc_opt "inputSchema" fields with
      | Some _ -> fields
      | None ->
        let fallback_schema =
          match List.assoc_opt "input_schema" fields with
          | Some schema -> schema
          | None -> `Assoc [("type", `String "object")]
        in
        ("inputSchema", fallback_schema)
        :: List.filter (fun (key, _) -> key <> "input_schema") fields
    in
    tool_of_yojson_generated (`Assoc normalized_fields)
  | json ->
    tool_of_yojson_generated json

(** Tool call result content types.
    MCP 2025-11-25 adds AudioContent and ResourceLinkContent. *)
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

let embedded_resource_to_yojson (r : embedded_resource) =
  let fields = [("uri", `String r.uri)] in
  let fields = match r.text with Some t -> ("text", `String t) :: fields | None -> fields in
  let fields = match r.blob with Some b -> ("blob", `String b) :: fields | None -> fields in
  let fields = match r.mime_type with Some m -> ("mimeType", `String m) :: fields | None -> fields in
  `Assoc fields

let embedded_resource_of_yojson = function
  | `Assoc fields ->
    let uri = match List.assoc_opt "uri" fields with
      | Some (`String s) -> Ok s
      | _ -> Error "embedded_resource: missing 'uri'"
    in
    let text = match List.assoc_opt "text" fields with
      | Some (`String s) -> Some s | _ -> None
    in
    let blob = match List.assoc_opt "blob" fields with
      | Some (`String s) -> Some s | _ -> None
    in
    let mime_type = match List.assoc_opt "mimeType" fields with
      | Some (`String s) -> Some s
      | _ -> match List.assoc_opt "mime_type" fields with
        | Some (`String s) -> Some s | _ -> None
    in
    (* M3 fix: MCP spec requires at least one of text or blob. *)
    Result.bind uri (fun uri ->
      if text = None && blob = None then
        Error "embedded_resource: must have at least one of 'text' or 'blob'"
      else
        Ok { uri; text; blob; mime_type })
  | _ -> Error "embedded_resource: expected object"

let annotations_field_to_json (a : content_annotations option) fields =
  match a with
  | Some ann -> ("annotations", content_annotations_to_yojson ann) :: fields
  | None -> fields

let tool_content_to_yojson = function
  | TextContent { type_; text; annotations } ->
    let fields = [("type", `String type_); ("text", `String text)] in
    `Assoc (annotations_field_to_json annotations fields)
  | ImageContent { type_; data; mime_type; annotations } ->
    let fields = [("type", `String type_); ("data", `String data); ("mimeType", `String mime_type)] in
    `Assoc (annotations_field_to_json annotations fields)
  | AudioContent { type_; data; mime_type; annotations } ->
    let fields = [("type", `String type_); ("data", `String data); ("mimeType", `String mime_type)] in
    `Assoc (annotations_field_to_json annotations fields)
  | ResourceContent { type_; resource; annotations } ->
    let fields = [("type", `String type_); ("resource", embedded_resource_to_yojson resource)] in
    `Assoc (annotations_field_to_json annotations fields)
  | ResourceLinkContent { type_; uri; name; description; mime_type; annotations } ->
    let fields = [("type", `String type_); ("uri", `String uri)] in
    let fields = match name with Some n -> ("name", `String n) :: fields | None -> fields in
    let fields = match description with Some d -> ("description", `String d) :: fields | None -> fields in
    let fields = match mime_type with Some m -> ("mimeType", `String m) :: fields | None -> fields in
    `Assoc (annotations_field_to_json annotations fields)

let parse_annotations fields =
  match List.assoc_opt "annotations" fields with
  | Some j -> (match content_annotations_of_yojson j with
    | Ok a -> Ok (Some a)
    | Error e -> Error ("content annotations: " ^ e))
  | None -> Ok None

let tool_content_of_yojson = function
  | `Assoc fields -> begin
    match List.assoc_opt "type" fields with
    | Some (`String "text") ->
      let text = match List.assoc_opt "text" fields with
        | Some (`String s) -> Ok s | _ -> Error "TextContent: missing 'text'"
      in
      Result.bind text (fun text ->
        Result.map (fun annotations ->
          TextContent { type_ = "text"; text; annotations }
        ) (parse_annotations fields))
    | Some (`String "image") ->
      let data = match List.assoc_opt "data" fields with
        | Some (`String s) -> Ok s | _ -> Error "ImageContent: missing 'data'"
      in
      let mime_type = match List.assoc_opt "mimeType" fields with
        | Some (`String s) -> Ok s | _ -> Error "ImageContent: missing 'mimeType'"
      in
      (match data, mime_type with
       | Ok data, Ok mime_type ->
         Result.map (fun annotations ->
           ImageContent { type_ = "image"; data; mime_type; annotations }
         ) (parse_annotations fields)
       | Error e, _ | _, Error e -> Error e)
    | Some (`String "audio") ->
      let data = match List.assoc_opt "data" fields with
        | Some (`String s) -> Ok s | _ -> Error "AudioContent: missing 'data'"
      in
      let mime_type = match List.assoc_opt "mimeType" fields with
        | Some (`String s) -> Ok s | _ -> Error "AudioContent: missing 'mimeType'"
      in
      (match data, mime_type with
       | Ok data, Ok mime_type ->
         Result.map (fun annotations ->
           AudioContent { type_ = "audio"; data; mime_type; annotations }
         ) (parse_annotations fields)
       | Error e, _ | _, Error e -> Error e)
    | Some (`String "resource") ->
      let resource = match List.assoc_opt "resource" fields with
        | Some j -> embedded_resource_of_yojson j
        | None -> Error "ResourceContent: missing 'resource'"
      in
      Result.bind resource (fun resource ->
        Result.map (fun annotations ->
          ResourceContent { type_ = "resource"; resource; annotations }
        ) (parse_annotations fields))
    | Some (`String "resource_link") ->
      let uri = match List.assoc_opt "uri" fields with
        | Some (`String s) -> Ok s | _ -> Error "ResourceLinkContent: missing 'uri'"
      in
      let name = match List.assoc_opt "name" fields with
        | Some (`String s) -> Some s | _ -> None
      in
      let description = match List.assoc_opt "description" fields with
        | Some (`String s) -> Some s | _ -> None
      in
      let mime_type = match List.assoc_opt "mimeType" fields with
        | Some (`String s) -> Some s | _ -> None
      in
      Result.bind uri (fun uri ->
        Result.map (fun annotations ->
          ResourceLinkContent {
            type_ = "resource_link"; uri; name; description;
            mime_type; annotations;
          }
        ) (parse_annotations fields))
    | Some (`String t) -> Error ("tool_content: unknown type " ^ t)
    | _ -> Error "tool_content: missing 'type'"
  end
  | _ -> Error "tool_content: expected object"

let tool_content_list_to_yojson lst =
  `List (List.map tool_content_to_yojson lst)

let tool_content_list_of_yojson = function
  | `List items ->
    List.fold_left (fun acc item ->
      match acc, tool_content_of_yojson item with
      | Ok acc, Ok item -> Ok (item :: acc)
      | Error e, _ | _, Error e -> Error e
    ) (Ok []) items
    |> Result.map List.rev
  | _ -> Error "tool_content list: expected array"

(** Tool call result.
    MCP 2025-11-25 adds structured_content for structured tool output. *)
type tool_result = {
  content: tool_content list;
  is_error: bool option; [@default None]
  structured_content: Yojson.Safe.t option; [@default None] [@key "structuredContent"]
  _meta: Yojson.Safe.t option; [@default None] [@key "_meta"]
}

let tool_result_to_yojson (r : tool_result) =
  let fields = [("content", tool_content_list_to_yojson r.content)] in
  let fields = match r.is_error with
    | Some b -> ("isError", `Bool b) :: fields | None -> fields
  in
  let fields = match r.structured_content with
    | Some j -> ("structuredContent", j) :: fields | None -> fields
  in
  let fields = match r._meta with
    | Some j -> ("_meta", j) :: fields | None -> fields
  in
  `Assoc fields

let tool_result_of_yojson = function
  | `Assoc fields ->
    let content = match List.assoc_opt "content" fields with
      | Some j -> tool_content_list_of_yojson j
      | None -> Ok []
    in
    let is_error = match List.assoc_opt "isError" fields with
      | Some (`Bool b) -> Some b
      | _ -> match List.assoc_opt "is_error" fields with
        | Some (`Bool b) -> Some b | _ -> None
    in
    let structured_content = match List.assoc_opt "structuredContent" fields with
      | Some (`Null) -> None
      | Some j -> Some j
      | None -> match List.assoc_opt "structured_content" fields with
        | Some (`Null) -> None | Some j -> Some j | None -> None
    in
    let _meta = match List.assoc_opt "_meta" fields with
      | Some (`Null) -> None | Some j -> Some j | None -> None
    in
    Result.map (fun content -> { content; is_error; structured_content; _meta }) content
  | _ -> Error "tool_result: expected object"

(** {2 Resources} *)

(** Resource definition - a data source *)
type resource = {
  uri: string;
  name: string;
  title: string option; [@default None]
  description: string option; [@default None]
  mime_type: string option; [@default None]
  icon: string option; [@default None]
}
[@@deriving yojson]

(** Resource template - for dynamic resources *)
type resource_template = {
  uri_template: string; [@key "uriTemplate"]
  name: string;
  title: string option; [@default None]
  description: string option; [@default None]
  mime_type: string option; [@default None]
  icon: string option; [@default None]
}
[@@deriving yojson]

(** Resource contents *)
type resource_contents = {
  uri: string;
  mime_type: string option; [@default None]
  text: string option; [@default None]
  blob: string option; [@default None]
}
[@@deriving yojson]

(** {2 Prompts} *)

(** Prompt argument definition *)
type prompt_argument = {
  name: string;
  description: string option; [@default None]
  required: bool option; [@default None]
}
[@@deriving yojson]

(** Prompt definition - a reusable prompt template *)
type prompt = {
  name: string;
  title: string option; [@default None]
  description: string option; [@default None]
  arguments: prompt_argument list option; [@default None]
  icon: string option; [@default None]
}
[@@deriving yojson]

(** Prompt message content *)
type prompt_content =
  | PromptText of { type_: string; text: string }
  | PromptImage of { type_: string; data: string; mime_type: string }
  | PromptResource of { type_: string; resource: embedded_resource }
[@@deriving yojson]

(** Prompt message *)
type prompt_message = {
  role: role;
  content: prompt_content;
}
[@@deriving yojson]

(** Prompt result *)
type prompt_result = {
  description: string option; [@default None]
  messages: prompt_message list;
}
[@@deriving yojson]

(** {2 Roots} *)

(** A root that a client exposes to the server (filesystem/workspace root).
    Reference: https://modelcontextprotocol.io/docs/concepts/roots *)
type root = {
  uri: string;
  name: string option; [@default None]
}
[@@deriving yojson]

(** Capability for roots — declares whether the client supports
    roots/list_changed notifications. *)
type roots_capability = {
  list_changed: bool option; [@default None] [@key "listChanged"]
}
[@@deriving yojson]

(** Create a root value *)
let make_root ~uri ?name () = { uri; name }

(** {2 Capability Sub-types} *)

type tools_capability = {
  list_changed: bool option; [@default None] [@key "listChanged"]
}
[@@deriving yojson]

type resources_capability = {
  subscribe: bool option; [@default None]
  list_changed: bool option; [@default None] [@key "listChanged"]
}
[@@deriving yojson]

type prompts_capability = {
  list_changed: bool option; [@default None] [@key "listChanged"]
}
[@@deriving yojson]

(** {2 Capabilities} *)

(** Server capabilities — typed per MCP 2025-11-25 spec.
    [logging] and [completions] have no sub-fields; [Some ()] = enabled. *)
type server_capabilities = {
  tools: tools_capability option;
  resources: resources_capability option;
  prompts: prompts_capability option;
  logging: unit option;
  completions: unit option;
  experimental: Yojson.Safe.t option;
  extensions: Yojson.Safe.t option;
}

let server_capabilities_to_yojson (c : server_capabilities) =
  let fields = [] in
  let fields = match c.extensions with
    | Some j -> ("extensions", j) :: fields | None -> fields
  in
  let fields = match c.experimental with
    | Some j -> ("experimental", j) :: fields | None -> fields
  in
  let fields = match c.completions with
    | Some () -> ("completions", `Assoc []) :: fields | None -> fields
  in
  let fields = match c.logging with
    | Some () -> ("logging", `Assoc []) :: fields | None -> fields
  in
  let fields = match c.prompts with
    | Some p -> ("prompts", prompts_capability_to_yojson p) :: fields | None -> fields
  in
  let fields = match c.resources with
    | Some r -> ("resources", resources_capability_to_yojson r) :: fields | None -> fields
  in
  let fields = match c.tools with
    | Some t -> ("tools", tools_capability_to_yojson t) :: fields | None -> fields
  in
  `Assoc fields

let server_capabilities_of_yojson = function
  | `Assoc fields ->
    let tools = match List.assoc_opt "tools" fields with
      | Some j -> (match tools_capability_of_yojson j with Ok v -> Some v | Error _ -> None)
      | None -> None
    in
    let resources = match List.assoc_opt "resources" fields with
      | Some j -> (match resources_capability_of_yojson j with Ok v -> Some v | Error _ -> None)
      | None -> None
    in
    let prompts = match List.assoc_opt "prompts" fields with
      | Some j -> (match prompts_capability_of_yojson j with Ok v -> Some v | Error _ -> None)
      | None -> None
    in
    let logging = match List.assoc_opt "logging" fields with
      | Some _ -> Some () | None -> None
    in
    let completions = match List.assoc_opt "completions" fields with
      | Some _ -> Some () | None -> None
    in
    let experimental = match List.assoc_opt "experimental" fields with
      | Some (`Null) -> None | Some j -> Some j | None -> None
    in
    let extensions = match List.assoc_opt "extensions" fields with
      | Some (`Null) -> None | Some j -> Some j | None -> None
    in
    Ok { tools; resources; prompts; logging; completions; experimental; extensions }
  | _ -> Error "server_capabilities: expected object"

(** Client capabilities — typed per MCP 2025-11-25 spec. *)
type client_capabilities = {
  roots: roots_capability option;
  sampling: unit option;
  elicitation: unit option;
  experimental: Yojson.Safe.t option;
  extensions: Yojson.Safe.t option;
}

let client_capabilities_to_yojson (c : client_capabilities) =
  let fields = [] in
  let fields = match c.extensions with
    | Some j -> ("extensions", j) :: fields | None -> fields
  in
  let fields = match c.experimental with
    | Some j -> ("experimental", j) :: fields | None -> fields
  in
  let fields = match c.elicitation with
    | Some () -> ("elicitation", `Assoc []) :: fields | None -> fields
  in
  let fields = match c.sampling with
    | Some () -> ("sampling", `Assoc []) :: fields | None -> fields
  in
  let fields = match c.roots with
    | Some r -> ("roots", roots_capability_to_yojson r) :: fields | None -> fields
  in
  `Assoc fields

let client_capabilities_of_yojson = function
  | `Assoc fields ->
    let roots = match List.assoc_opt "roots" fields with
      | Some j -> (match roots_capability_of_yojson j with Ok v -> Some v | Error _ -> None)
      | None -> None
    in
    let sampling = match List.assoc_opt "sampling" fields with
      | Some _ -> Some () | None -> None
    in
    let elicitation = match List.assoc_opt "elicitation" fields with
      | Some _ -> Some () | None -> None
    in
    let experimental = match List.assoc_opt "experimental" fields with
      | Some (`Null) -> None | Some j -> Some j | None -> None
    in
    let extensions = match List.assoc_opt "extensions" fields with
      | Some (`Null) -> None | Some j -> Some j | None -> None
    in
    Ok { roots; sampling; elicitation; experimental; extensions }
  | _ -> Error "client_capabilities: expected object"

(** {2 Initialize} *)

(** Client info in initialize request *)
type client_info = {
  name: string;
  version: string;
}
[@@deriving yojson]

(** Server info in initialize response *)
type server_info = {
  name: string;
  version: string;
}
[@@deriving yojson]

(** Initialize request params *)
type initialize_params = {
  protocol_version: string; [@key "protocolVersion"]
  capabilities: client_capabilities;
  client_info: client_info; [@key "clientInfo"]
  _meta: Yojson.Safe.t option; [@default None] [@key "_meta"]
}
[@@deriving yojson]

(** Initialize response result *)
type initialize_result = {
  protocol_version: string; [@key "protocolVersion"]
  capabilities: server_capabilities;
  server_info: server_info; [@key "serverInfo"]
  instructions: string option; [@default None]
  _meta: Yojson.Safe.t option; [@default None] [@key "_meta"]
}
[@@deriving yojson]

(** {2 Pagination} *)

(** Cursor for pagination *)
type cursor = string option

let cursor_to_yojson = function
  | None -> `Null
  | Some s -> `String s

let cursor_of_yojson = function
  | `Null -> Ok None
  | `String s -> Ok (Some s)
  | _ -> Error "Invalid cursor"

(** Paginated request params *)
type paginated_params = {
  cursor: cursor; [@default None]
}
[@@deriving yojson]

(** Paginated response (generic, manual serialization) *)
type 'a paginated_result = {
  items: 'a list;
  next_cursor: cursor;
}

let paginated_result_to_yojson item_to_yojson result =
  let items_json = `List (List.map item_to_yojson result.items) in
  let fields = [("items", items_json)] in
  let fields = match result.next_cursor with
    | None -> fields
    | Some c -> ("nextCursor", `String c) :: fields
  in
  `Assoc fields

let paginated_result_of_yojson item_of_yojson = function
  | `Assoc fields ->
    let open Result in
    let items = match List.assoc_opt "items" fields with
      | Some (`List items) ->
        List.fold_left (fun acc item ->
          match acc, item_of_yojson item with
          | Ok acc, Ok item -> Ok (item :: acc)
          | Error e, _ | _, Error e -> Error e
        ) (Ok []) items
        |> Result.map List.rev
      | _ -> Error "Missing or invalid items"
    in
    let next_cursor = match List.assoc_opt "nextCursor" fields with
      | Some (`String s) -> Some s
      | _ -> None
    in
    items |> Result.map (fun items -> { items; next_cursor })
  | _ -> Error "Invalid paginated_result"

(** {2 Convenience Constructors} *)

(** Create a tool definition *)
let make_tool ~name ?description ?title ?annotations ?icon
    ?(input_schema = `Assoc [("type", `String "object")])
    ?output_schema ?execution () =
  { name; description; input_schema; output_schema; title; annotations; icon; execution }

(** Create a resource definition *)
let make_resource ~uri ~name ?title ?description ?mime_type ?icon () =
  { uri; name; title; description; mime_type; icon }

(** Create a prompt definition *)
let make_prompt ~name ?title ?description ?arguments ?icon () =
  { name; title; description; arguments; icon }

(** {2 Tool Result Helpers} *)

(** {2 Type-safe Content Constructors}

    M4 fix: These constructors enforce that the [type_] discriminator matches
    the content variant, preventing mismatches like
    [TextContent \{ type_ = "image"; ... \}]. *)

let make_text_content ?annotations text =
  TextContent { type_ = "text"; text; annotations }

let make_image_content ?annotations ~mime_type data =
  ImageContent { type_ = "image"; data; mime_type; annotations }

let make_audio_content ?annotations ~mime_type data =
  AudioContent { type_ = "audio"; data; mime_type; annotations }

let make_resource_content ?annotations resource =
  ResourceContent { type_ = "resource"; resource; annotations }

let make_resource_link_content ?annotations ?name ?description ?mime_type uri =
  ResourceLinkContent { type_ = "resource_link"; uri; name; description; mime_type; annotations }

(** Create a text tool result *)
let tool_result_of_text text =
  { content = [make_text_content text];
    is_error = None;
    structured_content = None;
    _meta = None }

(** Create an error tool result *)
let tool_result_of_error message =
  { content = [make_text_content message];
    is_error = Some true;
    structured_content = None;
    _meta = None }

(** {2 Completion} *)
include Mcp_types_completion

(** {2 Elicitation} *)
include Mcp_types_elicitation

(** {2 Tasks (experimental, 2025-11-25)} *)
include Mcp_types_tasks
