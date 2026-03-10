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
  | V_2025_11_25

let protocol_version_to_string = function
  | V_2024_11_05 -> "2024-11-05"
  | V_2025_03_26 -> "2025-03-26"
  | V_2025_11_25 -> "2025-11-25"

let protocol_version_of_string = function
  | "2024-11-05" -> Some V_2024_11_05
  | "2025-03-26" -> Some V_2025_03_26
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
[@@deriving yojson]

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

(** {2 Tools} *)

(** Tool definition - a function that can be called.
    MCP 2025-11-25 adds optional title and annotations fields. *)
type tool = {
  name: string;
  description: string option;    [@default None]
  input_schema: Yojson.Safe.t;   [@key "inputSchema"]
  title: string option;          [@default None]
  annotations: tool_annotations option; [@default None]
  icon: string option;           [@default None]
}
[@@deriving yojson]

(** Alias for figma-mcp compatibility *)
type tool_def = tool

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
    Result.map (fun uri -> { uri; text; blob; mime_type }) uri
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
}

let tool_result_to_yojson (r : tool_result) =
  let fields = [("content", tool_content_list_to_yojson r.content)] in
  let fields = match r.is_error with
    | Some b -> ("isError", `Bool b) :: fields | None -> fields
  in
  let fields = match r.structured_content with
    | Some j -> ("structuredContent", j) :: fields | None -> fields
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
    Result.map (fun content -> { content; is_error; structured_content }) content
  | _ -> Error "tool_result: expected object"

(** {2 Resources} *)

(** Resource definition - a data source *)
type resource = {
  uri: string;
  name: string;
  description: string option; [@default None]
  mime_type: string option; [@default None]
  icon: string option; [@default None]
}
[@@deriving yojson]

(** Resource template - for dynamic resources *)
type resource_template = {
  uri_template: string; [@key "uriTemplate"]
  name: string;
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

(** {2 Capabilities} *)

(** Server capabilities *)
type server_capabilities = {
  tools: Yojson.Safe.t option; [@default None]
  resources: Yojson.Safe.t option; [@default None]
  prompts: Yojson.Safe.t option; [@default None]
  logging: Yojson.Safe.t option; [@default None]
  experimental: Yojson.Safe.t option; [@default None]
}
[@@deriving yojson]

(** Client capabilities *)
type client_capabilities = {
  roots: Yojson.Safe.t option; [@default None]
  sampling: Yojson.Safe.t option; [@default None]
  elicitation: Yojson.Safe.t option; [@default None]
  experimental: Yojson.Safe.t option; [@default None]
}
[@@deriving yojson]

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
}
[@@deriving yojson]

(** Initialize response result *)
type initialize_result = {
  protocol_version: string; [@key "protocolVersion"]
  capabilities: server_capabilities;
  server_info: server_info; [@key "serverInfo"]
  instructions: string option; [@default None]
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
    ?(input_schema = `Assoc [("type", `String "object")]) () =
  { name; description; input_schema; title; annotations; icon }

(** Create a resource definition *)
let make_resource ~uri ~name ?description ?mime_type ?icon () =
  { uri; name; description; mime_type; icon }

(** Create a prompt definition *)
let make_prompt ~name ?description ?arguments ?icon () =
  { name; description; arguments; icon }

(** {2 Tool Result Helpers} *)

(** Create a text tool result *)
let tool_result_of_text text =
  { content = [TextContent { type_ = "text"; text; annotations = None }];
    is_error = None;
    structured_content = None }

(** Create an error tool result *)
let tool_result_of_error message =
  { content = [TextContent { type_ = "text"; text = message; annotations = None }];
    is_error = Some true;
    structured_content = None }

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

(** {2 Completion} *)

(** Reference to the item being completed — either a prompt or a resource.
    MCP spec uses a "type" discriminator field:
    - \{ "type": "ref/prompt", "name": "..." \}
    - \{ "type": "ref/resource", "uri": "..." \}
*)
type completion_reference =
  | Prompt_ref of { name: string }
  | Resource_ref of { uri: string }

let completion_reference_to_yojson = function
  | Prompt_ref { name } ->
    `Assoc [("type", `String "ref/prompt"); ("name", `String name)]
  | Resource_ref { uri } ->
    `Assoc [("type", `String "ref/resource"); ("uri", `String uri)]

let completion_reference_of_yojson = function
  | `Assoc fields -> begin
    match List.assoc_opt "type" fields with
    | Some (`String "ref/prompt") -> begin
      match List.assoc_opt "name" fields with
      | Some (`String name) -> Ok (Prompt_ref { name })
      | _ -> Error "completion_reference: ref/prompt missing 'name'"
    end
    | Some (`String "ref/resource") -> begin
      match List.assoc_opt "uri" fields with
      | Some (`String uri) -> Ok (Resource_ref { uri })
      | _ -> Error "completion_reference: ref/resource missing 'uri'"
    end
    | Some (`String t) -> Error ("completion_reference: unknown type " ^ t)
    | _ -> Error "completion_reference: missing 'type'"
  end
  | _ -> Error "completion_reference: expected object"

(** Argument to complete — the name and current partial value *)
type completion_argument = {
  name: string;
  value: string;
}
[@@deriving yojson]

(** Create a completion argument *)
let make_completion_argument ~name ~value = { name; value }

(** Result of completion/complete — a list of suggested values *)
type completion_result = {
  values: string list;
  total: int option; [@default None]
  has_more: bool option; [@default None] [@key "hasMore"]
}
[@@deriving yojson]

(** Create a completion result *)
let make_completion_result ~values ?total ?has_more () =
  { values; total; has_more }

(** Completion context — wrapping arguments for the completion/complete request.
    MCP 2025-11-25 adds this context wrapper. *)
type completion_context = {
  arguments: (string * string) list option; [@default None]
}

let completion_context_to_yojson (ctx : completion_context) =
  let fields = match ctx.arguments with
    | Some pairs ->
      let args = `Assoc (List.map (fun (k, v) -> (k, `String v)) pairs) in
      [("arguments", args)]
    | None -> []
  in
  `Assoc fields

let completion_context_of_yojson = function
  | `Assoc fields ->
    let arguments = match List.assoc_opt "arguments" fields with
      | Some (`Assoc pairs) ->
        Some (List.filter_map (fun (k, v) ->
          match v with `String s -> Some (k, s) | _ -> None
        ) pairs)
      | _ -> None
    in
    Ok { arguments }
  | _ -> Error "completion_context: expected object"

(** {2 Elicitation} *)

(** Elicitation schema — a JSON Schema subset for requesting user input.
    MCP 2025-11-25: servers can ask clients to collect structured user input. *)
type elicitation_schema = {
  type_: string;  [@key "type"]
  properties: (string * Yojson.Safe.t) list;
  required: string list option; [@default None]
}

let elicitation_schema_to_yojson (s : elicitation_schema) =
  let fields = [
    ("type", `String s.type_);
    ("properties", `Assoc s.properties);
  ] in
  let fields = match s.required with
    | Some reqs -> ("required", `List (List.map (fun r -> `String r) reqs)) :: fields
    | None -> fields
  in
  `Assoc fields

let elicitation_schema_of_yojson = function
  | `Assoc fields ->
    let type_ = match List.assoc_opt "type" fields with
      | Some (`String s) -> Ok s
      | _ -> Error "elicitation_schema: missing 'type'"
    in
    let properties = match List.assoc_opt "properties" fields with
      | Some (`Assoc pairs) -> Ok pairs
      | _ -> Ok []
    in
    let required = match List.assoc_opt "required" fields with
      | Some (`List items) ->
        Some (List.filter_map (function `String s -> Some s | _ -> None) items)
      | _ -> None
    in
    (match type_, properties with
     | Ok type_, Ok properties -> Ok { type_; properties; required }
     | Error e, _ | _, Error e -> Error e)
  | _ -> Error "elicitation_schema: expected object"

(** Parameters for elicitation/create request *)
type elicitation_params = {
  message: string;
  requested_schema: elicitation_schema option; [@default None]
  mode: string option;
}

let elicitation_params_to_yojson (p : elicitation_params) =
  let fields = [("message", `String p.message)] in
  let fields = match p.requested_schema with
    | Some s -> ("requestedSchema", elicitation_schema_to_yojson s) :: fields
    | None -> fields
  in
  let fields = match p.mode with
    | Some m -> ("mode", `String m) :: fields
    | None -> fields
  in
  `Assoc fields

let elicitation_params_of_yojson = function
  | `Assoc fields ->
    let message = match List.assoc_opt "message" fields with
      | Some (`String s) -> Ok s
      | _ -> Error "elicitation_params: missing 'message'"
    in
    let requested_schema = match List.assoc_opt "requestedSchema" fields with
      | Some j -> (match elicitation_schema_of_yojson j with Ok s -> Some s | Error _ -> None)
      | None -> None
    in
    let mode = match List.assoc_opt "mode" fields with
      | Some (`String s) -> Some s
      | _ -> None
    in
    Result.map (fun message -> { message; requested_schema; mode }) message
  | _ -> Error "elicitation_params: expected object"

(** User response action for elicitation *)
type elicitation_action = Accept | Decline | Cancel

let elicitation_action_to_yojson = function
  | Accept -> `String "accept"
  | Decline -> `String "decline"
  | Cancel -> `String "cancel"

let elicitation_action_of_yojson = function
  | `String "accept" -> Ok Accept
  | `String "decline" -> Ok Decline
  | `String "cancel" -> Ok Cancel
  | _ -> Error "elicitation_action: expected 'accept', 'decline', or 'cancel'"

(** Result of elicitation/create *)
type elicitation_result = {
  action: elicitation_action;
  content: (string * Yojson.Safe.t) list option; [@default None]
}

let elicitation_result_to_yojson (r : elicitation_result) =
  let fields = [("action", elicitation_action_to_yojson r.action)] in
  let fields = match r.content with
    | Some pairs -> ("content", `Assoc pairs) :: fields
    | None -> fields
  in
  `Assoc fields

let elicitation_result_of_yojson = function
  | `Assoc fields ->
    let action = match List.assoc_opt "action" fields with
      | Some j -> elicitation_action_of_yojson j
      | None -> Error "elicitation_result: missing 'action'"
    in
    let content = match List.assoc_opt "content" fields with
      | Some (`Assoc pairs) -> Some pairs
      | _ -> None
    in
    Result.map (fun action -> { action; content }) action
  | _ -> Error "elicitation_result: expected object"

(** {2 Tasks (experimental, 2025-11-25)} *)

(** Task execution status.
    Tasks are durable state machines carrying execution state. *)
type task_status =
  | Working
  | Input_required
  | Completed
  | Failed
  | Cancelled

let task_status_to_yojson = function
  | Working -> `String "working"
  | Input_required -> `String "input_required"
  | Completed -> `String "completed"
  | Failed -> `String "failed"
  | Cancelled -> `String "cancelled"

let task_status_of_yojson = function
  | `String "working" -> Ok Working
  | `String "input_required" -> Ok Input_required
  | `String "completed" -> Ok Completed
  | `String "failed" -> Ok Failed
  | `String "cancelled" -> Ok Cancelled
  | _ -> Error "task_status: expected 'working', 'input_required', 'completed', 'failed', or 'cancelled'"

let task_status_is_terminal = function
  | Completed | Failed | Cancelled -> true
  | Working | Input_required -> false

(** Task object — execution state of a request. *)
type task = {
  task_id: string;
  status: task_status;
  status_message: string option;
  created_at: string;
  last_updated_at: string;
  ttl: int option;
  poll_interval: int option;
}

let task_to_yojson (t : task) =
  let fields = [
    ("taskId", `String t.task_id);
    ("status", task_status_to_yojson t.status);
    ("createdAt", `String t.created_at);
    ("lastUpdatedAt", `String t.last_updated_at);
  ] in
  let fields = match t.status_message with
    | Some m -> ("statusMessage", `String m) :: fields | None -> fields
  in
  let fields = match t.ttl with
    | Some v -> ("ttl", `Int v) :: fields | None -> fields
  in
  let fields = match t.poll_interval with
    | Some v -> ("pollInterval", `Int v) :: fields | None -> fields
  in
  `Assoc fields

let task_of_yojson = function
  | `Assoc fields ->
    let task_id = match List.assoc_opt "taskId" fields with
      | Some (`String s) -> Ok s
      | _ -> Error "task: missing 'taskId'"
    in
    let status = match List.assoc_opt "status" fields with
      | Some j -> task_status_of_yojson j
      | None -> Error "task: missing 'status'"
    in
    let status_message = match List.assoc_opt "statusMessage" fields with
      | Some (`String s) -> Some s | _ -> None
    in
    let created_at = match List.assoc_opt "createdAt" fields with
      | Some (`String s) -> Ok s
      | _ -> Error "task: missing 'createdAt'"
    in
    let last_updated_at = match List.assoc_opt "lastUpdatedAt" fields with
      | Some (`String s) -> Ok s
      | _ -> Error "task: missing 'lastUpdatedAt'"
    in
    let ttl = match List.assoc_opt "ttl" fields with
      | Some (`Int n) -> Some n
      | Some (`Null) -> None
      | _ -> None
    in
    let poll_interval = match List.assoc_opt "pollInterval" fields with
      | Some (`Int n) -> Some n | _ -> None
    in
    (match task_id, status, created_at, last_updated_at with
     | Ok task_id, Ok status, Ok created_at, Ok last_updated_at ->
       Ok { task_id; status; status_message; created_at; last_updated_at; ttl; poll_interval }
     | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _ | _, _, _, Error e ->
       Error e)
  | _ -> Error "task: expected object"

(** Task parameters — included in request params to create a task. *)
type task_params = {
  ttl: int option;
}

let task_params_to_yojson (p : task_params) =
  let fields = match p.ttl with
    | Some v -> [("ttl", `Int v)]
    | None -> []
  in
  `Assoc fields

let task_params_of_yojson = function
  | `Assoc fields ->
    let ttl = match List.assoc_opt "ttl" fields with
      | Some (`Int n) -> Some n | _ -> None
    in
    Ok { ttl }
  | _ -> Error "task_params: expected object"

(** CreateTaskResult — response to a task-augmented request. *)
type create_task_result = {
  task: task;
}

let create_task_result_to_yojson (r : create_task_result) =
  `Assoc [("task", task_to_yojson r.task)]

let create_task_result_of_yojson = function
  | `Assoc fields ->
    (match List.assoc_opt "task" fields with
     | Some j ->
       Result.map (fun task -> { task }) (task_of_yojson j)
     | None -> Error "create_task_result: missing 'task'")
  | _ -> Error "create_task_result: expected object"

(** Tool-level task support declaration. *)
type task_execution_support = Task_required | Task_optional | Task_forbidden

let task_execution_support_to_yojson = function
  | Task_required -> `String "required"
  | Task_optional -> `String "optional"
  | Task_forbidden -> `String "forbidden"

let task_execution_support_of_yojson = function
  | `String "required" -> Ok Task_required
  | `String "optional" -> Ok Task_optional
  | `String "forbidden" -> Ok Task_forbidden
  | _ -> Error "task_execution_support: expected 'required', 'optional', or 'forbidden'"

(** Convenience: create a task in working state. *)
let make_task ~task_id ~created_at ?status_message ?ttl ?poll_interval () =
  { task_id; status = Working; status_message;
    created_at; last_updated_at = created_at;
    ttl; poll_interval }
