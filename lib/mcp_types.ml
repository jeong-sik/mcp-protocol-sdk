(** MCP Protocol Types - Core primitives.

    The Model Context Protocol defines three core primitives:
    - Tools: Functions that can be called by the LLM
    - Resources: Data sources that can be read
    - Prompts: Reusable prompt templates

    Reference: https://modelcontextprotocol.io/docs/concepts/
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

(** {2 Tools} *)

(** Tool definition - a function that can be called *)
type tool = {
  name: string;
  description: string option; [@default None]
  input_schema: Yojson.Safe.t;
}
[@@deriving yojson]

(** Alias for figma-mcp compatibility *)
type tool_def = tool

(** Tool call result content types *)
type tool_content =
  | TextContent of { type_: string; text: string }
  | ImageContent of { type_: string; data: string; mime_type: string }
  | ResourceContent of { type_: string; resource: embedded_resource }
and embedded_resource = {
  uri: string;
  text: string option;
  blob: string option;
  mime_type: string option;
}
[@@deriving yojson]

(** Tool call result *)
type tool_result = {
  content: tool_content list;
  is_error: bool option; [@default None]
}
[@@deriving yojson]

(** {2 Resources} *)

(** Resource definition - a data source *)
type resource = {
  uri: string;
  name: string;
  description: string option; [@default None]
  mime_type: string option; [@default None]
}
[@@deriving yojson]

(** Resource template - for dynamic resources *)
type resource_template = {
  uri_template: string; [@key "uriTemplate"]
  name: string;
  description: string option; [@default None]
  mime_type: string option; [@default None]
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
}
[@@deriving yojson]

(** Prompt message role *)
type role = User | Assistant

let role_to_yojson = function
  | User -> `String "user"
  | Assistant -> `String "assistant"

let role_of_yojson = function
  | `String "user" -> Ok User
  | `String "assistant" -> Ok Assistant
  | _ -> Error "Invalid role"

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
let make_tool ~name ?description ?(input_schema = `Assoc [("type", `String "object")]) () =
  { name; description; input_schema }

(** Create a resource definition *)
let make_resource ~uri ~name ?description ?mime_type () =
  { uri; name; description; mime_type }

(** Create a prompt definition *)
let make_prompt ~name ?description ?arguments () =
  { name; description; arguments }

(** {2 Tool Result Helpers} *)

(** Create a text tool result *)
let tool_result_of_text text =
  { content = [TextContent { type_ = "text"; text }]; is_error = None }

(** Create an error tool result *)
let tool_result_of_error message =
  { content = [TextContent { type_ = "text"; text = message }]; is_error = Some true }
