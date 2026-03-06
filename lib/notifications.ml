(** MCP method string constants.

    Centralizes all MCP JSON-RPC method names to prevent typos
    and enable IDE autocompletion.

    Reference: https://modelcontextprotocol.io/docs/concepts/
*)

(** {2 Lifecycle} *)

(** Client → Server: initialize handshake *)
let initialize = "initialize"

(** Client → Server: initialization complete *)
let initialized = "notifications/initialized"

(** Either → Either: graceful shutdown request *)
let shutdown = "shutdown"

(** {2 Tools} *)

let tools_list = "tools/list"
let tools_call = "tools/call"
let tools_list_changed = "notifications/tools/list_changed"

(** {2 Resources} *)

let resources_list = "resources/list"
let resources_read = "resources/read"
let resources_subscribe = "resources/subscribe"
let resources_unsubscribe = "resources/unsubscribe"
let resources_list_changed = "notifications/resources/list_changed"
let resources_updated = "notifications/resources/updated"

(** Resource templates *)
let resources_templates_list = "resources/templates/list"

(** {2 Prompts} *)

let prompts_list = "prompts/list"
let prompts_get = "prompts/get"
let prompts_list_changed = "notifications/prompts/list_changed"

(** {2 Logging} *)

let logging_set_level = "logging/setLevel"
let logging_message = "notifications/message"

(** {2 Sampling} *)

let sampling_create_message = "sampling/createMessage"

(** {2 Roots} *)

let roots_list = "roots/list"
let roots_list_changed = "notifications/roots/list_changed"

(** {2 Completion} *)

let completion_complete = "completion/complete"

(** {2 Progress & Cancellation} *)

let progress = "notifications/progress"
let cancelled = "notifications/cancelled"

(** {2 Ping} *)

let ping = "ping"

(** {2 Elicitation (draft, 2025-03-26+)} *)

let elicitation_create = "elicitation/create"

(** {2 Utility} *)

(** Check if a method string is a notification (no response expected). *)
let is_notification method_ =
  String.length method_ >= 14 &&
  String.sub method_ 0 14 = "notifications/"
