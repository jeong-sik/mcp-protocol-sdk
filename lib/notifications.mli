(** MCP method string constants.

    Centralizes all MCP JSON-RPC method names.
*)

(** {2 Lifecycle} *)

val initialize : string
val initialized : string
val shutdown : string

(** {2 Tools} *)

val tools_list : string
val tools_call : string
val tools_list_changed : string

(** {2 Resources} *)

val resources_list : string
val resources_read : string
val resources_subscribe : string
val resources_unsubscribe : string
val resources_list_changed : string
val resources_updated : string
val resources_templates_list : string

(** {2 Prompts} *)

val prompts_list : string
val prompts_get : string
val prompts_list_changed : string

(** {2 Logging} *)

val logging_set_level : string
val logging_message : string

(** {2 Sampling} *)

val sampling_create_message : string

(** {2 Roots} *)

val roots_list : string
val roots_list_changed : string

(** {2 Completion} *)

val completion_complete : string

(** {2 Progress & Cancellation} *)

val progress : string
val cancelled : string

(** {2 Ping} *)

val ping : string

(** {2 Elicitation} *)

val elicitation_create : string

(** {2 Utility} *)

val is_notification : string -> bool
