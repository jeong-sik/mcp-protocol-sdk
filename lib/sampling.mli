(** MCP Sampling types.

    Supports the sampling/createMessage request.
*)

(** Role in a sampling message — alias for [Mcp_types.role]. *)
type role = Mcp_types.role = User | Assistant

val role_to_yojson : role -> Yojson.Safe.t
val role_of_yojson : Yojson.Safe.t -> (role, string) result

(** {2 Content} *)

type sampling_content =
  | Text of { type_: string; text: string }
  | Image of { type_: string; data: string; mime_type: string }

val sampling_content_to_yojson : sampling_content -> Yojson.Safe.t
val sampling_content_of_yojson : Yojson.Safe.t -> (sampling_content, string) result

(** {2 Messages} *)

type sampling_message = {
  role: role;
  content: sampling_content;
}

val sampling_message_to_yojson : sampling_message -> Yojson.Safe.t
val sampling_message_of_yojson : Yojson.Safe.t -> (sampling_message, string) result

(** {2 Model Preferences} *)

type model_hint = {
  name: string option;
}

type model_preferences = {
  hints: model_hint list option;
  cost_priority: float option;
  speed_priority: float option;
  intelligence_priority: float option;
}

val model_hint_to_yojson : model_hint -> Yojson.Safe.t
val model_hint_of_yojson : Yojson.Safe.t -> (model_hint, string) result
val model_preferences_to_yojson : model_preferences -> Yojson.Safe.t
val model_preferences_of_yojson : Yojson.Safe.t -> (model_preferences, string) result

(** {2 Sampling Tools} *)

(** Tool choice for sampling: auto, none, or a specific tool. *)
type sampling_tool_choice =
  | Auto
  | None_
  | Tool of string

val sampling_tool_choice_to_yojson : sampling_tool_choice -> Yojson.Safe.t
val sampling_tool_choice_of_yojson : Yojson.Safe.t -> (sampling_tool_choice, string) result

(** Tool definition for sampling. *)
type sampling_tool = {
  name: string;
  description: string option;
  input_schema: Yojson.Safe.t;
}

val sampling_tool_to_yojson : sampling_tool -> Yojson.Safe.t
val sampling_tool_of_yojson : Yojson.Safe.t -> (sampling_tool, string) result

(** {2 Create Message} *)

type create_message_params = {
  messages: sampling_message list;
  model_preferences: model_preferences option;
  system_prompt: string option;
  include_context: string option;
  temperature: float option;
  max_tokens: int;
  stop_sequences: string list option;
  metadata: Yojson.Safe.t option;
  tools: sampling_tool list option;
  tool_choice: sampling_tool_choice option;
  _meta: Yojson.Safe.t option;
}

val create_message_params_to_yojson : create_message_params -> Yojson.Safe.t
val create_message_params_of_yojson : Yojson.Safe.t -> (create_message_params, string) result

type create_message_result = {
  role: role;
  content: sampling_content;
  model: string;
  stop_reason: string option;
  _meta: Yojson.Safe.t option;
}

val create_message_result_to_yojson : create_message_result -> Yojson.Safe.t
val create_message_result_of_yojson : Yojson.Safe.t -> (create_message_result, string) result
