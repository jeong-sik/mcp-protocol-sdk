(** {2 Elicitation} *)

type elicitation_schema = {
  type_: string;
  properties: (string * Yojson.Safe.t) list;
  required: string list option;
}

val elicitation_schema_to_yojson : elicitation_schema -> Yojson.Safe.t
val elicitation_schema_of_yojson : Yojson.Safe.t -> (elicitation_schema, string) result

(** Elicitation mode — determines how user input is collected.
    Form: server provides schema, client renders form.
    Url: server provides URL, client opens it for the user. *)
type elicitation_mode = Form | Url

val elicitation_mode_to_yojson : elicitation_mode -> Yojson.Safe.t
val elicitation_mode_of_yojson : Yojson.Safe.t -> (elicitation_mode, string) result

type elicitation_params = {
  message: string;
  requested_schema: elicitation_schema option;
  mode: elicitation_mode option;
  url: string option;
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
