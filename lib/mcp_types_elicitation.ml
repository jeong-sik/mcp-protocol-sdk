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
