(** MCP Sampling types.

    Supports the sampling/createMessage request.

    Reference: https://modelcontextprotocol.io/docs/concepts/sampling
*)

(** Role in a sampling message. *)
type role = User | Assistant

let role_to_yojson = function
  | User -> `String "user"
  | Assistant -> `String "assistant"

let role_of_yojson = function
  | `String "user" -> Ok User
  | `String "assistant" -> Ok Assistant
  | _ -> Error "Invalid sampling role"

(** Content types for sampling messages. *)
type sampling_content =
  | Text of { type_: string; text: string }
  | Image of { type_: string; data: string; mime_type: string }

let sampling_content_to_yojson = function
  | Text { type_; text } ->
    `Assoc [("type", `String type_); ("text", `String text)]
  | Image { type_; data; mime_type } ->
    `Assoc [("type", `String type_); ("data", `String data); ("mimeType", `String mime_type)]

let sampling_content_of_yojson = function
  | `Assoc fields ->
    let open Yojson.Safe.Util in
    let type_ = List.assoc "type" fields |> to_string in
    (match type_ with
     | "text" ->
       let text = List.assoc "text" fields |> to_string in
       Ok (Text { type_; text })
     | "image" ->
       let data = List.assoc "data" fields |> to_string in
       let mime_type = List.assoc "mimeType" fields |> to_string in
       Ok (Image { type_; data; mime_type })
     | _ -> Error ("Unknown sampling content type: " ^ type_))
  | _ -> Error "sampling_content must be an object"

(** A message in the sampling conversation. *)
type sampling_message = {
  role: role;
  content: sampling_content;
}

let sampling_message_to_yojson msg =
  `Assoc [
    ("role", role_to_yojson msg.role);
    ("content", sampling_content_to_yojson msg.content);
  ]

let sampling_message_of_yojson = function
  | `Assoc fields ->
    (match role_of_yojson (List.assoc "role" fields),
           sampling_content_of_yojson (List.assoc "content" fields)
     with
     | Ok role, Ok content -> Ok { role; content }
     | Error e, _ | _, Error e -> Error e)
  | _ -> Error "sampling_message must be an object"

(** Model preferences for sampling. *)
type model_preferences = {
  hints: model_hint list option;
  cost_priority: float option;
  speed_priority: float option;
  intelligence_priority: float option;
}
and model_hint = {
  name: string option;
}

let model_hint_to_yojson h =
  let fields = match h.name with
    | Some n -> [("name", `String n)]
    | None -> []
  in
  `Assoc fields

let model_hint_of_yojson = function
  | `Assoc fields ->
    let name = match List.assoc_opt "name" fields with
      | Some (`String s) -> Some s
      | _ -> None
    in
    Ok { name }
  | _ -> Error "model_hint must be an object"

let model_preferences_to_yojson p =
  let fields = [] in
  let fields = match p.intelligence_priority with
    | Some v -> ("intelligencePriority", `Float v) :: fields
    | None -> fields
  in
  let fields = match p.speed_priority with
    | Some v -> ("speedPriority", `Float v) :: fields
    | None -> fields
  in
  let fields = match p.cost_priority with
    | Some v -> ("costPriority", `Float v) :: fields
    | None -> fields
  in
  let fields = match p.hints with
    | Some hints -> ("hints", `List (List.map model_hint_to_yojson hints)) :: fields
    | None -> fields
  in
  `Assoc fields

let model_preferences_of_yojson = function
  | `Assoc fields ->
    let hints = match List.assoc_opt "hints" fields with
      | Some (`List l) ->
        let parsed = List.filter_map (fun j ->
          match model_hint_of_yojson j with
          | Ok h -> Some h
          | Error _ -> None
        ) l in
        Some parsed
      | _ -> None
    in
    let float_opt key = match List.assoc_opt key fields with
      | Some (`Float f) -> Some f
      | Some (`Int i) -> Some (float_of_int i)
      | _ -> None
    in
    Ok {
      hints;
      cost_priority = float_opt "costPriority";
      speed_priority = float_opt "speedPriority";
      intelligence_priority = float_opt "intelligencePriority";
    }
  | _ -> Error "model_preferences must be an object"

(** Parameters for sampling/createMessage request. *)
type create_message_params = {
  messages: sampling_message list;
  model_preferences: model_preferences option;
  system_prompt: string option;
  include_context: string option;  (** "none" | "thisServer" | "allServers" *)
  temperature: float option;
  max_tokens: int;
  stop_sequences: string list option;
  metadata: Yojson.Safe.t option;
}

let create_message_params_to_yojson p =
  let fields = [
    ("messages", `List (List.map sampling_message_to_yojson p.messages));
    ("maxTokens", `Int p.max_tokens);
  ] in
  let fields = match p.metadata with
    | Some m -> ("metadata", m) :: fields
    | None -> fields
  in
  let fields = match p.stop_sequences with
    | Some seqs -> ("stopSequences", `List (List.map (fun s -> `String s) seqs)) :: fields
    | None -> fields
  in
  let fields = match p.temperature with
    | Some t -> ("temperature", `Float t) :: fields
    | None -> fields
  in
  let fields = match p.include_context with
    | Some ic -> ("includeContext", `String ic) :: fields
    | None -> fields
  in
  let fields = match p.system_prompt with
    | Some sp -> ("systemPrompt", `String sp) :: fields
    | None -> fields
  in
  let fields = match p.model_preferences with
    | Some mp -> ("modelPreferences", model_preferences_to_yojson mp) :: fields
    | None -> fields
  in
  `Assoc fields

let create_message_params_of_yojson = function
  | `Assoc fields ->
    let open Yojson.Safe.Util in
    let messages_json = List.assoc "messages" fields |> to_list in
    let messages = List.fold_left (fun acc j ->
      match acc, sampling_message_of_yojson j with
      | Ok acc, Ok msg -> Ok (msg :: acc)
      | Error e, _ | _, Error e -> Error e
    ) (Ok []) messages_json in
    (match messages with
     | Ok msgs ->
       let msgs = List.rev msgs in
       let max_tokens = List.assoc "maxTokens" fields |> to_int in
       Ok {
         messages = msgs;
         model_preferences = (match List.assoc_opt "modelPreferences" fields with
           | Some j -> (match model_preferences_of_yojson j with Ok p -> Some p | Error _ -> None)
           | None -> None);
         system_prompt = (match List.assoc_opt "systemPrompt" fields with
           | Some (`String s) -> Some s | _ -> None);
         include_context = (match List.assoc_opt "includeContext" fields with
           | Some (`String s) -> Some s | _ -> None);
         temperature = (match List.assoc_opt "temperature" fields with
           | Some (`Float f) -> Some f | Some (`Int i) -> Some (float_of_int i) | _ -> None);
         max_tokens;
         stop_sequences = (match List.assoc_opt "stopSequences" fields with
           | Some (`List l) -> Some (List.map to_string l) | _ -> None);
         metadata = List.assoc_opt "metadata" fields;
       }
     | Error e -> Error e)
  | _ -> Error "create_message_params must be an object"

(** Result for sampling/createMessage. *)
type create_message_result = {
  role: role;
  content: sampling_content;
  model: string;
  stop_reason: string option;
}

let create_message_result_to_yojson r =
  let fields = [
    ("role", role_to_yojson r.role);
    ("content", sampling_content_to_yojson r.content);
    ("model", `String r.model);
  ] in
  let fields = match r.stop_reason with
    | Some sr -> ("stopReason", `String sr) :: fields
    | None -> fields
  in
  `Assoc fields

let create_message_result_of_yojson = function
  | `Assoc fields ->
    (match role_of_yojson (List.assoc "role" fields),
           sampling_content_of_yojson (List.assoc "content" fields)
     with
     | Ok role, Ok content ->
       let open Yojson.Safe.Util in
       let model = List.assoc "model" fields |> to_string in
       let stop_reason = match List.assoc_opt "stopReason" fields with
         | Some (`String s) -> Some s
         | _ -> None
       in
       Ok { role; content; model; stop_reason }
     | Error e, _ | _, Error e -> Error e)
  | _ -> Error "create_message_result must be an object"
