(** MCP Sampling types.

    Supports the sampling/createMessage request.

    Reference: https://modelcontextprotocol.io/docs/concepts/sampling
*)

(** Role in a sampling message — alias for [Mcp_types.role]. *)
type role = Mcp_types.role = User | Assistant

let role_to_yojson = Mcp_types.role_to_yojson

let role_of_yojson = Mcp_types.role_of_yojson

let ( let* ) = Result.bind

let expect_object what = function
  | `Assoc fields -> Ok fields
  | _ -> Error (what ^ " must be an object")

let required_field what fields key =
  match List.assoc_opt key fields with
  | Some value -> Ok value
  | None -> Error (Printf.sprintf "%s: missing '%s' field" what key)

let optional_field fields key = List.assoc_opt key fields

let parse_required what fields key parser =
  let* value = required_field what fields key in
  parser value |> Result.map_error (fun err ->
      Printf.sprintf "%s.%s: %s" what key err)

let parse_optional what fields key parser =
  match optional_field fields key with
  | None -> Ok None
  | Some value ->
      parser value
      |> Result.map (fun parsed -> Some parsed)
      |> Result.map_error (fun err ->
             Printf.sprintf "%s.%s: %s" what key err)

let parse_string what value =
  match value with
  | `String s -> Ok s
  | _ -> Error (what ^ " must be a string")

let parse_float what value =
  match value with
  | `Float f -> Ok f
  | `Int i -> Ok (float_of_int i)
  | _ -> Error (what ^ " must be a float")

let parse_int what value =
  match value with
  | `Int i -> Ok i
  | _ -> Error (what ^ " must be an int")

let parse_string_list what value =
  match value with
  | `List items ->
      List.mapi
        (fun idx item ->
          parse_string (Printf.sprintf "%s[%d]" what idx) item)
        items
      |> List.fold_left
           (fun acc item ->
             let* parsed = acc in
             let* value = item in
             Ok (value :: parsed))
           (Ok [])
      |> Result.map List.rev
  | _ -> Error (what ^ " must be an array")

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
  | json ->
    let* fields = expect_object "sampling_content" json in
    let* type_ = parse_required "sampling_content" fields "type" (parse_string "type") in
    match type_ with
    | "text" ->
        let* text =
          parse_required "sampling_content" fields "text" (parse_string "text")
        in
        Ok (Text { type_; text })
    | "image" ->
        let* data =
          parse_required "sampling_content" fields "data" (parse_string "data")
        in
        let* mime_type =
          parse_required "sampling_content" fields "mimeType" (parse_string "mimeType")
        in
        Ok (Image { type_; data; mime_type })
    | _ -> Error ("Unknown sampling content type: " ^ type_)

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
  | json ->
      let* fields = expect_object "sampling_message" json in
      let* role = parse_required "sampling_message" fields "role" role_of_yojson in
      let* content =
        parse_required "sampling_message" fields "content" sampling_content_of_yojson
      in
      Ok { role; content }

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
  | json ->
      let* fields = expect_object "model_preferences" json in
      let parse_hints value =
        match value with
        | `List items ->
            List.mapi
              (fun idx item ->
                model_hint_of_yojson item
                |> Result.map_error (fun err ->
                       Printf.sprintf "hints[%d]: %s" idx err))
              items
            |> List.fold_left
                 (fun acc item ->
                   let* parsed = acc in
                   let* hint = item in
                   Ok (hint :: parsed))
                 (Ok [])
            |> Result.map List.rev
        | _ -> Error "hints must be an array"
      in
      let* hints = parse_optional "model_preferences" fields "hints" parse_hints in
      let* cost_priority =
        parse_optional "model_preferences" fields "costPriority" (parse_float "costPriority")
      in
      let* speed_priority =
        parse_optional "model_preferences" fields "speedPriority" (parse_float "speedPriority")
      in
      let* intelligence_priority =
        parse_optional
          "model_preferences"
          fields
          "intelligencePriority"
          (parse_float "intelligencePriority")
      in
      Ok { hints; cost_priority; speed_priority; intelligence_priority }

(** Tool choice for sampling: auto, none, or a specific tool. *)
type sampling_tool_choice =
  | Auto
  | None_
  | Tool of string

let sampling_tool_choice_to_yojson = function
  | Auto -> `Assoc [("type", `String "auto")]
  | None_ -> `Assoc [("type", `String "none")]
  | Tool name -> `Assoc [("type", `String "tool"); ("name", `String name)]

let sampling_tool_choice_of_yojson = function
  | `Assoc fields ->
    (match List.assoc_opt "type" fields with
     | Some (`String "auto") -> Ok Auto
     | Some (`String "none") -> Ok None_
     | Some (`String "tool") ->
       (match List.assoc_opt "name" fields with
        | Some (`String name) -> Ok (Tool name)
        | _ -> Error "sampling_tool_choice: 'tool' type requires a 'name' field")
     | Some (`String other) -> Error ("sampling_tool_choice: unknown type: " ^ other)
     | _ -> Error "sampling_tool_choice: missing 'type' field")
  | _ -> Error "sampling_tool_choice must be an object"

(** Tool definition for sampling. *)
type sampling_tool = {
  name: string;
  description: string option;
  input_schema: Yojson.Safe.t;
}

let sampling_tool_to_yojson t =
  let fields = [
    ("name", `String t.name);
    ("inputSchema", t.input_schema);
  ] in
  let fields = match t.description with
    | Some d -> ("description", `String d) :: fields
    | None -> fields
  in
  `Assoc fields

let sampling_tool_of_yojson = function
  | json ->
      let* fields = expect_object "sampling_tool" json in
      let* name = parse_required "sampling_tool" fields "name" (parse_string "name") in
      let* description =
        parse_optional "sampling_tool" fields "description" (parse_string "description")
      in
      let input_schema =
        match optional_field fields "inputSchema" with
        | Some json -> json
        | None -> `Assoc [("type", `String "object")]
      in
      Ok { name; description; input_schema }

(** Which context the sampling host should include with the request. *)
type include_context = None_ | ThisServer | AllServers

let include_context_to_yojson = function
  | None_ -> `String "none"
  | ThisServer -> `String "thisServer"
  | AllServers -> `String "allServers"

let include_context_of_yojson = function
  | `String "none" -> Ok None_
  | `String "thisServer" -> Ok ThisServer
  | `String "allServers" -> Ok AllServers
  | `String s -> Error (Printf.sprintf "include_context: unknown value %S" s)
  | _ -> Error "include_context must be a string"

(** Parameters for sampling/createMessage request. *)
type create_message_params = {
  messages: sampling_message list;
  model_preferences: model_preferences option;
  system_prompt: string option;
  include_context: include_context option;
  temperature: float option;
  max_tokens: int;
  stop_sequences: string list option;
  metadata: Yojson.Safe.t option;
  tools: sampling_tool list option;
  tool_choice: sampling_tool_choice option;
  _meta: Yojson.Safe.t option;
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
    | Some ic -> ("includeContext", include_context_to_yojson ic) :: fields
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
  let fields = match p.tools with
    | Some ts -> ("tools", `List (List.map sampling_tool_to_yojson ts)) :: fields
    | None -> fields
  in
  let fields = match p.tool_choice with
    | Some tc -> ("toolChoice", sampling_tool_choice_to_yojson tc) :: fields
    | None -> fields
  in
  let fields = match p._meta with
    | Some j -> ("_meta", j) :: fields
    | None -> fields
  in
  `Assoc fields

let create_message_params_of_yojson = function
  | json ->
      let* fields = expect_object "create_message_params" json in
      let parse_messages value =
        match value with
        | `List items ->
            List.mapi
              (fun idx item ->
                sampling_message_of_yojson item
                |> Result.map_error (fun err ->
                       Printf.sprintf "messages[%d]: %s" idx err))
              items
            |> List.fold_left
                 (fun acc item ->
                   let* parsed = acc in
                   let* message = item in
                   Ok (message :: parsed))
                 (Ok [])
            |> Result.map List.rev
        | _ -> Error "messages must be an array"
      in
      let parse_tools value =
        match value with
        | `List items ->
            List.mapi
              (fun idx item ->
                sampling_tool_of_yojson item
                |> Result.map_error (fun err ->
                       Printf.sprintf "tools[%d]: %s" idx err))
              items
            |> List.fold_left
                 (fun acc item ->
                   let* parsed = acc in
                   let* tool = item in
                   Ok (tool :: parsed))
                 (Ok [])
            |> Result.map List.rev
        | _ -> Error "tools must be an array"
      in
      let* messages = parse_required "create_message_params" fields "messages" parse_messages in
      let* max_tokens =
        parse_required "create_message_params" fields "maxTokens" (parse_int "maxTokens")
      in
      let* model_preferences =
        parse_optional
          "create_message_params"
          fields
          "modelPreferences"
          model_preferences_of_yojson
      in
      let* system_prompt =
        parse_optional "create_message_params" fields "systemPrompt" (parse_string "systemPrompt")
      in
      let* include_context =
        parse_optional
          "create_message_params"
          fields
          "includeContext"
          include_context_of_yojson
      in
      let* temperature =
        parse_optional "create_message_params" fields "temperature" (parse_float "temperature")
      in
      let* stop_sequences =
        parse_optional
          "create_message_params"
          fields
          "stopSequences"
          (parse_string_list "stopSequences")
      in
      let* tools =
        parse_optional "create_message_params" fields "tools" parse_tools
      in
      let* tool_choice =
        parse_optional
          "create_message_params"
          fields
          "toolChoice"
          sampling_tool_choice_of_yojson
      in
      let _meta =
        match optional_field fields "_meta" with
        | Some `Null | None -> None
        | Some value -> Some value
      in
      Ok
        {
          messages;
          model_preferences;
          system_prompt;
          include_context;
          temperature;
          max_tokens;
          stop_sequences;
          metadata = optional_field fields "metadata";
          tools;
          tool_choice;
          _meta;
        }

(** Result for sampling/createMessage. *)
type create_message_result = {
  role: role;
  content: sampling_content;
  model: string;
  stop_reason: string option;
  _meta: Yojson.Safe.t option;
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
  let fields = match r._meta with
    | Some j -> ("_meta", j) :: fields
    | None -> fields
  in
  `Assoc fields

let create_message_result_of_yojson = function
  | json ->
      let* fields = expect_object "create_message_result" json in
      let* role = parse_required "create_message_result" fields "role" role_of_yojson in
      let* content =
        parse_required "create_message_result" fields "content" sampling_content_of_yojson
      in
      let* model =
        parse_required "create_message_result" fields "model" (parse_string "model")
      in
      let* stop_reason =
        parse_optional "create_message_result" fields "stopReason" (parse_string "stopReason")
      in
      let _meta =
        match optional_field fields "_meta" with
        | Some `Null | None -> None
        | Some value -> Some value
      in
      Ok { role; content; model; stop_reason; _meta }
