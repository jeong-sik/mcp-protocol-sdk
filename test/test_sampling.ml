open Mcp_protocol

let json = Alcotest.testable
  (fun fmt j -> Format.pp_print_string fmt (Yojson.Safe.to_string j))
  (fun a b -> Yojson.Safe.equal a b)

(* --- role --- *)

let test_role_roundtrip () =
  let roles = [Sampling.User; Assistant] in
  List.iter (fun role ->
    let j = Sampling.role_to_yojson role in
    Alcotest.(check (result pass string)) "roundtrip"
      (Ok role) (Sampling.role_of_yojson j)
  ) roles

let test_role_invalid () =
  Alcotest.(check bool) "invalid"
    true (Result.is_error (Sampling.role_of_yojson (`String "system")))

(* --- sampling_content --- *)

let test_text_content_roundtrip () =
  let c = Sampling.Text { type_ = "text"; text = "hello" } in
  let j = Sampling.sampling_content_to_yojson c in
  match Sampling.sampling_content_of_yojson j with
  | Ok (Text { text; _ }) ->
    Alcotest.(check string) "text" "hello" text
  | Ok _ -> Alcotest.fail "Wrong content type"
  | Error e -> Alcotest.fail e

let test_image_content_roundtrip () =
  let c = Sampling.Image { type_ = "image"; data = "base64data"; mime_type = "image/png" } in
  let j = Sampling.sampling_content_to_yojson c in
  match Sampling.sampling_content_of_yojson j with
  | Ok (Image { data; mime_type; _ }) ->
    Alcotest.(check string) "data" "base64data" data;
    Alcotest.(check string) "mime_type" "image/png" mime_type
  | Ok _ -> Alcotest.fail "Wrong content type"
  | Error e -> Alcotest.fail e

let test_content_unknown_type () =
  let j = `Assoc [("type", `String "video"); ("url", `String "...")] in
  Alcotest.(check bool) "unknown type"
    true (Result.is_error (Sampling.sampling_content_of_yojson j))

(* --- sampling_message --- *)

let test_sampling_message_roundtrip () =
  let msg : Sampling.sampling_message = {
    role = User;
    content = Text { type_ = "text"; text = "What is MCP?" };
  } in
  let j = Sampling.sampling_message_to_yojson msg in
  match Sampling.sampling_message_of_yojson j with
  | Ok msg' ->
    Alcotest.(check (result pass string)) "role"
      (Ok Sampling.User) (Sampling.role_of_yojson (Sampling.role_to_yojson msg'.role))
  | Error e -> Alcotest.fail e

(* --- model_preferences --- *)

let test_model_preferences_full () =
  let p : Sampling.model_preferences = {
    hints = Some [{ name = Some "claude-3" }; { name = None }];
    cost_priority = Some 0.3;
    speed_priority = Some 0.5;
    intelligence_priority = Some 0.9;
  } in
  let j = Sampling.model_preferences_to_yojson p in
  match Sampling.model_preferences_of_yojson j with
  | Ok p' ->
    Alcotest.(check bool) "has hints" true (Option.is_some p'.hints);
    Alcotest.(check int) "hint count" 2 (List.length (Option.get p'.hints));
    Alcotest.(check (float 0.01)) "cost" 0.3 (Option.get p'.cost_priority);
    Alcotest.(check (float 0.01)) "speed" 0.5 (Option.get p'.speed_priority);
    Alcotest.(check (float 0.01)) "intelligence" 0.9 (Option.get p'.intelligence_priority)
  | Error e -> Alcotest.fail e

let test_model_preferences_minimal () =
  let p : Sampling.model_preferences = {
    hints = None;
    cost_priority = None;
    speed_priority = None;
    intelligence_priority = None;
  } in
  let j = Sampling.model_preferences_to_yojson p in
  match Sampling.model_preferences_of_yojson j with
  | Ok p' ->
    Alcotest.(check bool) "no hints" true (Option.is_none p'.hints);
    Alcotest.(check bool) "no cost" true (Option.is_none p'.cost_priority)
  | Error e -> Alcotest.fail e

(* --- create_message_params --- *)

let test_create_message_params_roundtrip () =
  let params : Sampling.create_message_params = {
    messages = [
      { role = User; content = Text { type_ = "text"; text = "Hello" } };
    ];
    model_preferences = None;
    system_prompt = Some "You are helpful.";
    include_context = Some "thisServer";
    temperature = Some 0.7;
    max_tokens = 1024;
    stop_sequences = Some ["END"];
    metadata = Some (`Assoc [("key", `String "val")]);
    tools = None;
    tool_choice = None;
    _meta = None;
  } in
  let j = Sampling.create_message_params_to_yojson params in
  match Sampling.create_message_params_of_yojson j with
  | Ok p' ->
    Alcotest.(check int) "messages" 1 (List.length p'.messages);
    Alcotest.(check (option string)) "system_prompt" (Some "You are helpful.") p'.system_prompt;
    Alcotest.(check (option string)) "include_context" (Some "thisServer") p'.include_context;
    Alcotest.(check int) "max_tokens" 1024 p'.max_tokens;
    Alcotest.(check (option (float 0.01))) "temperature" (Some 0.7) p'.temperature
  | Error e -> Alcotest.fail e

let test_create_message_params_minimal () =
  let params : Sampling.create_message_params = {
    messages = [];
    model_preferences = None;
    system_prompt = None;
    include_context = None;
    temperature = None;
    max_tokens = 100;
    stop_sequences = None;
    metadata = None;
    tools = None;
    tool_choice = None;
    _meta = None;
  } in
  let j = Sampling.create_message_params_to_yojson params in
  match Sampling.create_message_params_of_yojson j with
  | Ok p' ->
    Alcotest.(check int) "empty messages" 0 (List.length p'.messages);
    Alcotest.(check int) "max_tokens" 100 p'.max_tokens;
    Alcotest.(check bool) "no system" true (Option.is_none p'.system_prompt)
  | Error e -> Alcotest.fail e

(* --- create_message_result --- *)

let test_create_message_result_roundtrip () =
  let result : Sampling.create_message_result = {
    role = Assistant;
    content = Text { type_ = "text"; text = "MCP stands for..." };
    model = "claude-3-opus";
    stop_reason = Some "end_turn";
    _meta = None;
  } in
  let j = Sampling.create_message_result_to_yojson result in
  match Sampling.create_message_result_of_yojson j with
  | Ok r' ->
    Alcotest.(check string) "model" "claude-3-opus" r'.model;
    Alcotest.(check (option string)) "stop_reason" (Some "end_turn") r'.stop_reason
  | Error e -> Alcotest.fail e

let test_create_message_result_no_stop () =
  let result : Sampling.create_message_result = {
    role = Assistant;
    content = Text { type_ = "text"; text = "..." };
    model = "gpt-4";
    stop_reason = None;
    _meta = None;
  } in
  let j = Sampling.create_message_result_to_yojson result in
  let open Yojson.Safe.Util in
  Alcotest.(check json) "no stopReason" `Null (member "stopReason" j)

(* --- sampling_tool_choice --- *)

let test_sampling_tool_choice_auto () =
  let j = Sampling.sampling_tool_choice_to_yojson Auto in
  match Sampling.sampling_tool_choice_of_yojson j with
  | Ok Auto -> ()
  | Ok _ -> Alcotest.fail "expected Auto"
  | Error e -> Alcotest.fail e

let test_sampling_tool_choice_none () =
  let j = Sampling.sampling_tool_choice_to_yojson None_ in
  match Sampling.sampling_tool_choice_of_yojson j with
  | Ok None_ -> ()
  | Ok _ -> Alcotest.fail "expected None_"
  | Error e -> Alcotest.fail e

let test_sampling_tool_choice_tool () =
  let j = Sampling.sampling_tool_choice_to_yojson (Tool "get_weather") in
  match Sampling.sampling_tool_choice_of_yojson j with
  | Ok (Tool name) ->
    Alcotest.(check string) "tool name" "get_weather" name
  | Ok _ -> Alcotest.fail "expected Tool"
  | Error e -> Alcotest.fail e

let test_sampling_tool_choice_invalid () =
  let j = `Assoc [("type", `String "unknown")] in
  Alcotest.(check bool) "invalid type"
    true (Result.is_error (Sampling.sampling_tool_choice_of_yojson j))

(* --- sampling_tool --- *)

let test_sampling_tool_roundtrip () =
  let t : Sampling.sampling_tool = {
    name = "get_weather";
    description = Some "Get current weather";
    input_schema = `Assoc [("type", `String "object")];
  } in
  let j = Sampling.sampling_tool_to_yojson t in
  match Sampling.sampling_tool_of_yojson j with
  | Ok t' ->
    Alcotest.(check string) "name" "get_weather" t'.name;
    Alcotest.(check (option string)) "description" (Some "Get current weather") t'.description
  | Error e -> Alcotest.fail e

let test_sampling_tool_no_description () =
  let t : Sampling.sampling_tool = {
    name = "echo";
    description = None;
    input_schema = `Assoc [("type", `String "object")];
  } in
  let j = Sampling.sampling_tool_to_yojson t in
  match Sampling.sampling_tool_of_yojson j with
  | Ok t' ->
    Alcotest.(check string) "name" "echo" t'.name;
    Alcotest.(check bool) "no description" true (Option.is_none t'.description)
  | Error e -> Alcotest.fail e

(* --- Sampling tool calling (SEP-1577) --- *)

let test_create_message_params_with_tools () =
  let tool_def : Sampling.sampling_tool = {
    name = "get_weather";
    description = None;
    input_schema = `Assoc [("type", `String "object")];
  } in
  let params : Sampling.create_message_params = {
    messages = [
      { role = User; content = Text { type_ = "text"; text = "Weather?" } };
    ];
    model_preferences = None;
    system_prompt = None;
    include_context = None;
    temperature = None;
    max_tokens = 512;
    stop_sequences = None;
    metadata = None;
    tools = Some [tool_def];
    tool_choice = Some Auto;
    _meta = None;
  } in
  let j = Sampling.create_message_params_to_yojson params in
  match Sampling.create_message_params_of_yojson j with
  | Ok p' ->
    Alcotest.(check bool) "tools present" true (Option.is_some p'.tools);
    Alcotest.(check int) "tool count" 1 (List.length (Option.get p'.tools));
    Alcotest.(check string) "tool name" "get_weather" (List.hd (Option.get p'.tools)).name;
    Alcotest.(check bool) "tool_choice present" true (Option.is_some p'.tool_choice);
    (match p'.tool_choice with
     | Some Auto -> ()
     | _ -> Alcotest.fail "expected Auto tool_choice")
  | Error e -> Alcotest.fail e

let test_create_message_params_no_tools () =
  let params : Sampling.create_message_params = {
    messages = [];
    model_preferences = None;
    system_prompt = None;
    include_context = None;
    temperature = None;
    max_tokens = 100;
    stop_sequences = None;
    metadata = None;
    tools = None;
    tool_choice = None;
    _meta = None;
  } in
  let j = Sampling.create_message_params_to_yojson params in
  let open Yojson.Safe.Util in
  (* tools and toolChoice should not appear in JSON when None *)
  (match j with
   | `Assoc fields ->
     Alcotest.(check bool) "no tools key" false (List.mem_assoc "tools" fields);
     Alcotest.(check bool) "no toolChoice key" false (List.mem_assoc "toolChoice" fields)
   | _ -> Alcotest.fail "expected object");
  ignore (member "maxTokens" j)

(* --- Suite --- *)

let () =
  Alcotest.run "Sampling" [
    "role", [
      Alcotest.test_case "roundtrip" `Quick test_role_roundtrip;
      Alcotest.test_case "invalid" `Quick test_role_invalid;
    ];
    "sampling_content", [
      Alcotest.test_case "text" `Quick test_text_content_roundtrip;
      Alcotest.test_case "image" `Quick test_image_content_roundtrip;
      Alcotest.test_case "unknown type" `Quick test_content_unknown_type;
    ];
    "sampling_message", [
      Alcotest.test_case "roundtrip" `Quick test_sampling_message_roundtrip;
    ];
    "model_preferences", [
      Alcotest.test_case "full" `Quick test_model_preferences_full;
      Alcotest.test_case "minimal" `Quick test_model_preferences_minimal;
    ];
    "sampling_tool_choice", [
      Alcotest.test_case "auto" `Quick test_sampling_tool_choice_auto;
      Alcotest.test_case "none" `Quick test_sampling_tool_choice_none;
      Alcotest.test_case "tool" `Quick test_sampling_tool_choice_tool;
      Alcotest.test_case "invalid" `Quick test_sampling_tool_choice_invalid;
    ];
    "sampling_tool", [
      Alcotest.test_case "roundtrip" `Quick test_sampling_tool_roundtrip;
      Alcotest.test_case "no description" `Quick test_sampling_tool_no_description;
    ];
    "create_message_params", [
      Alcotest.test_case "roundtrip" `Quick test_create_message_params_roundtrip;
      Alcotest.test_case "minimal" `Quick test_create_message_params_minimal;
      Alcotest.test_case "with tools" `Quick test_create_message_params_with_tools;
      Alcotest.test_case "no tools omitted" `Quick test_create_message_params_no_tools;
    ];
    "create_message_result", [
      Alcotest.test_case "roundtrip" `Quick test_create_message_result_roundtrip;
      Alcotest.test_case "no stop reason" `Quick test_create_message_result_no_stop;
    ];
  ]
