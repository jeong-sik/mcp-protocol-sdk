open Mcp_protocol

(* --- method string constants --- *)

let test_lifecycle_methods () =
  Alcotest.(check string) "initialize" "initialize" Notifications.initialize;
  Alcotest.(check string) "initialized" "notifications/initialized" Notifications.initialized;
  Alcotest.(check string) "shutdown" "shutdown" Notifications.shutdown

let test_tool_methods () =
  Alcotest.(check string) "list" "tools/list" Notifications.tools_list;
  Alcotest.(check string) "call" "tools/call" Notifications.tools_call;
  Alcotest.(check string) "changed" "notifications/tools/list_changed" Notifications.tools_list_changed

let test_resource_methods () =
  Alcotest.(check string) "list" "resources/list" Notifications.resources_list;
  Alcotest.(check string) "read" "resources/read" Notifications.resources_read;
  Alcotest.(check string) "subscribe" "resources/subscribe" Notifications.resources_subscribe;
  Alcotest.(check string) "templates" "resources/templates/list" Notifications.resources_templates_list;
  Alcotest.(check string) "changed" "notifications/resources/list_changed" Notifications.resources_list_changed;
  Alcotest.(check string) "updated" "notifications/resources/updated" Notifications.resources_updated

let test_prompt_methods () =
  Alcotest.(check string) "list" "prompts/list" Notifications.prompts_list;
  Alcotest.(check string) "get" "prompts/get" Notifications.prompts_get;
  Alcotest.(check string) "changed" "notifications/prompts/list_changed" Notifications.prompts_list_changed

let test_logging_methods () =
  Alcotest.(check string) "setLevel" "logging/setLevel" Notifications.logging_set_level;
  Alcotest.(check string) "message" "notifications/message" Notifications.logging_message

let test_sampling_methods () =
  Alcotest.(check string) "createMessage" "sampling/createMessage" Notifications.sampling_create_message

let test_roots_methods () =
  Alcotest.(check string) "list" "roots/list" Notifications.roots_list;
  Alcotest.(check string) "changed" "notifications/roots/list_changed" Notifications.roots_list_changed

let test_completion_methods () =
  Alcotest.(check string) "complete" "completion/complete" Notifications.completion_complete

let test_progress_cancel () =
  Alcotest.(check string) "progress" "notifications/progress" Notifications.progress;
  Alcotest.(check string) "cancelled" "notifications/cancelled" Notifications.cancelled

let test_ping () =
  Alcotest.(check string) "ping" "ping" Notifications.ping

(* --- is_notification --- *)

let test_is_notification () =
  Alcotest.(check bool) "notifications/initialized"
    true (Notifications.is_notification "notifications/initialized");
  Alcotest.(check bool) "notifications/tools/list_changed"
    true (Notifications.is_notification "notifications/tools/list_changed");
  Alcotest.(check bool) "tools/list is not notification"
    false (Notifications.is_notification "tools/list");
  Alcotest.(check bool) "initialize is not notification"
    false (Notifications.is_notification "initialize");
  Alcotest.(check bool) "empty string"
    false (Notifications.is_notification "");
  Alcotest.(check bool) "short string"
    false (Notifications.is_notification "notif")

(* --- Suite --- *)

let () =
  Alcotest.run "Notifications" [
    "lifecycle", [
      Alcotest.test_case "methods" `Quick test_lifecycle_methods;
    ];
    "tools", [
      Alcotest.test_case "methods" `Quick test_tool_methods;
    ];
    "resources", [
      Alcotest.test_case "methods" `Quick test_resource_methods;
    ];
    "prompts", [
      Alcotest.test_case "methods" `Quick test_prompt_methods;
    ];
    "logging", [
      Alcotest.test_case "methods" `Quick test_logging_methods;
    ];
    "sampling", [
      Alcotest.test_case "methods" `Quick test_sampling_methods;
    ];
    "roots", [
      Alcotest.test_case "methods" `Quick test_roots_methods;
    ];
    "completion", [
      Alcotest.test_case "methods" `Quick test_completion_methods;
    ];
    "progress_cancel", [
      Alcotest.test_case "methods" `Quick test_progress_cancel;
    ];
    "ping", [
      Alcotest.test_case "method" `Quick test_ping;
    ];
    "is_notification", [
      Alcotest.test_case "predicate" `Quick test_is_notification;
    ];
  ]
