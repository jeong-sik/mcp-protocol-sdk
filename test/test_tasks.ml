open Mcp_protocol

let json = Alcotest.testable
  (fun fmt j -> Format.pp_print_string fmt (Yojson.Safe.to_string j))
  (fun a b -> Yojson.Safe.equal a b)

(* --- task_status --- *)

let test_task_status_roundtrip () =
  let cases = [
    (Mcp_types.Working, "working");
    (Mcp_types.Input_required, "input_required");
    (Mcp_types.Completed, "completed");
    (Mcp_types.Failed, "failed");
    (Mcp_types.Cancelled, "cancelled");
  ] in
  List.iter (fun (status, expected_str) ->
    let j = Mcp_types.task_status_to_yojson status in
    Alcotest.(check json) ("serialize " ^ expected_str)
      (`String expected_str) j;
    match Mcp_types.task_status_of_yojson j with
    | Ok decoded ->
      Alcotest.(check json) ("roundtrip " ^ expected_str)
        j (Mcp_types.task_status_to_yojson decoded)
    | Error e -> Alcotest.fail e
  ) cases

let test_task_status_invalid () =
  match Mcp_types.task_status_of_yojson (`String "unknown_status") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for invalid status"

let test_task_status_is_terminal () =
  Alcotest.(check bool) "working" false
    (Mcp_types.task_status_is_terminal Working);
  Alcotest.(check bool) "input_required" false
    (Mcp_types.task_status_is_terminal Input_required);
  Alcotest.(check bool) "completed" true
    (Mcp_types.task_status_is_terminal Completed);
  Alcotest.(check bool) "failed" true
    (Mcp_types.task_status_is_terminal Failed);
  Alcotest.(check bool) "cancelled" true
    (Mcp_types.task_status_is_terminal Cancelled)

(* --- task --- *)

let test_task_roundtrip () =
  let task : Mcp_types.task = {
    task_id = "task-001";
    status = Working;
    status_message = Some "Processing step 1";
    created_at = "2025-11-25T10:00:00Z";
    last_updated_at = "2025-11-25T10:00:05Z";
    ttl = Some 60000;
    poll_interval = Some 5000;
  } in
  let j = Mcp_types.task_to_yojson task in
  (* Verify camelCase JSON keys *)
  (match j with
   | `Assoc fields ->
     Alcotest.(check bool) "has taskId"
       true (List.mem_assoc "taskId" fields);
     Alcotest.(check bool) "has createdAt"
       true (List.mem_assoc "createdAt" fields);
     Alcotest.(check bool) "has lastUpdatedAt"
       true (List.mem_assoc "lastUpdatedAt" fields);
     Alcotest.(check bool) "has statusMessage"
       true (List.mem_assoc "statusMessage" fields);
     Alcotest.(check bool) "has pollInterval"
       true (List.mem_assoc "pollInterval" fields);
   | _ -> Alcotest.fail "expected Assoc");
  match Mcp_types.task_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "task_id" "task-001" decoded.task_id;
    Alcotest.(check json) "status" (`String "working")
      (Mcp_types.task_status_to_yojson decoded.status);
    Alcotest.(check (option string)) "status_message"
      (Some "Processing step 1") decoded.status_message;
    Alcotest.(check string) "created_at"
      "2025-11-25T10:00:00Z" decoded.created_at;
    Alcotest.(check string) "last_updated_at"
      "2025-11-25T10:00:05Z" decoded.last_updated_at;
    Alcotest.(check (option int)) "ttl" (Some 60000) decoded.ttl;
    Alcotest.(check (option int)) "poll_interval" (Some 5000) decoded.poll_interval
  | Error e -> Alcotest.fail e

let test_task_minimal () =
  let task : Mcp_types.task = {
    task_id = "task-002";
    status = Completed;
    status_message = None;
    created_at = "2025-11-25T10:00:00Z";
    last_updated_at = "2025-11-25T10:01:00Z";
    ttl = None;
    poll_interval = None;
  } in
  let j = Mcp_types.task_to_yojson task in
  match Mcp_types.task_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "task_id" "task-002" decoded.task_id;
    Alcotest.(check (option string)) "no message" None decoded.status_message;
    Alcotest.(check (option int)) "no ttl" None decoded.ttl;
    Alcotest.(check (option int)) "no poll_interval" None decoded.poll_interval
  | Error e -> Alcotest.fail e

let test_task_missing_required () =
  let j = `Assoc [("status", `String "working")] in
  match Mcp_types.task_of_yojson j with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for missing taskId"

(* --- task_params --- *)

let test_task_params_roundtrip () =
  let p = Mcp_types.{ ttl = Some 30000 } in
  let j = Mcp_types.task_params_to_yojson p in
  match Mcp_types.task_params_of_yojson j with
  | Ok decoded -> Alcotest.(check (option int)) "ttl" (Some 30000) decoded.ttl
  | Error e -> Alcotest.fail e

let test_task_params_empty () =
  let p = Mcp_types.{ ttl = None } in
  let j = Mcp_types.task_params_to_yojson p in
  Alcotest.(check json) "empty object" (`Assoc []) j;
  match Mcp_types.task_params_of_yojson j with
  | Ok decoded -> Alcotest.(check (option int)) "no ttl" None decoded.ttl
  | Error e -> Alcotest.fail e

(* --- create_task_result --- *)

let test_create_task_result_roundtrip () =
  let task : Mcp_types.task = {
    task_id = "task-003";
    status = Working;
    status_message = None;
    created_at = "2025-11-25T12:00:00Z";
    last_updated_at = "2025-11-25T12:00:00Z";
    ttl = None;
    poll_interval = Some 2000;
  } in
  let result = Mcp_types.{ task } in
  let j = Mcp_types.create_task_result_to_yojson result in
  (* Verify envelope structure *)
  (match j with
   | `Assoc fields ->
     Alcotest.(check bool) "has task field" true (List.mem_assoc "task" fields)
   | _ -> Alcotest.fail "expected Assoc");
  match Mcp_types.create_task_result_of_yojson j with
  | Ok decoded ->
    Alcotest.(check string) "task_id" "task-003" decoded.task.task_id
  | Error e -> Alcotest.fail e

(* --- task_execution_support --- *)

let test_task_execution_support_roundtrip () =
  let cases = [
    (Mcp_types.Task_required, "required");
    (Mcp_types.Task_optional, "optional");
    (Mcp_types.Task_forbidden, "forbidden");
  ] in
  List.iter (fun (support, expected_str) ->
    let j = Mcp_types.task_execution_support_to_yojson support in
    Alcotest.(check json) ("serialize " ^ expected_str)
      (`String expected_str) j;
    match Mcp_types.task_execution_support_of_yojson j with
    | Ok decoded ->
      Alcotest.(check json) ("roundtrip " ^ expected_str)
        j (Mcp_types.task_execution_support_to_yojson decoded)
    | Error e -> Alcotest.fail e
  ) cases

(* --- make_task --- *)

let test_make_task () =
  let task = Mcp_types.make_task
    ~task_id:"task-new"
    ~created_at:"2025-11-25T15:00:00Z"
    ~status_message:"Starting"
    ~poll_interval:1000
    ()
  in
  Alcotest.(check string) "task_id" "task-new" task.task_id;
  Alcotest.(check json) "status is working"
    (`String "working") (Mcp_types.task_status_to_yojson task.status);
  Alcotest.(check (option string)) "status_message"
    (Some "Starting") task.status_message;
  Alcotest.(check string) "created = updated"
    task.created_at task.last_updated_at;
  Alcotest.(check (option int)) "no ttl" None task.ttl;
  Alcotest.(check (option int)) "poll_interval" (Some 1000) task.poll_interval

(* --- notifications method strings --- *)

let test_task_methods () =
  Alcotest.(check string) "tasks_get" "tasks/get" Notifications.tasks_get;
  Alcotest.(check string) "tasks_result" "tasks/result" Notifications.tasks_result;
  Alcotest.(check string) "tasks_list" "tasks/list" Notifications.tasks_list;
  Alcotest.(check string) "tasks_cancel" "tasks/cancel" Notifications.tasks_cancel;
  Alcotest.(check string) "tasks_status"
    "notifications/tasks/status" Notifications.tasks_status

let test_tasks_status_is_notification () =
  Alcotest.(check bool) "tasks_status is notification"
    true (Notifications.is_notification Notifications.tasks_status);
  Alcotest.(check bool) "tasks_get is not notification"
    false (Notifications.is_notification Notifications.tasks_get)

(* --- version_features has_tasks --- *)

let test_version_features_has_tasks () =
  let f_base = Version.features_of_version "2024-11-05" in
  Alcotest.(check bool) "base no tasks" false f_base.has_tasks;
  let f_mar = Version.features_of_version "2025-03-26" in
  Alcotest.(check bool) "2025-03-26 no tasks" false f_mar.has_tasks;
  let f_latest = Version.features_of_version "2025-11-25" in
  Alcotest.(check bool) "2025-11-25 has tasks" true f_latest.has_tasks

(* --- Suite --- *)

let () =
  Alcotest.run "Tasks" [
    "task_status", [
      Alcotest.test_case "roundtrip" `Quick test_task_status_roundtrip;
      Alcotest.test_case "invalid" `Quick test_task_status_invalid;
      Alcotest.test_case "is_terminal" `Quick test_task_status_is_terminal;
    ];
    "task", [
      Alcotest.test_case "roundtrip" `Quick test_task_roundtrip;
      Alcotest.test_case "minimal" `Quick test_task_minimal;
      Alcotest.test_case "missing required" `Quick test_task_missing_required;
    ];
    "task_params", [
      Alcotest.test_case "roundtrip" `Quick test_task_params_roundtrip;
      Alcotest.test_case "empty" `Quick test_task_params_empty;
    ];
    "create_task_result", [
      Alcotest.test_case "roundtrip" `Quick test_create_task_result_roundtrip;
    ];
    "task_execution_support", [
      Alcotest.test_case "roundtrip" `Quick test_task_execution_support_roundtrip;
    ];
    "make_task", [
      Alcotest.test_case "constructor" `Quick test_make_task;
    ];
    "notifications", [
      Alcotest.test_case "method strings" `Quick test_task_methods;
      Alcotest.test_case "is_notification" `Quick test_tasks_status_is_notification;
    ];
    "version_features", [
      Alcotest.test_case "has_tasks" `Quick test_version_features_has_tasks;
    ];
  ]
