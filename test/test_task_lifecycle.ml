open Mcp_protocol
open Mcp_protocol_eio

(* --- Task_store.create_task --- *)

let test_create_task () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  Alcotest.(check string) "auto id" "task-0" t.task_id;
  Alcotest.(check bool) "working"
    true (t.status = Mcp_types.Working);
  Alcotest.(check string) "created_at" "2025-01-01T00:00:00Z" t.created_at;
  Alcotest.(check string) "updated = created" t.created_at t.last_updated_at;
  Alcotest.(check int) "count" 1 (Task_store.count store)

let test_create_task_custom_id () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z"
    ~task_id:"custom-42" ~ttl:60000 ~poll_interval:5000
    ~status_message:"initializing" () in
  Alcotest.(check string) "custom id" "custom-42" t.task_id;
  Alcotest.(check (option int)) "ttl" (Some 60000) t.ttl;
  Alcotest.(check (option int)) "poll_interval" (Some 5000) t.poll_interval;
  Alcotest.(check (option string)) "message" (Some "initializing") t.status_message

let test_create_auto_increment () =
  let store = Task_store.create () in
  let t0 = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  let t1 = Task_store.create_task store ~created_at:"2025-01-01T00:00:01Z" () in
  Alcotest.(check string) "first" "task-0" t0.task_id;
  Alcotest.(check string) "second" "task-1" t1.task_id;
  Alcotest.(check int) "count" 2 (Task_store.count store)

(* --- Task_store.get --- *)

let test_get_existing () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  match Task_store.get store t.task_id with
  | Some found -> Alcotest.(check string) "same id" t.task_id found.task_id
  | None -> Alcotest.fail "task should exist"

let test_get_missing () =
  let store = Task_store.create () in
  match Task_store.get store "nonexistent" with
  | None -> ()
  | Some _ -> Alcotest.fail "should be None"

(* --- Task_store.update_status --- *)

let test_update_status_valid () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  match Task_store.update_status store t.task_id Mcp_types.Completed
    ~updated_at:"2025-01-01T00:01:00Z" () with
  | Ok t' ->
    Alcotest.(check bool) "completed" true (t'.status = Completed);
    Alcotest.(check string) "updated_at" "2025-01-01T00:01:00Z" t'.last_updated_at;
    (* Verify store is updated too *)
    (match Task_store.get store t.task_id with
     | Some stored -> Alcotest.(check bool) "stored completed" true (stored.status = Completed)
     | None -> Alcotest.fail "task disappeared")
  | Error e -> Alcotest.fail e

let test_update_status_with_message () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  match Task_store.update_status store t.task_id Mcp_types.Working
    ~updated_at:"2025-01-01T00:00:30Z" ~message:"step 2 of 5" () with
  | Ok t' ->
    Alcotest.(check (option string)) "message" (Some "step 2 of 5") t'.status_message
  | Error e -> Alcotest.fail e

let test_update_status_invalid () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  (* Complete it first *)
  (match Task_store.update_status store t.task_id Mcp_types.Completed
     ~updated_at:"2025-01-01T00:01:00Z" () with
   | Ok _ -> ()
   | Error e -> Alcotest.fail e);
  (* Try to go back to working *)
  match Task_store.update_status store t.task_id Mcp_types.Working
    ~updated_at:"2025-01-01T00:02:00Z" () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for completed -> working"

let test_update_status_not_found () =
  let store = Task_store.create () in
  match Task_store.update_status store "ghost" Mcp_types.Working
    ~updated_at:"2025-01-01T00:00:00Z" () with
  | Error msg ->
    Alcotest.(check bool) "contains 'not found'" true
      (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error for missing task"

(* --- Task_store.cancel --- *)

let test_cancel_active () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  match Task_store.cancel store t.task_id ~updated_at:"2025-01-01T00:01:00Z" with
  | Ok t' -> Alcotest.(check bool) "cancelled" true (t'.status = Cancelled)
  | Error e -> Alcotest.fail e

let test_cancel_already_terminal () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  (match Task_store.update_status store t.task_id Failed
     ~updated_at:"2025-01-01T00:01:00Z" () with
   | Ok _ -> () | Error e -> Alcotest.fail e);
  match Task_store.cancel store t.task_id ~updated_at:"2025-01-01T00:02:00Z" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for cancelling failed task"

(* --- Task_store.set_result / get_result --- *)

let test_result_storage () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  Alcotest.(check bool) "no result yet" true
    (Task_store.get_result store t.task_id = None);
  let payload = `Assoc [("output", `String "42")] in
  Task_store.set_result store t.task_id payload;
  match Task_store.get_result store t.task_id with
  | Some r -> Alcotest.(check bool) "result matches" true (Yojson.Safe.equal r payload)
  | None -> Alcotest.fail "result should exist"

(* --- Task_store.list --- *)

let test_list_all () =
  let store = Task_store.create () in
  let _t0 = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  let _t1 = Task_store.create_task store ~created_at:"2025-01-01T00:00:01Z" () in
  let _t2 = Task_store.create_task store ~created_at:"2025-01-01T00:00:02Z" () in
  let (tasks, _next) = Task_store.list store () in
  Alcotest.(check int) "3 tasks" 3 (List.length tasks)

let test_list_filtered () =
  let store = Task_store.create () in
  let t0 = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  let _t1 = Task_store.create_task store ~created_at:"2025-01-01T00:00:01Z" () in
  (match Task_store.update_status store t0.task_id Completed
     ~updated_at:"2025-01-01T00:01:00Z" () with
   | Ok _ -> () | Error e -> Alcotest.fail e);
  let (working, _) = Task_store.list store ~status:Working () in
  let (completed, _) = Task_store.list store ~status:Completed () in
  Alcotest.(check int) "1 working" 1 (List.length working);
  Alcotest.(check int) "1 completed" 1 (List.length completed)

let test_list_pagination () =
  let store = Task_store.create () in
  for i = 0 to 4 do
    let _t = Task_store.create_task store
      ~created_at:(Printf.sprintf "2025-01-01T00:00:0%dZ" i) () in
    ()
  done;
  let (page1, cursor1) = Task_store.list store ~limit:2 () in
  Alcotest.(check int) "page1 size" 2 (List.length page1);
  Alcotest.(check bool) "has cursor" true (Option.is_some cursor1);
  let (page2, cursor2) = Task_store.list store ~limit:2 ?cursor:cursor1 () in
  Alcotest.(check int) "page2 size" 2 (List.length page2);
  Alcotest.(check bool) "has cursor2" true (Option.is_some cursor2);
  let (page3, cursor3) = Task_store.list store ~limit:2 ?cursor:cursor2 () in
  Alcotest.(check int) "page3 size" 1 (List.length page3);
  Alcotest.(check bool) "no more cursor" true (Option.is_none cursor3)

(* --- Task_store.remove / gc_terminal --- *)

let test_remove () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  Alcotest.(check bool) "removed" true (Task_store.remove store t.task_id);
  Alcotest.(check bool) "gone" true (Task_store.get store t.task_id = None);
  Alcotest.(check bool) "remove missing" false (Task_store.remove store t.task_id)

let test_gc_terminal () =
  let store = Task_store.create () in
  let t0 = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  let _t1 = Task_store.create_task store ~created_at:"2025-01-01T00:00:01Z" () in
  let t2 = Task_store.create_task store ~created_at:"2025-01-01T00:00:02Z" () in
  (match Task_store.update_status store t0.task_id Completed
     ~updated_at:"2025-01-01T00:01:00Z" () with
   | Ok _ -> () | Error e -> Alcotest.fail e);
  (match Task_store.update_status store t2.task_id Failed
     ~updated_at:"2025-01-01T00:01:00Z" () with
   | Ok _ -> () | Error e -> Alcotest.fail e);
  let removed = Task_store.gc_terminal store in
  Alcotest.(check int) "removed 2" 2 removed;
  Alcotest.(check int) "1 remaining" 1 (Task_store.count store)

(* --- Task_store.to_task_handlers --- *)

let dummy_ctx : Handler.context = {
  send_notification = (fun ~method_:_ ~params:_ -> Ok ());
  send_log = (fun _ _ -> Ok ());
  send_progress = (fun ~token:_ ~progress:_ ~message:_ ~total:_ -> Ok ());
  request_sampling = (fun _ -> Error "not implemented");
  request_roots_list = (fun () -> Error "not implemented");
  request_elicitation = (fun _ -> Error "not implemented");
}

let test_handlers_get () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  let handlers = Task_store.to_task_handlers store in
  match handlers.get dummy_ctx t.task_id with
  | Ok found -> Alcotest.(check string) "id" t.task_id found.task_id
  | Error e -> Alcotest.fail e

let test_handlers_get_missing () =
  let store = Task_store.create () in
  let handlers = Task_store.to_task_handlers store in
  match handlers.get dummy_ctx "ghost" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error"

let test_handlers_list () =
  let store = Task_store.create () in
  let _t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  let handlers = Task_store.to_task_handlers store in
  match handlers.list dummy_ctx None with
  | Ok (tasks, _) -> Alcotest.(check int) "1 task" 1 (List.length tasks)
  | Error e -> Alcotest.fail e

let test_handlers_cancel () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  let handlers = Task_store.to_task_handlers store in
  match handlers.cancel dummy_ctx t.task_id with
  | Ok t' -> Alcotest.(check bool) "cancelled" true (t'.status = Cancelled)
  | Error e -> Alcotest.fail e

let test_handlers_result_not_ready () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  let handlers = Task_store.to_task_handlers store in
  match handlers.result dummy_ctx t.task_id with
  | Error msg ->
    Alcotest.(check bool) "mentions not complete" true
      (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error for active task"

let test_handlers_result_with_payload () =
  let store = Task_store.create () in
  let t = Task_store.create_task store ~created_at:"2025-01-01T00:00:00Z" () in
  (match Task_store.update_status store t.task_id Completed
     ~updated_at:"2025-01-01T00:01:00Z" () with
   | Ok _ -> () | Error e -> Alcotest.fail e);
  let payload = `String "done" in
  Task_store.set_result store t.task_id payload;
  let handlers = Task_store.to_task_handlers store in
  match handlers.result dummy_ctx t.task_id with
  | Ok r -> Alcotest.(check bool) "result" true (Yojson.Safe.equal r payload)
  | Error e -> Alcotest.fail e

(* --- Full lifecycle --- *)

let test_full_lifecycle () =
  let store = Task_store.create () in
  (* 1. Create *)
  let t = Task_store.create_task store
    ~created_at:"2025-01-01T00:00:00Z"
    ~status_message:"queued" () in
  Alcotest.(check bool) "working" true (t.status = Working);
  (* 2. Update progress *)
  (match Task_store.update_status store t.task_id Working
     ~updated_at:"2025-01-01T00:00:10Z" ~message:"step 1/3" () with
   | Ok _ -> () | Error e -> Alcotest.fail e);
  (* 3. Need input *)
  (match Task_store.update_status store t.task_id Input_required
     ~updated_at:"2025-01-01T00:00:20Z" ~message:"need API key" () with
   | Ok _ -> () | Error e -> Alcotest.fail e);
  (* 4. Resume *)
  (match Task_store.update_status store t.task_id Working
     ~updated_at:"2025-01-01T00:00:30Z" ~message:"step 2/3" () with
   | Ok _ -> () | Error e -> Alcotest.fail e);
  (* 5. Complete *)
  (match Task_store.update_status store t.task_id Completed
     ~updated_at:"2025-01-01T00:01:00Z" ~message:"all done" () with
   | Ok t_final ->
     Alcotest.(check bool) "completed" true (t_final.status = Completed);
     Alcotest.(check (option string)) "final message" (Some "all done") t_final.status_message
   | Error e -> Alcotest.fail e);
  (* 6. Store result *)
  Task_store.set_result store t.task_id (`Assoc [("answer", `Int 42)]);
  (* 7. Verify terminal is stuck *)
  (match Task_store.update_status store t.task_id Working
     ~updated_at:"2025-01-01T00:02:00Z" () with
   | Error _ -> ()
   | Ok _ -> Alcotest.fail "should not transition from completed")

(* --- Suite --- *)

let () =
  Alcotest.run "Task Lifecycle" [
    "create", [
      Alcotest.test_case "basic" `Quick test_create_task;
      Alcotest.test_case "custom id" `Quick test_create_task_custom_id;
      Alcotest.test_case "auto increment" `Quick test_create_auto_increment;
    ];
    "get", [
      Alcotest.test_case "existing" `Quick test_get_existing;
      Alcotest.test_case "missing" `Quick test_get_missing;
    ];
    "update_status", [
      Alcotest.test_case "valid" `Quick test_update_status_valid;
      Alcotest.test_case "with message" `Quick test_update_status_with_message;
      Alcotest.test_case "invalid" `Quick test_update_status_invalid;
      Alcotest.test_case "not found" `Quick test_update_status_not_found;
    ];
    "cancel", [
      Alcotest.test_case "active" `Quick test_cancel_active;
      Alcotest.test_case "already terminal" `Quick test_cancel_already_terminal;
    ];
    "result", [
      Alcotest.test_case "storage" `Quick test_result_storage;
    ];
    "list", [
      Alcotest.test_case "all" `Quick test_list_all;
      Alcotest.test_case "filtered" `Quick test_list_filtered;
      Alcotest.test_case "pagination" `Quick test_list_pagination;
    ];
    "remove", [
      Alcotest.test_case "remove" `Quick test_remove;
      Alcotest.test_case "gc_terminal" `Quick test_gc_terminal;
    ];
    "to_task_handlers", [
      Alcotest.test_case "get" `Quick test_handlers_get;
      Alcotest.test_case "get missing" `Quick test_handlers_get_missing;
      Alcotest.test_case "list" `Quick test_handlers_list;
      Alcotest.test_case "cancel" `Quick test_handlers_cancel;
      Alcotest.test_case "result not ready" `Quick test_handlers_result_not_ready;
      Alcotest.test_case "result with payload" `Quick test_handlers_result_with_payload;
    ];
    "lifecycle", [
      Alcotest.test_case "full" `Quick test_full_lifecycle;
    ];
  ]
