(** {2 Tasks (experimental, 2025-11-25)} *)

(** Task execution status.
    Tasks are durable state machines carrying execution state. *)
type task_status =
  | Working
  | Input_required
  | Completed
  | Failed
  | Cancelled

let task_status_to_yojson = function
  | Working -> `String "working"
  | Input_required -> `String "input_required"
  | Completed -> `String "completed"
  | Failed -> `String "failed"
  | Cancelled -> `String "cancelled"

let task_status_of_yojson = function
  | `String "working" -> Ok Working
  | `String "input_required" -> Ok Input_required
  | `String "completed" -> Ok Completed
  | `String "failed" -> Ok Failed
  | `String "cancelled" -> Ok Cancelled
  | _ -> Error "task_status: expected 'working', 'input_required', 'completed', 'failed', or 'cancelled'"

let task_status_is_terminal = function
  | Completed | Failed | Cancelled -> true
  | Working | Input_required -> false

(** Terminal task statuses — tasks in these states cannot transition further. *)
type terminal_status =
  | Terminal_completed
  | Terminal_failed
  | Terminal_cancelled

(** Active task statuses — tasks that are still in progress. *)
type active_status =
  | Active_working
  | Active_input_required

(** Classified view of task_status — separates terminal from active at the type level.
    Functions that should only accept active tasks can take [active_status],
    and functions that should only accept terminal tasks can take [terminal_status]. *)
type classified_status =
  | Terminal of terminal_status
  | Active of active_status

let classify_status = function
  | Completed -> Terminal Terminal_completed
  | Failed -> Terminal Terminal_failed
  | Cancelled -> Terminal Terminal_cancelled
  | Working -> Active Active_working
  | Input_required -> Active Active_input_required

let of_terminal = function
  | Terminal_completed -> Completed
  | Terminal_failed -> Failed
  | Terminal_cancelled -> Cancelled

let of_active = function
  | Active_working -> Working
  | Active_input_required -> Input_required

(** Task object — execution state of a request. *)
type task = {
  task_id: string;
  status: task_status;
  status_message: string option;
  created_at: string;
  last_updated_at: string;
  ttl: int option;
  poll_interval: int option;
}

let task_to_yojson (t : task) =
  let fields = [
    ("taskId", `String t.task_id);
    ("status", task_status_to_yojson t.status);
    ("createdAt", `String t.created_at);
    ("lastUpdatedAt", `String t.last_updated_at);
  ] in
  let fields = match t.status_message with
    | Some m -> ("statusMessage", `String m) :: fields | None -> fields
  in
  let fields = match t.ttl with
    | Some v -> ("ttl", `Int v) :: fields | None -> fields
  in
  let fields = match t.poll_interval with
    | Some v -> ("pollInterval", `Int v) :: fields | None -> fields
  in
  `Assoc fields

let task_of_yojson = function
  | `Assoc fields ->
    let task_id = match List.assoc_opt "taskId" fields with
      | Some (`String s) -> Ok s
      | _ -> Error "task: missing 'taskId'"
    in
    let status = match List.assoc_opt "status" fields with
      | Some j -> task_status_of_yojson j
      | None -> Error "task: missing 'status'"
    in
    let status_message = match List.assoc_opt "statusMessage" fields with
      | Some (`String s) -> Some s | _ -> None
    in
    let created_at = match List.assoc_opt "createdAt" fields with
      | Some (`String s) -> Ok s
      | _ -> Error "task: missing 'createdAt'"
    in
    let last_updated_at = match List.assoc_opt "lastUpdatedAt" fields with
      | Some (`String s) -> Ok s
      | _ -> Error "task: missing 'lastUpdatedAt'"
    in
    let ttl = match List.assoc_opt "ttl" fields with
      | Some (`Int n) -> Some n
      | Some (`Null) -> None
      | _ -> None
    in
    let poll_interval = match List.assoc_opt "pollInterval" fields with
      | Some (`Int n) -> Some n | _ -> None
    in
    (match task_id, status, created_at, last_updated_at with
     | Ok task_id, Ok status, Ok created_at, Ok last_updated_at ->
       Ok { task_id; status; status_message; created_at; last_updated_at; ttl; poll_interval }
     | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _ | _, _, _, Error e ->
       Error e)
  | _ -> Error "task: expected object"

(** Task parameters — included in request params to create a task. *)
type task_params = {
  ttl: int option;
}

let task_params_to_yojson (p : task_params) =
  let fields = match p.ttl with
    | Some v -> [("ttl", `Int v)]
    | None -> []
  in
  `Assoc fields

let task_params_of_yojson = function
  | `Assoc fields ->
    let ttl = match List.assoc_opt "ttl" fields with
      | Some (`Int n) -> Some n | _ -> None
    in
    Ok { ttl }
  | _ -> Error "task_params: expected object"

(** CreateTaskResult — response to a task-augmented request. *)
type create_task_result = {
  task: task;
}

let create_task_result_to_yojson (r : create_task_result) =
  `Assoc [("task", task_to_yojson r.task)]

let create_task_result_of_yojson = function
  | `Assoc fields ->
    (match List.assoc_opt "task" fields with
     | Some j ->
       Result.map (fun task -> { task }) (task_of_yojson j)
     | None -> Error "create_task_result: missing 'task'")
  | _ -> Error "create_task_result: expected object"

(** Tool-level task support declaration. *)
type task_execution_support = Task_required | Task_optional | Task_forbidden

let task_execution_support_to_yojson = function
  | Task_required -> `String "required"
  | Task_optional -> `String "optional"
  | Task_forbidden -> `String "forbidden"

let task_execution_support_of_yojson = function
  | `String "required" -> Ok Task_required
  | `String "optional" -> Ok Task_optional
  | `String "forbidden" -> Ok Task_forbidden
  | _ -> Error "task_execution_support: expected 'required', 'optional', or 'forbidden'"

(** Valid state transitions per MCP spec.
    Working      -> Working | Input_required | Completed | Failed | Cancelled
    Input_required -> Working | Completed | Failed | Cancelled
    Terminal states (Completed, Failed, Cancelled) have no outgoing transitions. *)
let valid_transitions = function
  | Working -> [Working; Input_required; Completed; Failed; Cancelled]
  | Input_required -> [Working; Completed; Failed; Cancelled]
  | Completed | Failed | Cancelled -> []

(** Attempt a state transition. Returns [Ok task'] with updated status and
    last_updated_at, or [Error reason] if the transition is invalid. *)
let transition (t : task) ~next_status ~updated_at =
  let allowed = valid_transitions t.status in
  if List.mem next_status allowed then
    Ok { t with status = next_status; last_updated_at = updated_at }
  else
    let pp s = match task_status_to_yojson s with `String s -> s | _ -> "?" in
    Error (Printf.sprintf "invalid transition: %s -> %s" (pp t.status) (pp next_status))

(** Return a copy with an updated status_message and last_updated_at.
    Does not change the status itself. *)
let with_message (t : task) ~message ~updated_at =
  { t with status_message = Some message; last_updated_at = updated_at }

(** Convenience: create a task in working state. *)
let make_task ~task_id ~created_at ?status_message ?ttl ?poll_interval () =
  { task_id; status = Working; status_message;
    created_at; last_updated_at = created_at;
    ttl; poll_interval }
