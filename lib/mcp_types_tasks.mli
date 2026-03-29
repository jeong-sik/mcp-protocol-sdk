(** {2 Tasks (experimental, 2025-11-25)} *)

type task_status =
  | Working
  | Input_required
  | Completed
  | Failed
  | Cancelled

val task_status_to_yojson : task_status -> Yojson.Safe.t
val task_status_of_yojson : Yojson.Safe.t -> (task_status, string) result
val task_status_is_terminal : task_status -> bool

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

(** Classify a task_status into terminal or active. *)
val classify_status : task_status -> classified_status

(** Convert terminal_status back to task_status. *)
val of_terminal : terminal_status -> task_status

(** Convert active_status back to task_status. *)
val of_active : active_status -> task_status

type task = {
  task_id: string;
  status: task_status;
  status_message: string option;
  created_at: string;
  last_updated_at: string;
  ttl: int option;
  poll_interval: int option;
}

val task_to_yojson : task -> Yojson.Safe.t
val task_of_yojson : Yojson.Safe.t -> (task, string) result

type task_params = {
  ttl: int option;
}

val task_params_to_yojson : task_params -> Yojson.Safe.t
val task_params_of_yojson : Yojson.Safe.t -> (task_params, string) result

type create_task_result = {
  task: task;
}

val create_task_result_to_yojson : create_task_result -> Yojson.Safe.t
val create_task_result_of_yojson : Yojson.Safe.t -> (create_task_result, string) result

type task_execution_support = Task_required | Task_optional | Task_forbidden

val task_execution_support_to_yojson : task_execution_support -> Yojson.Safe.t
val task_execution_support_of_yojson : Yojson.Safe.t -> (task_execution_support, string) result

(** Valid next states for a given status. Terminal states return [[]]. *)
val valid_transitions : task_status -> task_status list

(** Attempt a state transition. Returns [Ok task'] with updated status and
    last_updated_at, or [Error reason] if the transition is invalid.
    Terminal states reject all transitions. *)
val transition : task -> next_status:task_status -> updated_at:string -> (task, string) result

(** Return a copy with an updated status_message and last_updated_at.
    Does not change the status itself. *)
val with_message : task -> message:string -> updated_at:string -> task

val make_task :
  task_id:string -> created_at:string ->
  ?status_message:string -> ?ttl:int -> ?poll_interval:int ->
  unit -> task
