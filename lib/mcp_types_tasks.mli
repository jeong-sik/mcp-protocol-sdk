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

val make_task :
  task_id:string -> created_at:string ->
  ?status_message:string -> ?ttl:int -> ?poll_interval:int ->
  unit -> task
