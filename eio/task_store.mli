(** In-memory task lifecycle store.

    Provides an ergonomic API for managing MCP async task state.
    Thread-safe via [Stdlib.Mutex] (non-yielding, works in both Eio
    and non-Eio contexts).

    Usage:
    {[
      let store = Task_store.create () in

      (* Register as handler *)
      let server =
        Server.create ~name:"my-server" ~version:"1.0.0" ()
        |> Server.add_task_handlers (Task_store.to_task_handlers store)
      in

      (* In a tool handler that creates a long-running task *)
      let task = Task_store.create_task store ?ttl ?poll_interval () in
      (* ... do work in a fiber, updating status ... *)
      Task_store.update_status store task.task_id Completed ~updated_at:"..." |> ignore;
    ]}

    @since 1.4.0
*)

open Mcp_protocol

(** Opaque task store handle. *)
type t

(** Create a new empty task store. *)
val create : unit -> t

(** Create a new task in [Working] state.
    @param task_id Optional custom ID. Auto-generated if omitted.
    @param created_at ISO 8601 timestamp (caller supplies for testability).
    @param ttl Optional time-to-live in milliseconds.
    @param poll_interval Optional poll interval in milliseconds.
    @param status_message Optional initial status message. *)
val create_task :
  t ->
  created_at:string ->
  ?task_id:string ->
  ?ttl:int ->
  ?poll_interval:int ->
  ?status_message:string ->
  unit -> Mcp_types.task

(** Look up a task by ID. *)
val get : t -> string -> Mcp_types.task option

(** Transition a task to a new status. Validates the state machine.
    Returns [Ok task'] on success, [Error reason] on invalid transition
    or unknown task_id. *)
val update_status :
  t -> string -> Mcp_types.task_status ->
  updated_at:string -> ?message:string ->
  unit -> (Mcp_types.task, string) result

(** Store or replace the result payload for a task.
    Typically called after transitioning to [Completed]. *)
val set_result : t -> string -> Yojson.Safe.t -> unit

(** Retrieve the result payload for a task. *)
val get_result : t -> string -> Yojson.Safe.t option

(** Cancel a task (transition to [Cancelled]).
    Returns [Error] if the task is already terminal or unknown. *)
val cancel : t -> string -> updated_at:string -> (Mcp_types.task, string) result

(** List tasks, optionally filtered by status. Supports cursor-based pagination.
    Returns [(tasks, next_cursor option)]. *)
val list :
  t -> ?status:Mcp_types.task_status -> ?cursor:string -> ?limit:int ->
  unit -> Mcp_types.task list * string option

(** Number of tasks in the store. *)
val count : t -> int

(** Remove a task from the store. Returns [true] if the task existed. *)
val remove : t -> string -> bool

(** Remove all tasks in terminal states. Returns the number removed. *)
val gc_terminal : t -> int

(** Build a {!Handler.task_handlers} record backed by this store.
    The returned record is suitable for {!Handler.add_task_handlers}. *)
val to_task_handlers : t -> Handler.task_handlers
