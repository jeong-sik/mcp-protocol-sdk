(** In-memory task lifecycle store.

    Thread-safe under Eio: all mutable state is guarded by [Mutex].
    Uses [Stdlib.Mutex] because this module has no yielding operations
    inside critical sections and may be called from non-Eio contexts (tests). *)

open Mcp_protocol

module StringMap = Map.Make(String)

type entry = {
  task: Mcp_types.task;
  result: Yojson.Safe.t option;
}

type t = {
  mutable tasks: entry StringMap.t;
  mutable counter: int;
  mu: Mutex.t;
}

let create () =
  { tasks = StringMap.empty; counter = 0; mu = Mutex.create () }

(** Generate a short unique task ID: "task-<counter>". *)
let gen_id t =
  let n = t.counter in
  t.counter <- n + 1;
  Printf.sprintf "task-%d" n

let with_lock t f =
  Mutex.lock t.mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mu) f

let create_task t ~created_at ?task_id ?ttl ?poll_interval ?status_message () =
  with_lock t (fun () ->
    let task_id = match task_id with
      | Some id -> id
      | None -> gen_id t
    in
    let task = Mcp_types.make_task ~task_id ~created_at ?status_message ?ttl ?poll_interval () in
    t.tasks <- StringMap.add task_id { task; result = None } t.tasks;
    task)

let get t id =
  with_lock t (fun () ->
    Option.map (fun e -> e.task) (StringMap.find_opt id t.tasks))

let update_status t id next_status ~updated_at ?message () =
  with_lock t (fun () ->
    match StringMap.find_opt id t.tasks with
    | None -> Error (Printf.sprintf "task not found: %s" id)
    | Some entry ->
      match Mcp_types.transition entry.task ~next_status ~updated_at with
      | Error _ as e -> e
      | Ok task' ->
        let task' = match message with
          | Some msg -> Mcp_types.with_message task' ~message:msg ~updated_at
          | None -> task'
        in
        t.tasks <- StringMap.add id { entry with task = task' } t.tasks;
        Ok task')

let set_result t id payload =
  with_lock t (fun () ->
    match StringMap.find_opt id t.tasks with
    | None -> ()
    | Some entry ->
      t.tasks <- StringMap.add id { entry with result = Some payload } t.tasks)

let get_result t id =
  with_lock t (fun () ->
    match StringMap.find_opt id t.tasks with
    | None -> None
    | Some entry -> entry.result)

let cancel t id ~updated_at =
  update_status t id Cancelled ~updated_at ()

let list t ?status ?cursor ?(limit = 100) () =
  with_lock t (fun () ->
    let all_entries =
      StringMap.bindings t.tasks
      |> List.map snd
      |> List.map (fun e -> e.task)
    in
    (* Filter by status *)
    let filtered = match status with
      | None -> all_entries
      | Some s -> List.filter (fun (task : Mcp_types.task) -> task.status = s) all_entries
    in
    (* Sort by task_id ascending for stable pagination.
       task_id is auto-incrementing ("task-<N>") so it provides a
       unique, monotonic cursor — unlike created_at which may collide
       when multiple tasks are created within the same timestamp. *)
    let sorted = List.sort (fun (a : Mcp_types.task) (b : Mcp_types.task) ->
      String.compare a.task_id b.task_id) filtered
    in
    (* Apply cursor: skip tasks with task_id <= cursor *)
    let after_cursor = match cursor with
      | None -> sorted
      | Some c ->
        List.filter (fun (task : Mcp_types.task) ->
          String.compare task.task_id c > 0) sorted
    in
    (* Apply limit *)
    let page = List.filteri (fun i _ -> i < limit) after_cursor in
    let next_cursor =
      if List.length after_cursor > limit then
        match List.rev page with
        | last :: _ -> Some last.Mcp_types.task_id
        | [] -> None
      else None
    in
    (page, next_cursor))

let count t =
  with_lock t (fun () -> StringMap.cardinal t.tasks)

let remove t id =
  with_lock t (fun () ->
    let existed = StringMap.mem id t.tasks in
    t.tasks <- StringMap.remove id t.tasks;
    existed)

let gc_terminal t =
  with_lock t (fun () ->
    let before = StringMap.cardinal t.tasks in
    t.tasks <- StringMap.filter (fun _ entry ->
      not (Mcp_types.task_status_is_terminal entry.task.status)) t.tasks;
    before - StringMap.cardinal t.tasks)

let to_task_handlers (t : t) : Handler.task_handlers =
  {
    get = (fun _ctx task_id ->
      match get t task_id with
      | Some task -> Ok task
      | None -> Error (Printf.sprintf "task not found: %s" task_id));

    result = (fun _ctx task_id ->
      match get_result t task_id with
      | Some r -> Ok r
      | None ->
        match get t task_id with
        | None -> Error (Printf.sprintf "task not found: %s" task_id)
        | Some task ->
          if Mcp_types.task_status_is_terminal task.status then
            Ok `Null  (* terminal but no result stored *)
          else
            Error (Printf.sprintf "task %s is not yet complete (status: %s)"
              task_id
              (match Mcp_types.task_status_to_yojson task.status with
               | `String s -> s | _ -> "?")));

    list = (fun _ctx cursor ->
      match list t ?cursor () with
      | tasks, next -> Ok (tasks, next));

    cancel = (fun _ctx task_id ->
      match get t task_id with
      | None -> Error (Printf.sprintf "task not found: %s" task_id)
      | Some task ->
        let updated_at = task.Mcp_types.last_updated_at in
        (* Use the same updated_at as fallback; callers with real clocks
           should use [cancel] directly. *)
        cancel t task_id ~updated_at);
  }
