(** MCP Client over Eio stdio transport. *)

module Base = Generic_client.Make (Stdio_transport)

type t = Base.t

type sampling_handler = Base.sampling_handler
type roots_handler = Base.roots_handler
type elicitation_handler = Base.elicitation_handler
type notification_handler = Base.notification_handler

let create ~stdin ~stdout ?clock () =
  let transport = Stdio_transport.create ~stdin ~stdout () in
  Base.create ~transport ?clock ()

let on_sampling = Base.on_sampling
let on_roots_list = Base.on_roots_list
let on_elicitation = Base.on_elicitation
let on_notification = Base.on_notification

let initialize = Base.initialize
let ping = Base.ping

let list_tools = Base.list_tools
let list_tools_all = Base.list_tools_all
let call_tool = Base.call_tool

let list_resources = Base.list_resources
let list_resources_all = Base.list_resources_all
let read_resource = Base.read_resource
let subscribe_resource = Base.subscribe_resource
let unsubscribe_resource = Base.unsubscribe_resource
let list_resource_templates = Base.list_resource_templates

let list_prompts = Base.list_prompts
let list_prompts_all = Base.list_prompts_all
let get_prompt = Base.get_prompt

let get_task = Base.get_task
let get_task_result = Base.get_task_result
let list_tasks = Base.list_tasks
let cancel_task = Base.cancel_task

let close = Base.close
