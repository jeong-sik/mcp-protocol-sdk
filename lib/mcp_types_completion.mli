(** {2 Completion} *)

type completion_reference =
  | Prompt_ref of { name: string }
  | Resource_ref of { uri: string }

val completion_reference_to_yojson : completion_reference -> Yojson.Safe.t
val completion_reference_of_yojson : Yojson.Safe.t -> (completion_reference, string) result

type completion_argument = {
  name: string;
  value: string;
}

val completion_argument_to_yojson : completion_argument -> Yojson.Safe.t
val completion_argument_of_yojson : Yojson.Safe.t -> (completion_argument, string) result

val make_completion_argument : name:string -> value:string -> completion_argument

type completion_result = {
  values: string list;
  total: int option;
  has_more: bool option;
}

val completion_result_to_yojson : completion_result -> Yojson.Safe.t
val completion_result_of_yojson : Yojson.Safe.t -> (completion_result, string) result

val make_completion_result : values:string list -> ?total:int -> ?has_more:bool -> unit -> completion_result

type completion_context = {
  arguments: (string * string) list option;
}

val completion_context_to_yojson : completion_context -> Yojson.Safe.t
val completion_context_of_yojson : Yojson.Safe.t -> (completion_context, string) result
