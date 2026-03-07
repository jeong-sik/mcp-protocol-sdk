(** MCP Logging types.

    Supports the logging/setLevel request and notifications/message notification.
    Log levels follow RFC 5424.
*)

type log_level =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency

val log_level_to_string : log_level -> string
val log_level_of_string : string -> log_level option
val log_level_to_yojson : log_level -> Yojson.Safe.t
val log_level_of_yojson : Yojson.Safe.t -> (log_level, string) result
val log_level_to_int : log_level -> int
val compare_level : log_level -> log_level -> int
val should_log : min_level:log_level -> msg_level:log_level -> bool

type logging_message = {
  level: log_level;
  logger: string option;
  data: Yojson.Safe.t;
}

val logging_message_to_yojson : logging_message -> Yojson.Safe.t
val logging_message_of_yojson : Yojson.Safe.t -> (logging_message, string) result

type set_level_params = {
  level: log_level;
}

val set_level_params_to_yojson : set_level_params -> Yojson.Safe.t
val set_level_params_of_yojson : Yojson.Safe.t -> (set_level_params, string) result
