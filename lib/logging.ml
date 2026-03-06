(** MCP Logging types.

    Supports the logging/setLevel request and notifications/message notification.

    Reference: https://modelcontextprotocol.io/docs/concepts/utilities/logging
*)

(** Log level as defined by MCP specification (RFC 5424 based). *)
type log_level =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency

let log_level_to_string = function
  | Debug -> "debug"
  | Info -> "info"
  | Notice -> "notice"
  | Warning -> "warning"
  | Error -> "error"
  | Critical -> "critical"
  | Alert -> "alert"
  | Emergency -> "emergency"

let log_level_of_string = function
  | "debug" -> Some Debug
  | "info" -> Some Info
  | "notice" -> Some Notice
  | "warning" -> Some Warning
  | "error" -> Some Error
  | "critical" -> Some Critical
  | "alert" -> Some Alert
  | "emergency" -> Some Emergency
  | _ -> None

let log_level_to_yojson level =
  `String (log_level_to_string level)

let log_level_of_yojson = function
  | `String s ->
    (match log_level_of_string s with
     | Some level -> Ok level
     | None -> Error ("Unknown log level: " ^ s))
  | _ -> Error "Log level must be a string"

(** Numeric severity (RFC 5424 order: emergency=0, debug=7). *)
let log_level_to_int = function
  | Emergency -> 0
  | Alert -> 1
  | Critical -> 2
  | Error -> 3
  | Warning -> 4
  | Notice -> 5
  | Info -> 6
  | Debug -> 7

(** Compare log levels by severity. Returns negative if [a] is more severe. *)
let compare_level a b =
  compare (log_level_to_int a) (log_level_to_int b)

(** Check if a message at [msg_level] should be emitted given [min_level]. *)
let should_log ~min_level ~msg_level =
  log_level_to_int msg_level <= log_level_to_int min_level

(** Logging message notification data (notifications/message). *)
type logging_message = {
  level: log_level;
  logger: string option;
  data: Yojson.Safe.t;
}

let logging_message_to_yojson msg =
  let fields = [
    ("level", log_level_to_yojson msg.level);
    ("data", msg.data);
  ] in
  let fields = match msg.logger with
    | Some l -> ("logger", `String l) :: fields
    | None -> fields
  in
  `Assoc fields

let logging_message_of_yojson = function
  | `Assoc fields ->
    let level_json = List.assoc "level" fields in
    (match log_level_of_yojson level_json with
     | Ok level ->
       let logger = match List.assoc_opt "logger" fields with
         | Some (`String s) -> Some s
         | _ -> None
       in
       let data = match List.assoc_opt "data" fields with
         | Some d -> d
         | None -> `Null
       in
       Ok { level; logger; data }
     | Error e -> Error e)
  | _ -> Error "logging_message must be an object"

(** Set level request params (logging/setLevel). *)
type set_level_params = {
  level: log_level;
}

let set_level_params_to_yojson p =
  `Assoc [("level", log_level_to_yojson p.level)]

let set_level_params_of_yojson = function
  | `Assoc fields ->
    let level_json = List.assoc "level" fields in
    (match log_level_of_yojson level_json with
     | Ok level -> Ok { level }
     | Error e -> Error e)
  | _ -> Error "set_level_params must be an object"
