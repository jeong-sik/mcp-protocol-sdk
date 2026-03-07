type protocol_error = {
  code: int;
  message: string;
  data: Yojson.Safe.t option;
}

type sdk_error =
  | Not_connected
  | Connection_closed of string
  | Request_timeout of float
  | Cancelled of string option
  | Send_failed of string
  | Parse_error of string
  | Internal of string

type t =
  | Protocol of protocol_error
  | Sdk of sdk_error

let to_string = function
  | Protocol { code; message; data = None } ->
    Printf.sprintf "ProtocolError(%d): %s" code message
  | Protocol { code; message; data = Some d } ->
    Printf.sprintf "ProtocolError(%d): %s [%s]" code message
      (Yojson.Safe.to_string d)
  | Sdk Not_connected -> "SdkError: Not connected"
  | Sdk (Connection_closed reason) ->
    Printf.sprintf "SdkError: Connection closed (%s)" reason
  | Sdk (Request_timeout seconds) ->
    Printf.sprintf "SdkError: Request timed out after %.1fs" seconds
  | Sdk (Cancelled None) -> "SdkError: Request cancelled"
  | Sdk (Cancelled (Some reason)) ->
    Printf.sprintf "SdkError: Request cancelled (%s)" reason
  | Sdk (Send_failed detail) ->
    Printf.sprintf "SdkError: Send failed (%s)" detail
  | Sdk (Parse_error detail) ->
    Printf.sprintf "SdkError: Parse error (%s)" detail
  | Sdk (Internal detail) ->
    Printf.sprintf "SdkError: Internal error (%s)" detail

let protocol ~code ~message ?data () = Protocol { code; message; data }
let timeout seconds = Sdk (Request_timeout seconds)
let cancelled ?reason () = Sdk (Cancelled reason)
let connection_closed reason = Sdk (Connection_closed reason)
let send_failed detail = Sdk (Send_failed detail)
let parse_error detail = Sdk (Parse_error detail)
let not_connected = Sdk Not_connected
let internal detail = Sdk (Internal detail)

let to_string_result = function
  | Ok v -> Ok v
  | Error e -> Error (to_string e)
