(** Type-safe tool argument extraction from JSON.

    Eliminates manual [Yojson.Safe.t] pattern matching for tool handlers.

    Usage:
    {[
      let handler _ctx _name args =
        let open Tool_arg in
        let* text = required args "text" string in
        let count = optional args "count" int ~default:1 in
        Ok (Mcp_types.tool_result_of_text
          (String.concat "" (List.init count (fun _ -> text))))
    ]}
*)

(** {2 Extraction Functions}

    Each extractor takes a JSON value and returns [Ok value] or [Error msg]. *)

type 'a extractor = Yojson.Safe.t -> ('a, string) result

let string : string extractor = function
  | `String s -> Ok s
  | j -> Error (Printf.sprintf "expected string, got %s" (Yojson.Safe.to_string j))

let int : int extractor = function
  | `Int i -> Ok i
  | `Float f when Float.is_integer f -> Ok (int_of_float f)
  | j -> Error (Printf.sprintf "expected int, got %s" (Yojson.Safe.to_string j))

let float : float extractor = function
  | `Float f -> Ok f
  | `Int i -> Ok (float_of_int i)
  | j -> Error (Printf.sprintf "expected float, got %s" (Yojson.Safe.to_string j))

let bool : bool extractor = function
  | `Bool b -> Ok b
  | j -> Error (Printf.sprintf "expected bool, got %s" (Yojson.Safe.to_string j))

let json : Yojson.Safe.t extractor = fun j -> Ok j

let list_of (extract : 'a extractor) : 'a list extractor = function
  | `List items ->
    List.fold_left (fun acc item ->
      match acc, extract item with
      | Ok lst, Ok v -> Ok (lst @ [v])
      | Error e, _ -> Error e
      | _, Error e -> Error e
    ) (Ok []) items
  | j -> Error (Printf.sprintf "expected array, got %s" (Yojson.Safe.to_string j))

(** {2 Field Access}

    Extract named fields from tool [args] (which is [Yojson.Safe.t option]). *)

(** Extract a required field. Returns [Error] if the field is missing or
    if the args are [None]. *)
let required (args : Yojson.Safe.t option) (name : string) (extract : 'a extractor)
  : ('a, string) result =
  match args with
  | None -> Error (Printf.sprintf "missing argument '%s' (no arguments provided)" name)
  | Some (`Assoc fields) ->
    begin match List.assoc_opt name fields with
    | Some j ->
      begin match extract j with
      | Ok v -> Ok v
      | Error e -> Error (Printf.sprintf "argument '%s': %s" name e)
      end
    | None -> Error (Printf.sprintf "missing required argument '%s'" name)
    end
  | Some _ -> Error "arguments must be a JSON object"

(** Extract an optional field. Returns [default] if the field is missing. *)
let optional (args : Yojson.Safe.t option) (name : string) (extract : 'a extractor)
    ~(default : 'a) : 'a =
  match args with
  | Some (`Assoc fields) ->
    begin match List.assoc_opt name fields with
    | Some j -> (match extract j with Ok v -> v | Error _ -> default)
    | None -> default
    end
  | _ -> default

(** Extract an optional field as [Some value] or [None]. *)
let optional_opt (args : Yojson.Safe.t option) (name : string) (extract : 'a extractor)
  : 'a option =
  match args with
  | Some (`Assoc fields) ->
    begin match List.assoc_opt name fields with
    | Some j -> (match extract j with Ok v -> Some v | Error _ -> None)
    | None -> None
    end
  | _ -> None

(** {2 Monadic Binding}

    Use [let*] for chaining required argument extractions:
    {[
      let handler _ctx _name args =
        let open Tool_arg in
        let* x = required args "x" int in
        let* y = required args "y" int in
        Ok (Mcp_types.tool_result_of_text (string_of_int (x + y)))
    ]}
*)

let ( let* ) = Result.bind
