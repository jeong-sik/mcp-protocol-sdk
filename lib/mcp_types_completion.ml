(** {2 Completion} *)

(** Reference to the item being completed — either a prompt or a resource.
    MCP spec uses a "type" discriminator field:
    - \{ "type": "ref/prompt", "name": "..." \}
    - \{ "type": "ref/resource", "uri": "..." \}
*)
type completion_reference =
  | Prompt_ref of { name: string }
  | Resource_ref of { uri: string }

let completion_reference_to_yojson = function
  | Prompt_ref { name } ->
    `Assoc [("type", `String "ref/prompt"); ("name", `String name)]
  | Resource_ref { uri } ->
    `Assoc [("type", `String "ref/resource"); ("uri", `String uri)]

let completion_reference_of_yojson = function
  | `Assoc fields -> begin
    match List.assoc_opt "type" fields with
    | Some (`String "ref/prompt") -> begin
      match List.assoc_opt "name" fields with
      | Some (`String name) -> Ok (Prompt_ref { name })
      | _ -> Error "completion_reference: ref/prompt missing 'name'"
    end
    | Some (`String "ref/resource") -> begin
      match List.assoc_opt "uri" fields with
      | Some (`String uri) -> Ok (Resource_ref { uri })
      | _ -> Error "completion_reference: ref/resource missing 'uri'"
    end
    | Some (`String t) -> Error ("completion_reference: unknown type " ^ t)
    | _ -> Error "completion_reference: missing 'type'"
  end
  | _ -> Error "completion_reference: expected object"

(** Argument to complete — the name and current partial value *)
type completion_argument = {
  name: string;
  value: string;
}
[@@deriving yojson]

(** Create a completion argument *)
let make_completion_argument ~name ~value = { name; value }

(** Result of completion/complete — a list of suggested values *)
type completion_result = {
  values: string list;
  total: int option; [@default None]
  has_more: bool option; [@default None] [@key "hasMore"]
}
[@@deriving yojson]

(** Create a completion result *)
let make_completion_result ~values ?total ?has_more () =
  { values; total; has_more }

(** Completion context — wrapping arguments for the completion/complete request.
    MCP 2025-11-25 adds this context wrapper. *)
type completion_context = {
  arguments: (string * string) list option; [@default None]
}

let completion_context_to_yojson (ctx : completion_context) =
  let fields = match ctx.arguments with
    | Some pairs ->
      let args = `Assoc (List.map (fun (k, v) -> (k, `String v)) pairs) in
      [("arguments", args)]
    | None -> []
  in
  `Assoc fields

let completion_context_of_yojson = function
  | `Assoc fields ->
    let arguments = match List.assoc_opt "arguments" fields with
      | Some (`Assoc pairs) ->
        Some (List.filter_map (fun (k, v) ->
          match v with `String s -> Some (k, s) | _ -> None
        ) pairs)
      | _ -> None
    in
    Ok { arguments }
  | _ -> Error "completion_context: expected object"
