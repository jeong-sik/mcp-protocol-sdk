(** Mid-complexity MCP server demonstrating three SDK usage patterns:

    1. Raw types   — direct record construction for resources/content
    2. Builders    — make_tool, make_text_content, Tool_arg for tools
    3. Bridge      — converting domain types to SDK types (consumer pattern)

    A note-taking server with create, search, and resource access.

    Usage (stdio transport):
      dune exec examples/note_server.exe

    Usage (HTTP transport):
      dune exec examples/note_server.exe -- --http 8080 *)

open Mcp_protocol

(* ================================================================
   Domain types — these live in the consumer, not the SDK.
   The bridge pattern converts these to SDK types.
   ================================================================ *)

type note = {
  id: int;
  title: string;
  body: string;
  tags: string list;
  created_at: float;
}

let notes : note list ref = ref []
let next_id = ref 1

(* ================================================================
   Pattern 1: Raw types — direct record construction.
   Use when SDK convenience builders don't cover your case,
   or when converting from existing domain types.
   ================================================================ *)

(** Convert a domain [note] to an SDK resource_contents record.
    This is the "bridge" from domain → SDK. *)
let resource_contents_of_note (n : note) : Mcp_types.resource_contents =
  let json = `Assoc [
    ("id", `Int n.id);
    ("title", `String n.title);
    ("body", `String n.body);
    ("tags", `List (List.map (fun t -> `String t) n.tags));
    ("created_at", `Float n.created_at);
  ] in
  (* Raw record construction — no make_ helper needed for resource_contents *)
  Mcp_types.{
    uri = Printf.sprintf "note://%d" n.id;
    mime_type = Some "application/json";
    text = Some (Yojson.Safe.to_string json);
    blob = None;
  }

(* ================================================================
   Pattern 2: Builders — make_tool + Tool_arg for ergonomic tools.
   Preferred for most tool definitions.
   ================================================================ *)

(** Tool handler: create a note.
    Demonstrates Tool_arg with required + optional + list_of. *)
let handle_create_note _ctx _name args =
  let open Tool_arg in
  let* title = required args "title" string in
  let* body = required args "body" string in
  let tags = optional args "tags" (list_of string) ~default:[] in
  let id = !next_id in
  incr next_id;
  let note = { id; title; body; tags; created_at = Unix.gettimeofday () } in
  notes := note :: !notes;
  Ok (Mcp_types.tool_result_of_text
    (Printf.sprintf "Created note #%d: %s" id title))

(** Tool handler: search notes by query string.
    Demonstrates multiple content items in a single result. *)
let handle_search_notes _ctx _name args =
  let open Tool_arg in
  let* query = required args "query" string in
  let limit = optional args "limit" int ~default:10 in
  let query_lower = String.lowercase_ascii query in
  let matches =
    List.filter (fun n ->
      String.lowercase_ascii n.title |> fun t ->
      String.lowercase_ascii n.body |> fun b ->
      (* Substring search in title or body *)
      let contains s sub =
        let len_s = String.length s and len_sub = String.length sub in
        if len_sub > len_s then false
        else
          let rec check i =
            if i > len_s - len_sub then false
            else if String.sub s i len_sub = sub then true
            else check (i + 1)
          in check 0
      in
      contains t query_lower || contains b query_lower)
      !notes
  in
  let matches = List.filteri (fun i _ -> i < limit) matches in
  let content = match matches with
    | [] -> [Mcp_types.make_text_content "No notes found."]
    | _ ->
        List.map (fun n ->
          Mcp_types.make_text_content
            (Printf.sprintf "#%d [%s] %s"
               n.id
               (String.concat "," n.tags)
               n.title))
          matches
  in
  Ok Mcp_types.{ content; is_error = None; structured_content = None; _meta = None }

(** Tool handler: delete a note by ID.
    Demonstrates destructive tool annotation. *)
let handle_delete_note _ctx _name args =
  let open Tool_arg in
  let* id = required args "id" int in
  let before = List.length !notes in
  notes := List.filter (fun n -> n.id <> id) !notes;
  let after = List.length !notes in
  if before > after then
    Ok (Mcp_types.tool_result_of_text (Printf.sprintf "Deleted note #%d." id))
  else
    Ok (Mcp_types.tool_result_of_error (Printf.sprintf "Note #%d not found." id))

(* ================================================================
   Pattern 3: Bridge — tool_annotations, input_schema, make_tool.
   Shows how to define metadata that helps clients (IDEs, agents)
   understand tool behavior without calling them.
   ================================================================ *)

(** Custom JSON Schema for create_note input. *)
let create_note_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("title", `Assoc [
      ("type", `String "string");
      ("description", `String "Note title")]);
    ("body", `Assoc [
      ("type", `String "string");
      ("description", `String "Note body text")]);
    ("tags", `Assoc [
      ("type", `String "array");
      ("items", `Assoc [("type", `String "string")]);
      ("description", `String "Optional tags for categorization")]);
  ]);
  ("required", `List [`String "title"; `String "body"]);
]

let search_note_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("query", `Assoc [
      ("type", `String "string");
      ("description", `String "Search text (case-insensitive substring match)")]);
    ("limit", `Assoc [
      ("type", `String "integer");
      ("description", `String "Max results (default: 10)")]);
  ]);
  ("required", `List [`String "query"]);
]

let delete_note_schema = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc [
    ("id", `Assoc [
      ("type", `String "integer");
      ("description", `String "Note ID to delete")]);
  ]);
  ("required", `List [`String "id"]);
]

(* ================================================================
   Server setup — composing tools, resources, and prompts.
   ================================================================ *)

let () =
  Eio_main.run @@ fun env ->
  let server =
    Mcp_protocol_eio.Server.create
      ~name:"note-server" ~version:"0.1.0"
      ~instructions:"Note-taking MCP server. Demonstrates raw types, builders, and bridge patterns."
      ()
    (* --- Tools with annotations via make_tool --- *)
    |> Mcp_protocol_eio.Server.add_tool
         (Mcp_types.make_tool ~name:"create_note"
            ~description:"Create a new note with title, body, and optional tags."
            ~input_schema:create_note_schema
            ~annotations:Mcp_types.{
              title = Some "Create Note";
              read_only_hint = Some false;
              destructive_hint = Some false;
              idempotent_hint = Some false;
              open_world_hint = Some false;
            } ())
         handle_create_note
    |> Mcp_protocol_eio.Server.add_tool
         (Mcp_types.make_tool ~name:"search_notes"
            ~description:"Search notes by text query."
            ~input_schema:search_note_schema
            ~annotations:Mcp_types.{
              title = Some "Search Notes";
              read_only_hint = Some true;
              destructive_hint = Some false;
              idempotent_hint = Some true;
              open_world_hint = Some false;
            } ())
         handle_search_notes
    |> Mcp_protocol_eio.Server.add_tool
         (Mcp_types.make_tool ~name:"delete_note"
            ~description:"Delete a note by ID."
            ~input_schema:delete_note_schema
            ~annotations:Mcp_types.{
              title = Some "Delete Note";
              read_only_hint = Some false;
              destructive_hint = Some true;
              idempotent_hint = Some true;
              open_world_hint = Some false;
            } ())
         handle_delete_note
    (* --- Resource: list all notes --- *)
    |> Mcp_protocol_eio.Server.resource ~uri:"note://all" "all-notes"
         ~description:"List of all stored notes" ~mime_type:"application/json"
         (fun _ctx _uri ->
           Ok (List.rev_map resource_contents_of_note !notes))
  in
  Mcp_protocol_eio.Server.run server
    ~stdin:(Eio.Stdenv.stdin env)
    ~stdout:(Eio.Stdenv.stdout env)
    ()
