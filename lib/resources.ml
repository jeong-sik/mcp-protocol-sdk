(** MCP Resources/ResourceTemplates helpers.

    Backward compatibility module.
    Re-exports resource types with helpers for resources/* methods.
*)

type resource = Protocol.resource

type resource_template = {
  uri_template : string;
  name : string;
  description : string option;
  mime_type : string option;
}

type content = {
  uri : string;
  mime_type : string;
  text : string;
}

let resource_template_to_json (t : resource_template) =
  let base = [
    ("uriTemplate", `String t.uri_template);
    ("name", `String t.name);
  ] in
  let with_desc = match t.description with
    | Some d -> base @ [("description", `String d)]
    | None -> base
  in
  let with_mime = match t.mime_type with
    | Some m -> with_desc @ [("mimeType", `String m)]
    | None -> with_desc
  in
  `Assoc with_mime

let content_to_json (c : content) =
  `Assoc [
    ("uri", `String c.uri);
    ("mimeType", `String c.mime_type);
    ("text", `String c.text);
  ]

let list_result (resources : resource list) =
  `Assoc [
    ("resources", `List (List.map Protocol.resource_to_json resources));
  ]

let templates_list_result (templates : resource_template list) =
  `Assoc [
    ("resourceTemplates", `List (List.map resource_template_to_json templates));
  ]

let read_result (contents : content list) =
  `Assoc [
    ("contents", `List (List.map content_to_json contents));
  ]
