(** MCP Resources/ResourceTemplates helpers.

    Backward compatibility module.
    Re-exports resource types with helpers for resources/* methods.
*)

type resource = Protocol.resource

type resource_template = {
  uri_template : string;
  name : string;
  description : string;
  mime_type : string;
}

type content = {
  uri : string;
  mime_type : string;
  text : string;
}

let resource_template_to_json (t : resource_template) =
  `Assoc [
    ("uriTemplate", `String t.uri_template);
    ("name", `String t.name);
    ("description", `String t.description);
    ("mimeType", `String t.mime_type);
  ]

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
