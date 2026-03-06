(** MCP Resources/ResourceTemplates helpers.

    Backward compatibility module.
    Re-exports resource types from Mcp_types with helpers for resources/* methods.
*)

type resource = Mcp_types.resource

type resource_template = Mcp_types.resource_template

type content = Mcp_types.resource_contents

let resource_template_to_json = Mcp_types.resource_template_to_yojson

let content_to_json (c : content) =
  Mcp_types.resource_contents_to_yojson c

let list_result (resources : resource list) =
  `Assoc [
    ("resources", `List (List.map Mcp_types.resource_to_yojson resources));
  ]

let templates_list_result (templates : resource_template list) =
  `Assoc [
    ("resourceTemplates", `List (List.map resource_template_to_json templates));
  ]

let read_result (contents : content list) =
  `Assoc [
    ("contents", `List (List.map content_to_json contents));
  ]
