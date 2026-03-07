(** MCP Resources/ResourceTemplates helpers.

    Backward compatibility module.
    Re-exports resource types from Mcp_types with helpers for resources/* methods. *)

type resource = Mcp_types.resource
type resource_template = Mcp_types.resource_template
type content = Mcp_types.resource_contents

val resource_template_to_json : resource_template -> Yojson.Safe.t
val content_to_json : content -> Yojson.Safe.t
val list_result : resource list -> Yojson.Safe.t
val templates_list_result : resource_template list -> Yojson.Safe.t
val read_result : content list -> Yojson.Safe.t
