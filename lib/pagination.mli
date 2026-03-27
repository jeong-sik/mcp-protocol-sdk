(** Cursor-based pagination with loop detection.

    [collect_pages fetch_page] repeatedly calls [fetch_page] with the current
    cursor (starting from [None]) and accumulates results until no next cursor
    is returned.  Detects cursor loops and returns [Error] if one is found. *)
val collect_pages :
  (string option -> (('a list * string option), string) result) ->
  ('a list, string) result
