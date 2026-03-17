(** Result combinators for list traversal.
    Eliminates the ubiquitous fold_left + Result pattern. *)

val traverse_list : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
(** [traverse_list f xs] applies [f] to each element. Returns [Ok] with all results
    if all succeed, or the first [Error]. *)

val filter_map_list : ('a -> ('b option, 'e) result) -> 'a list -> ('b list, 'e) result
(** [filter_map_list f xs] like traverse_list but filters [None] results. *)
