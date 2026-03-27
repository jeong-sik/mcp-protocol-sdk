module Cursor_set = Set.Make (String)

let collect_pages fetch_page =
  let rec loop seen cursor acc_rev =
    match fetch_page cursor with
    | Error _ as err -> err
    | Ok (page, next_cursor) ->
      begin match next_cursor with
      | Some value when Cursor_set.mem value seen ->
        Error (Printf.sprintf "Pagination cursor loop detected: %s" value)
      | Some value ->
        loop (Cursor_set.add value seen) (Some value)
          (List.rev_append page acc_rev)
      | None -> Ok (List.rev_append page acc_rev |> List.rev)
      end
  in
  loop Cursor_set.empty None []
