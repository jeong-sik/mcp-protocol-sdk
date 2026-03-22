(** Type-safe tool argument extraction from JSON.

    {[
      let handler _ctx _name args =
        let open Tool_arg in
        let* text = required args "text" string in
        let count = optional args "count" int ~default:1 in
        Ok (tool_result_of_text (String.concat "" (List.init count (fun _ -> text))))
    ]}
*)

(** {2 Extractors} *)

(** A function that extracts a typed value from JSON. *)
type 'a extractor = Yojson.Safe.t -> ('a, string) result

val string : string extractor
val int : int extractor
val float : float extractor
val bool : bool extractor
val json : Yojson.Safe.t extractor
val list_of : 'a extractor -> 'a list extractor

(** {2 Field Access} *)

(** Extract a required field from tool args. Error if missing. *)
val required : Yojson.Safe.t option -> string -> 'a extractor -> ('a, string) result

(** Extract an optional field, returning [default] if missing. *)
val optional : Yojson.Safe.t option -> string -> 'a extractor -> default:'a -> 'a

(** Extract an optional field as [Some value] or [None]. *)
val optional_opt : Yojson.Safe.t option -> string -> 'a extractor -> 'a option

(** {2 Monadic Binding} *)

(** [let*] for chaining required extractions. *)
val ( let* ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
