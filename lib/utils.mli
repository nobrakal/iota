module StringSet : Set.S with type elt = string
module StringMap : Map.S with type key = string

val keys_of_map : 'a StringMap.t -> StringSet.t
val stringmap_of_list : (string * 'a) list -> 'a StringMap.t

(** Add parenthesis around the string *)
val paren : string -> string

(** Add space around the string *)
val space : string -> string

val fold_opt : ('a -> 'b option) -> 'a list -> 'b option

val stringmap_fold_opt : (string -> 'a -> 'b option) -> 'a StringMap.t -> 'b option

val print_warning : string -> unit
