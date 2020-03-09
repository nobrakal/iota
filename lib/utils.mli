module StringSet : Set.S with type elt = string

(** Add parenthesis around the string *)
val paren : string -> string

(** Add space around the string *)
val space : string -> string
