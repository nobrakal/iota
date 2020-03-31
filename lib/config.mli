open Utils

(** func name and type *)
type accessor = string * string

(** name and possible children *)
type ty_dec = string * accessor list

type config =
  { maxprof : int
  ; predicates : (bool * string * string) list (** is_dynamic,name,type *)
  ; types : ty_dec list }

val static_of_config : config -> StringSet.t
val dynamic_of_config : config -> StringSet.t
