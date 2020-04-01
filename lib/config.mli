open Utils

type accessor =
  | Simple of string * string (** func name and type *)
  | Multiple of string * int * string (** func name, a number of child and type *)

(** name and possible children *)
type ty_dec = string * accessor list

type config =
  { maxprof : int
  ; predicates : (bool * string * string) list (** is_dynamic,name,type *)
  ; types : ty_dec list }

val static_of_config : config -> StringSet.t
val dynamic_of_config : config -> StringSet.t
