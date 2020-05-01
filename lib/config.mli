open Utils

type accessor =
  | Simple of string * string (** func name and type *)
  | Multiple of string * int * string (** func name, a number of child and type *)

type config =
  { maxprof : int
  ; predicates : (bool * string) StringMap.t (** name -> is_dynamic,type *)
  ; types : (accessor list) StringMap.t (** name -> possible children *)
  }

val static_of_config : config -> StringSet.t
val dynamic_of_config : config -> StringSet.t
