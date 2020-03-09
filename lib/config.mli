open Utils

type config =
  { maxprof : int
  ; predicates : (bool * string * string) list (** is_dynamic,name,type *)
  ; types : Sum_types.ty_dec list }

val static_of_config : config -> StringSet.t
val dynamic_of_config : config -> StringSet.t
