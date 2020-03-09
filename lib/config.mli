type config =
  { maxprof : int
  ; predicates : (bool * string * string) list (** is_dynamic,name,type *)
  ; types : Sum_types.ty_dec list }

val static_of_config : config -> Program.SString.t
val dynamic_of_config : config -> Program.SString.t
