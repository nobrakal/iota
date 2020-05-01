open Utils

type accessor =
  | Simple of string * string (** func name and type *)
  | Multiple of string * int * string (** func name, a number of child and type *)

type config =
  { maxprof : int
  ; predicates : (bool * string) StringMap.t (** name -> is_dynamic,type *)
  ; types : (accessor list) StringMap.t (** name -> possible children *)
  ; links : string list StringMap.t (** name -> possible linked *)
  }

let static_of_config config =
  keys_of_map (StringMap.filter (fun _ (b,_) -> not b) config.predicates)

let dynamic_of_config config =
  keys_of_map (StringMap.filter (fun _ (b,_) -> b) config.predicates)
