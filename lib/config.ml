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

let map_filter f p xs =
  List.(map f (filter p xs))

let static_of_config config =
  StringSet.of_list (map_filter (fun (_,x,_) -> x) (fun (b,_,_) -> not b) config.predicates)

let dynamic_of_config config =
  StringSet.of_list (map_filter (fun (_,x,_) -> x) (fun (b,_,_) -> b) config.predicates)
