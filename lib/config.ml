type config =
  { maxprof : int
  ; predicates : (bool * string * string) list (** is_dynamic,name,type *)
  ; types : Sum_types.ty_dec list }

let map_filter f p xs =
  List.(map f (filter p xs))

let static_of_config config =
  Program.SString.of_list (map_filter (fun (_,x,_) -> x) (fun (b,_,_) -> not b) config.predicates)

let dynamic_of_config config =
  Program.SString.of_list (map_filter (fun (_,x,_) -> x) (fun (b,_,_) -> b) config.predicates)
