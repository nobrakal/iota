(** func name and type *)
type accessor = string * string

(** name and possible children *)
type ty_dec = string * accessor list

module S : Set.S with type elt = string
module Parent : Map.S with type key = string

val possible_parent : ty_dec list -> S.t Parent.t
