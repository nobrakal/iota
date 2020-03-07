(** func name and type *)
type accessor = string * string

(** name and possible children *)
type ty_dec = string * accessor list

module S = Set.Make(String)
module Parent = Map.Make(String)

let possible_parent (xs : ty_dec list) =
  let upd parent = function
    | None -> Some (S.singleton parent)
    | Some xs -> Some (S.add parent xs) in
  let add_p m (parent,xs) =
    List.fold_left
      (fun m (_,x) -> Parent.update x (upd parent) m) m xs in
  List.fold_left add_p Parent.empty xs
