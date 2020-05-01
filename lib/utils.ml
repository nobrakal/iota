module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

let keys_of_map xs = StringMap.fold (fun x _ -> StringSet.add x) xs StringSet.empty
let stringmap_of_list xs = List.fold_left (fun acc (x,y) -> StringMap.add x y acc) StringMap.empty xs

let paren s = "(" ^ s ^ ")"
let space s = " " ^ s ^ " "

let fold_opt f xs =
  let run_on_none acc x =
    match acc with
    | None -> f x
    | _ -> acc in
  List.fold_left run_on_none None xs

let stringmap_fold_opt f xs =
  let run_on_none k x acc =
    match acc with
    | None -> f k x
    | _ -> acc in
  StringMap.fold run_on_none xs None

let print_warning s =
  print_endline ("[WARNING] " ^ s)
