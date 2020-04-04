module StringSet = Set.Make(String)

let paren s = "(" ^ s ^ ")"
let space s = " " ^ s ^ " "

let fold_opt f xs =
  let run_on_none acc x =
    match acc with
    | None -> f x
    | _ -> acc in
  List.fold_left run_on_none None xs
