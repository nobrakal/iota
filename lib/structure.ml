open Program
open Final

module Make(Manip : Manip) = struct

  open Manip

  type invalidity =
    | IllFormedGuard
    | IllFormedGeneral of gen

  let string_of_invalidity = function
    | IllFormedGuard -> "Ill-formed guard"
    | IllFormedGeneral g ->
       let s =
         match g with
         | Ensure -> "ensure"
         | Maintain -> "maintain" in
       "Ill-formed "^ s ^" part"

  let is_guard_for x phi =
    match extract_guard phi with
    | None -> false
    | Some (y,z) -> x = y || x = z

  let rec verify_guards = function
    | Leaf _ | Var _ ->
       true
    | Forall (x,guard,phi) | Exists (x,guard,phi) ->
       is_guard_for x guard && verify_guards phi
    | Pand (x,y) | Por (x,y) | Apply (x,y)  ->
       verify_guards x && verify_guards y

  let extract_guards = function
    | [] -> Some []
    | x::xs ->
       let aux xs x =
         Option.bind xs (fun xs -> Option.map (fun x -> x::xs) (extract_guard x)) in
       List.fold_left aux (Option.map (fun x -> [x]) (extract_guard x)) xs

  let extract_xvar_candidates_of x xs =
    to_list (List.fold_left (fun acc x -> S.inter acc x) x xs)

  let valid_candidate phi lst =
    let lit x_var = function
      | Dyn (_,x) -> S.equal x_var (variables_of_dynamic x)
      | Stat _ -> true in
    let aux acc x =
      match acc with
      | Some _ -> acc
      | None ->
         if fold_formula (lit (S.singleton x)) (fun _ x -> x) (fun _ -> ( && )) phi
         then Some x
         else None in
    List.fold_left aux None lst

  let verify_general (General (guards,phi)) =
    match extract_guards guards with
    | None -> false
    | Some guards ->
       let guards = List.map (fun (x,y) -> S.of_list [x;y]) guards in
       let exists_xvar =
         match guards with
         | [] -> true
         | x::xs ->
            Option.is_some (valid_candidate phi (extract_xvar_candidates_of x xs)) in
       let fv_phi_incl_fv_guards =
         let fv_guards = List.fold_left S.union S.empty guards in
         S.for_all (fun x -> S.mem x fv_guards) (variables_of_formula phi) in
       exists_xvar && fv_phi_incl_fv_guards

  let validate_program {fsafe; fensure; fmaintain} =
    if not (List.for_all verify_guards fsafe)
    then Some (IllFormedGuard)
    else if not (List.for_all verify_general fensure)
    then Some (IllFormedGeneral (Ensure))
    else if not (List.for_all verify_general fmaintain)
    then Some (IllFormedGeneral (Maintain))
    else None
end
