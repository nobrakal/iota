open Program

let rec free_variables_of_safe = function
  | Leaf _ -> SString.empty
  | Var x -> SString.singleton x
  | Forall (_,_,x) | Exists (_,_,x) -> free_variables_of_safe x
  | Pand (x,y) | Por (x,y) -> SString.union (free_variables_of_safe x) (free_variables_of_safe y)

module Make(V : Set.OrderedType) = struct

  type validity =
  | Good
  | IllFormedGuard
  | IllFormedGeneral of gen
  | UnboundVar of string

let string_of_validity = function
  | Good -> "Good"
  | IllFormedGuard -> "Ill-formed guard"
  | UnboundVar s -> "Unbound variable " ^ s
  | IllFormedGeneral g ->
     let s =
       match g with
       | Ensure -> "ensure"
       | Maintain -> "maintain" in
     "Ill-formed "^ s ^" part"

  module S = Set.Make(V)
  let to_list s = S.fold (fun x y -> x::y) s []

  let variables_of_dynamic = function
    | Has x -> S.singleton x
    | Link (x,y) -> S.of_list [x;y]
    | Other (_,x) -> S.singleton x

  let variables_of_lit = function
    | Dyn (_,x) -> variables_of_dynamic x
    | Stat (_,x) -> S.singleton x

  let variables_of_formula =
    fold_formula variables_of_lit (fun _ x -> x) (fun _ -> S.union)

  let extract_guard phi =
    match to_list (variables_of_formula phi) with
    | [x;y] -> Some (x,y)
    | _ -> None

  let is_guard_for x phi =
    match extract_guard phi with
    | None -> false
    | Some (y,z) -> x = y || x = z

  let rec verify_guards = function
    | Leaf _ | Var _ ->
       true
    | Forall (x,guard,phi) | Exists (x,guard,phi) ->
       is_guard_for x guard && verify_guards phi
    | Pand (x,y) | Por (x,y) ->
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

  exception UnboundVar' of string

  let verify_variable_scope xs safe =
    let verif acc x =
      let fv = free_variables_of_safe x in
      SString.iter
        (fun s ->
          if not (SString.mem s acc)
          then raise (UnboundVar' s)) fv in
    let aux (Def (name,_,x)) acc =
      verif acc x;
      SString.add name acc
    in
    try
      let vars = List.fold_right aux xs SString.empty in
      List.iter (verif vars) safe;
      None
    with
    | UnboundVar' s -> Some s

  let validate_program {vars; safe; ensure; maintain} =
    if not (List.for_all verify_guards safe)
    then IllFormedGuard
    else if not (List.for_all verify_general ensure)
    then IllFormedGeneral (Ensure)
    else if not (List.for_all verify_general maintain)
    then IllFormedGeneral (Maintain)
    else
      match verify_variable_scope vars safe with
      | Some s -> UnboundVar s
      | None -> Good
end
