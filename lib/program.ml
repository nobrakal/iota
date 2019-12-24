type unop = Not

type binop =
  | And
  | Or

type 'a dynamic =
  | Has of 'a
  | Link of 'a * 'a
  | Other of string * 'a

type 'a lit =
  | Dyn of bool * 'a dynamic
  | Stat of string * 'a

type 'a formula =
  | Lit of 'a
  | Unop of unop * 'a formula
  | Binop of binop * 'a formula * 'a formula

type ('a,'l) pre_safe =
  | Leaf of 'l formula
  | Var of string
  | Forall of 'a  * 'l formula * ('a,'l) pre_safe
  | Exists of 'a * 'l formula * ('a,'l) pre_safe
  | Pand of ('a,'l) pre_safe * ('a,'l) pre_safe
  | Por of ('a,'l) pre_safe * ('a,'l) pre_safe

type 'a safe = ('a, 'a lit) pre_safe

type 'l general =
  | General of 'l formula list * 'l formula

type ('a,'l) pre_program =
  { vars : (string * ('a, 'l) pre_safe) list
  ; safe : ('a, 'l) pre_safe list
  ; ensure : 'l general list
  ; maintain : 'l general list }

type 'a parsed_program = ('a, bool * 'a dynamic) pre_program
type 'a program = ('a, 'a lit) pre_program

let fold_formula l u b =
  let rec aux = function
    | Lit x -> l x
    | Unop (x,y) -> u x (aux y)
    | Binop (x,y,z) -> b x (aux y) (aux z)
  in aux

type gen = Ensure | Maintain

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

type parse_error =
  | UnboundDynamic of string
  | UnboundSymbol of string

let string_of_parse_error = function
  | UnboundDynamic s -> "Unbound dynamic: " ^ s
  | UnboundSymbol s -> "Unbound symbol: " ^ s

module SString = Set.Make(String)

let rec free_variables_of_safe = function
  | Leaf _ -> SString.empty
  | Var x -> SString.singleton x
  | Forall (_,_,x) | Exists (_,_,x) -> free_variables_of_safe x
  | Pand (x,y) | Por (x,y) -> SString.union (free_variables_of_safe x) (free_variables_of_safe y)

module type Variables = sig
  type t
  val compare : t -> t -> int
end

module Make(V : Variables) = struct

  type nonrec parsed_program = V.t parsed_program
  type nonrec program = V.t program

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
    let aux (name,x) acc =
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

  exception ParseError of parse_error

  let final_of_formula ~static ~dynamic =
    let mk_lit (b,dyn) =
      match dyn with
      | Has _ | Link _ -> Dyn (b,dyn)
      | Other (s,x) ->
         if SString.mem s dynamic then Dyn (b,dyn)
         else
           if b
           then raise (ParseError (UnboundDynamic s))
           else
             if SString.mem s static then Stat (s,x)
             else raise (ParseError (UnboundSymbol s))
    in
    fold_formula (fun x -> Lit (mk_lit x))
      (fun u x -> Unop (u,x)) (fun b x y -> Binop (b,x,y))

  let safe_of_parsed ~static ~dynamic =
    let rec aux = function
      | Leaf x -> Leaf (final_of_formula ~static ~dynamic x)
      | Var x -> Var x
      | Forall (a,l,x) -> Forall (a, final_of_formula ~static ~dynamic l, aux x)
      | Exists (a,l,x) -> Exists (a, final_of_formula ~static ~dynamic l, aux x)
      | Pand (x,y) -> Pand (aux x, aux y)
      | Por (x,y) -> Por (aux x, aux y)
    in aux

  let program_of_parsed ~static ~dynamic {vars; safe; ensure; maintain} =
    try
      let general (General (xs,x)) =
        General ((List.map (final_of_formula ~static ~dynamic) xs),(final_of_formula ~static ~dynamic x)) in
      let vars = List.map (fun (name,x) -> name, safe_of_parsed ~static ~dynamic x) vars in
      let safe = List.map (safe_of_parsed ~static ~dynamic) safe in
      let ensure = List.map general ensure in
      let maintain = List.map general maintain in
      Ok ({vars; safe; ensure; maintain})
    with
       ParseError s -> Error s

end
