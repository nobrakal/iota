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
  | Lit of 'a lit
  | Unop of unop * 'a formula
  | Binop of binop * 'a formula * 'a formula

type 'a safe =
  | Leaf of 'a formula
  | Forall of 'a  * 'a formula * 'a safe
  | Exists of 'a * 'a formula * 'a safe
  | Pand of 'a safe * 'a safe
  | Por of 'a safe * 'a safe

type 'a general =
  | General of 'a formula list * 'a formula

type 'a program =
  { safe : 'a safe
  ; ensure : 'a general
  ; maintain : 'a general }

let fold_formula l u b =
  let rec aux = function
    | Lit x -> l x
    | Unop (x,y) -> u x (aux y)
    | Binop (x,y,z) -> b x (aux y) (aux z)
  in aux

module type Variables = sig
  type t
  val compare : t -> t -> int
end

module About(V : Variables) = struct

  type nonrec program = V.t program

  module S = Set.Make(V)
  let to_list s = S.fold (fun x y -> x::y) s []

  let variables_of_dynamic = function
    | Has x -> S.singleton x
    | Link (x,y) -> S.of_list [x;y]
    | Other (_,x) -> S.singleton x

  let variables_of_formula =
    let lit = function
       | Dyn (_,x) -> variables_of_dynamic x
       | Stat (_,x) -> S.singleton x in
    fold_formula lit (fun _ x -> x) (fun _ -> S.union)

  let extract_guard phi =
    match to_list (variables_of_formula phi) with
    | [x;y] -> Some (x,y)
    | _ -> None

  let is_guard_for x phi =
    match extract_guard phi with
    | None -> false
    | Some (y,z) -> x = y || x = z

  let rec verify_guards = function
    | Leaf _ ->
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
            match valid_candidate phi (extract_xvar_candidates_of x xs) with
            | None -> false
            | Some _ -> true in
       let fv_phi_incl_fv_guards =
         let fv_guards = List.fold_left S.union S.empty guards in
         S.for_all (fun x -> S.mem x fv_guards) (variables_of_formula phi) in
       exists_xvar && fv_phi_incl_fv_guards

  let is_valid_program {safe; ensure; maintain} =
    verify_guards safe && verify_general ensure && verify_general maintain

end
