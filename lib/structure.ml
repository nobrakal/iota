open Program
open Final

type gen = Ensure | Maintain

module type Structure =
  sig
    type t

    type invalidity =
      | IllFormedGuard
      | IllFormedGeneral of gen

    val string_of_invalidity : invalidity -> string

    (** Retun good iff guards are really guards and ensure and maintain are well-formed *)
    val validate_program : t Final.final_program -> invalidity option
  end

module Make(Manip : Manip) = struct

  open Manip

  type t = Manip.t

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

  let verify_guards x =
    let rec aux = function
    | FLeaf _ -> true
    | FQuantif (_,x,(_,a,b),phi) ->
       (a <> b) && (x = extract_var a || x = extract_var b) && aux phi
    | FBinop (_,x,y) -> aux x && aux y
    in aux x

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
         if fold_formula (lit (S.singleton x)) (fun x -> x) (fun _ -> ( && )) phi
         then Some x
         else None in
    List.fold_left aux None lst

  let verify_general (General (guards,phi)) =
    let guards = List.map (fun (_,x,y) -> S.of_list [extract_var x; extract_var y]) guards in
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
