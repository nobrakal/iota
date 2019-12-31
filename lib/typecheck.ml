open Program

module Subst = Map.Make(String)

type base_ty = Safet | Litt

type monoty =
  | V of string
  | T of base_ty
  | Arrow of (monoty * monoty)

type scheme = S of string list * monoty

let internal_counter = ref 0

let fresh_ty () =
  internal_counter := !internal_counter + 1;
  "_" ^ string_of_int ! internal_counter

let rec fv_of_ty = function
  | T _ -> SString.empty
  | V x -> SString.singleton x
  | Arrow (x,y) -> SString.union (fv_of_ty x) (fv_of_ty y)

let fv_of_scheme (S (xs,x)) =
  SString.diff (fv_of_ty x) (SString.of_list xs)

let scheme_of_mono x = S ([],x)

let apply_subst_ty subst =
  let rec aux = function
    | T _ as t -> t
    | Arrow (x,y) -> Arrow (aux x, aux y)
    | V x as t ->
       match Subst.find_opt x subst with
       | None -> t
       | Some x -> x in
  aux

let apply_subst_scheme subst (S (xs,x)) =
  let subst = Subst.filter (fun x _ -> List.mem x xs) subst in
  S (xs, apply_subst_ty subst x)

let left_bias _ x _ = Some x

let compose_subst s1 s2 =
  Subst.union left_bias (Subst.map (apply_subst_ty s1) s2) s1

let instanciate (S (xs,x)) =
  let subst = List.fold_left (fun s k -> Subst.add k (V (fresh_ty ())) s) Subst.empty xs in
  apply_subst_ty subst x

let bindvar u t =
  if (V u) = t
  then Subst.empty
  else
    let fv = fv_of_ty t in
    if SString.mem u fv
    then raise Exit
    else Subst.singleton u t

let unify x y =
  let rec aux = function
    | Arrow (x1,x2), Arrow (y1,y2) ->
       let s1 = aux (x1,y1) in
       let s2 = aux (apply_subst_ty s1 x2, apply_subst_ty s1 y2) in
       compose_subst s1 s2
    | T x, T y when x = y -> Subst.empty
    | V x,y | y, V x -> bindvar x y
    | _ -> raise Exit
  in aux (x,y)

let ty_safe = T Safet
let ty_lit = T Litt

let applys xs fv =
  List.fold_right (fun x acc -> Arrow (x,acc)) xs fv

module Make (Manip : Manip) = struct
  open Manip

  type type_error =
    | UnboundVar of Manip.t
    | WrongType of string

  exception Te of type_error

  let string_of_type_error string_of_var = function
    | UnboundVar t -> "Unbound variable: " ^ (string_of_var t)
    | WrongType x -> "Type error: " ^ x

  let unify x y =
    try unify x y with
    | Exit -> raise (Te (WrongType "unify"))

  let todo _ _ _ = assert false

  let fv_of_env x = M.fold (fun _ x s -> SString.union s (fv_of_scheme x)) x SString.empty
  let apply_subst_env s x = M.map (apply_subst_scheme s) x

  let generalize env t =
    let vars = SString.diff (fv_of_ty t) (fv_of_env env) in
    let vars = SString.fold (fun x xs -> x::xs) vars [] in
    S (vars,t)

  let ti_var env x =
    match M.find_opt x env with
    | None -> raise (Te (UnboundVar x))
    | Some t -> instanciate t

  let ti_lit env x =
    let vars = to_list (variables_of_lit x) in
    let vars = List.map (ti_var env) vars in
    List.fold_left (fun acc v -> Subst.union todo (unify v ty_lit) acc) Subst.empty vars

  let ti_formula env x =
    let rec aux = function
      | Lit x -> ti_lit env x
      | Unop (_,x) -> aux x
      | Binop (_,x,y) -> Subst.union todo (aux x) (aux y)
    in aux x

  let ti_safe env x =
    let rec aux env = function
      | Leaf x ->
         ty_safe,ti_formula env x
      | Var x ->
         ti_var env x, Subst.empty
      | Apply (x,y) ->
         let tv = V (fresh_ty ()) in
         let t,s = aux env x in
         let t',s' = aux (M.map (apply_subst_scheme s) env) y in
         let s'' = unify (apply_subst_ty s' t) (Arrow (t', tv)) in
         apply_subst_ty s'' tv,compose_subst s'' (compose_subst s' s)
      | Exists (x,u,b) | Forall (x,u,b) ->
         let nenv = M.add x (scheme_of_mono ty_lit) env in
         let subst = ti_formula nenv u in
         let t,b = aux nenv b in
         if t = ty_safe
         then ty_safe,Subst.union todo subst b
         else raise Exit
      | Pand (x,y) | Por (x,y) ->
         let t1,x = aux env x in
         let t2,y = aux env y in
         if t1 = ty_safe && t2 = ty_safe
         then ty_safe, Subst.union todo x y
         else raise Exit
    in aux env x

  let type_of_def env (Def (name,args,body)) =
    let args = List.map (fun x -> x,V (fresh_ty ())) args in
    let env = List.fold_left (fun acc (x,v) -> M.add x (scheme_of_mono v) acc) env args in
    let t,s = ti_safe env body in
    let t = applys (List.map (fun (_,v) -> apply_subst_ty s v) args) t in
    name,t,s

  let ti_let env def =
    let name,t,s = type_of_def env def in
    let t' = generalize (apply_subst_env s env) t in
    let env' = M.add name t' env in
    s,env'

  let ti_lets env xs =
    List.fold_left
      (fun (s2,env) x -> let s1,env = ti_let env x in compose_subst s1 s2, env) (Subst.empty,env) xs

  let verify_safe env xs =
    let v_env = M.fold (fun k _ s -> S.add k s) env S.empty in
    let v_xs = List.fold_left S.union S.empty (List.map variables_of_safe xs) in
    let fv = S.diff v_xs v_env in
    let env = S.fold (fun k env -> M.add k (scheme_of_mono (T Litt)) env) fv env in
    List.for_all (fun x -> fst (ti_safe env x) = T Safet) xs

  let typecheck_program {vars; safe; _} =
    let env = M.empty in
    let s,env = ti_lets env vars in
    let env = apply_subst_env s env in
    if verify_safe env safe
    then None
    else Some (WrongType "A safe was not safe")
end
