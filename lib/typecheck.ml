open Program

module Subst = Map.Make(String)

type base_ty = Safet | Litt

type monoty =
  | V of string
  | T of base_ty
  | Arrow of (monoty * monoty)

let rec string_of_monoty = function
  | V x -> x
  | T x ->
     begin match x with
     | Safet -> "Safe"
     | Litt -> "Lit" end
  | Arrow (x,y) -> "(" ^ string_of_monoty x ^ ") -> (" ^ string_of_monoty y ^ ")"

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
    | WrongType of monoty * monoty (* actual, expected *)

  exception Te of type_error

  let string_of_type_error string_of_var = function
    | UnboundVar t ->
       "Unbound variable: " ^ (string_of_var t)
    | WrongType (x,y) ->
       "Type error:\n"
       ^ "Expected: " ^ string_of_monoty x ^ "\n"
       ^ "Actual:   " ^ string_of_monoty y

  let unify x y =
    try unify x y with
    | Exit -> raise (Te (WrongType (x,y)))

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
    List.fold_left (fun acc v -> Subst.union left_bias (unify v ty_lit) acc) Subst.empty vars

  let union x y =
    let x' = Subst.fold (fun x _ y -> SString.add x y) x SString.empty in
    let y' = Subst.fold (fun x _ y -> SString.add x y) y SString.empty in
    let inter = SString.inter x' y' in
    let substs =
      SString.fold (fun e acc -> (unify (Subst.find e x) (Subst.find e y)) :: acc ) inter [] in
    let final = compose_subst x y in
    match substs with
    | [] -> final
    | x::xs ->
       let f = List.fold_left compose_subst x xs in
       compose_subst final f

  let ti_formula env x =
    let rec aux = function
      | Lit x -> ti_lit env x
      | Unop (_,x) -> aux x
      | Binop (_,x,y) ->
         union (aux x) (aux y)
    in aux x

  let try_unify x y =
    try unify x y
    with
      Exit -> raise (Te (WrongType (x, y)))

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
         let s = try_unify t ty_safe in
         ty_safe,union s (union subst b)
      | Pand (x,y) | Por (x,y) ->
         let t1,x = aux env x in
         let t2,y = aux env y in
         let s1 = try_unify t1 ty_safe in
         let s2 = try_unify t2 ty_safe in
         ty_safe, union s2 (union s1 (union x y))
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
    let safe = T Safet in
    let v_env = M.fold (fun k _ s -> S.add k s) env S.empty in
    let v_xs = List.fold_left S.union S.empty (List.map variables_of_safe xs) in
    let fv = S.diff v_xs v_env in
    let env = S.fold (fun k env -> M.add k (scheme_of_mono (T Litt)) env) fv env in
    List.iter (fun x -> let t = fst (ti_safe env x) in if t <> safe then raise (Te (WrongType (t,safe)))) xs

  let typecheck_program {vars; safe; _} =
    let env = M.empty in
    try
      let s,env = ti_lets env vars in
      let env = apply_subst_env s env in
      verify_safe env safe; None
    with
    | Te x -> Some x
end
