open Program
open Utils

type ('a,'l) rich_pre_safe =
  | F of ('a,'l) pre_safe
  | Bracket of 'a list * ('a,'l) pre_safe (* existentially quantified variables, without guards *)

type ('a,'l) rich_def =
  | RDef of ('a * 'a list * ('a,'l) rich_pre_safe)

type 'a typed_program =
  { tvars : ('a, ('a, rbinpred) lit) rich_def list
  ; tsafe : ('a, ('a, rbinpred) lit) pre_safe list
  ; tensure : ('a, ('a, rbinpred) lit, rbinpred) general list
  ; tmaintain : ('a, ('a, rbinpred) lit, rbinpred) general list }

module Subst = Map.Make(String)

(** Type of monomorphic types *)
type monoty =
  | Vt of string (** Type variable *)
  | Safet (** A safe expression *)
  | Litt of string (** A known litteral *)
  | Arrow of (monoty * monoty) (** Arrow *)

let rec string_of_monoty = function
  | Vt x -> "'" ^ x
  | Safet -> "Safe"
  | Litt l -> "Lit " ^ l
  | Arrow (x,y) -> "(" ^ string_of_monoty x ^ ") -> (" ^ string_of_monoty y ^ ")"

type scheme = S of string list * monoty

let fresh_ty =
  let internal_counter = ref 0 in
  fun () ->
  internal_counter := !internal_counter + 1;
  Vt ("_" ^ string_of_int ! internal_counter)

let rec fv_of_ty = function
  | Litt _ | Safet -> StringSet.empty
  | Vt x -> StringSet.singleton x
  | Arrow (x,y) -> StringSet.union (fv_of_ty x) (fv_of_ty y)

let fv_of_scheme (S (xs,x)) =
  StringSet.diff (fv_of_ty x) (StringSet.of_list xs)

let scheme_of_mono x = S ([],x)

let apply_subst_ty subst =
  let rec aux t =
    match t with
    | Litt _ | Safet -> t
    | Arrow (x,y) -> Arrow (aux x, aux y)
    | Vt x ->
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
  let subst = List.fold_left (fun s k -> Subst.add k (fresh_ty ()) s) Subst.empty xs in
  apply_subst_ty subst x

let bindvar u t =
  if (Vt u) = t
  then Subst.empty
  else
    let fv = fv_of_ty t in
    if StringSet.mem u fv
    then raise Exit
    else Subst.singleton u t

let unify x y =
  let rec aux = function
    | Arrow (x1,x2), Arrow (y1,y2) ->
       let s1 = aux (x1,y1) in
       let s2 = aux (apply_subst_ty s1 x2, apply_subst_ty s1 y2) in
       compose_subst s1 s2
    | Safet, Safet -> Subst.empty
    | Litt x, Litt y when x = y -> Subst.empty
    | Vt x,y | y, Vt x -> bindvar x y
    | _ -> raise Exit
  in aux (x,y)

let applys xs fv =
  List.fold_right (fun x acc -> Arrow (x,acc)) xs fv

module type Typecheck =
  sig

    type t

    type type_error =
      | UnboundVar of t
      | WrongType of monoty * monoty (* actual, expected *)

    val string_of_type_error : (t -> string) -> type_error -> string

    val typecheck_program :
      predicates:('a * string * string) list ->
      types:(Config.ty_dec list) -> t program -> (t typed_program,type_error) result
  end

module Make (Manip : Manip) = struct
  open Manip

  type t = Manip.t

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

  let fv_of_env x = M.fold (fun _ x s -> StringSet.union s (fv_of_scheme x)) x StringSet.empty
  let apply_subst_env s x = M.map (apply_subst_scheme s) x

  let generalize env t =
    let vars = StringSet.diff (fv_of_ty t) (fv_of_env env) in
    let vars = StringSet.fold (fun x xs -> x::xs) vars [] in
    S (vars,t)

  let extract_type =
    let open Config in
    function
    | Simple (_,t) | Multiple (_,_,t) -> t

  let get_accessor_type' f i xs =
    let open Config in
    let test = function
      | Simple (n,_) -> f = n
      | Multiple (n,limit,_) ->
         match i with
         | None -> false
         | Some i ->
            f = n && (i >=0 && i < limit) in
    let aux x =
      if test x then Some (extract_type x) else None  in
    fold_opt aux xs

  let get_accessor_type f i types =
    let aux (start,xs) =
      Option.map (fun x -> start,x) (get_accessor_type' f i xs) in
    fold_opt aux types

  let exists_end_type endt xs =
    List.exists (fun x -> extract_type x = endt) xs

  let ti_var ~types env x =
    let rec aux x =
      match x with
      | Program.V x ->
         begin match M.find_opt x env with
         | None -> raise (Te (UnboundVar x))
         | Some t -> instanciate t,Subst.empty end
      | Func (f,i,x) ->
         let tx,sx = aux x in
         begin match get_accessor_type f i types with
         | Some (start,endt) ->
            let s = unify tx (Litt start) in
            (Litt endt),(compose_subst s sx)
         | None -> failwith "todo: unbound function" end
      | Parent (startt,endt,x) ->
         let tx,sx = aux x in
         if List.exists (fun (x,xs) -> x=startt && exists_end_type endt xs) types
          then
            let s = unify tx (Litt endt) in
            (Litt startt),(compose_subst s sx)
         else failwith "Undefined parent"
    in aux x

  let variables_of_lit = function
    | Stat (_,x) -> [x]
    | Dyn (_,x) ->
       match x with
       | Has x | Other (_,x) -> [x]
       | Bin (_,x,y) -> [x; y]

  let verif_pred ~predicates xs =
    function
    | Stat (s,_) | Dyn (_,Other (s,_)) ->
       let _,_,ty = List.find (fun (_,n,_) -> s = n) predicates in
       unify (fst (List.hd xs)) (Litt ty)
    | _ -> Subst.empty

  let ti_lit ~predicates ~types env x =
    let vars = List.map (ti_var ~types env) (variables_of_lit x) in
    let subst = List.fold_left (fun acc v -> Subst.union left_bias (snd v) acc) Subst.empty vars in
    Subst.union left_bias (verif_pred ~predicates vars x) subst

  let union x y =
    let x' = Subst.fold (fun x _ y -> StringSet.add x y) x StringSet.empty in
    let y' = Subst.fold (fun x _ y -> StringSet.add x y) y StringSet.empty in
    let inter = StringSet.inter x' y' in
    let substs =
      StringSet.fold (fun e acc -> (unify (Subst.find e x) (Subst.find e y)) :: acc ) inter [] in
    let final = compose_subst x y in
    match substs with
    | [] -> final
    | x::xs ->
       let f = List.fold_left compose_subst x xs in
       compose_subst final f

  let try_unify x y =
    try unify x y
    with
      Exit -> raise (Te (WrongType (x, y)))

  let ti_safe ~predicates ~types env x =
    let rec aux env = function
      | Leaf x ->
         Safet,ti_lit ~predicates ~types env x
      | Var x ->
         let t =
           match M.find_opt x env with
           | None -> raise (Te (UnboundVar x))
           | Some t -> instanciate t in
         t, Subst.empty
      | Apply (x,y) ->
         let tv = fresh_ty () in
         let t,s = aux env x in
         let t',s' = aux (M.map (apply_subst_scheme s) env) y in
         let s'' = unify (apply_subst_ty s' t) (Arrow (t', tv)) in
         apply_subst_ty s'' tv,compose_subst s'' (compose_subst s' s)
      | Quantif (_,x,u,b) ->
         let nenv = M.add x (scheme_of_mono (fresh_ty ())) env in
         let subst = ti_lit ~predicates ~types nenv (Dyn (false,Bin u)) in
         let t,b = aux nenv b in
         let s = try_unify t Safet in
         Safet,union s (union subst b)
      | Formula f ->
         fold_formula (aux env) (fun x -> x) (fun _ (_,x) (_,y) -> Safet, union x y) f
    in aux env x

  let type_of_def ~predicates ~types env args body =
    let args = List.map (fun x -> x,fresh_ty ()) args in
    let env = List.fold_left (fun acc (x,v) -> M.add x (scheme_of_mono v) acc) env args in
    let t,s = ti_safe ~predicates ~types env body in
    let t = applys (List.map (fun (_,v) -> apply_subst_ty s v) args) t in
    t,s

  let ti_let ~predicates ~types env (Def (name,args,body) as e) =
    let body,(t,s) =
      try F body,type_of_def ~predicates ~types env args body
      with
      | Te (UnboundVar _)  -> (* unbound var, we try to quantify all the unbound var *) (* TODO useful ?? *)
         let fvs = Manip.(to_list (fv_of_def e)) in
         let nenv = List.fold_left (fun env x -> M.add x (scheme_of_mono (fresh_ty ())) env) env fvs in
         let body' = Bracket (fvs,body) in
         body',type_of_def ~predicates ~types nenv args body
      | x -> raise x in
    let t' = generalize (apply_subst_env s env) t in
    let env' = M.add name t' env in
    RDef (name,args,body),s,env'

  let ti_lets ~predicates ~types env xs =
    let ds,s,env =
      List.fold_left
        (fun (ds,s2,env) x ->
          let d,s1,env = ti_let ~predicates ~types env x
          in d::ds,compose_subst s1 s2, env) ([],Subst.empty,env) xs
    in
    List.rev ds,s,env

  let verify_safe ~predicates ~types env xs =
    let v_env = M.fold (fun k _ s -> S.add k s) env S.empty in
    let v_xs = List.fold_left S.union S.empty (List.map variables_of_safe xs) in
    let fv = S.diff v_xs v_env in
    let env = S.fold (fun k env -> M.add k (scheme_of_mono (fresh_ty ())) env) fv env in
    let aux x =
      let t = fst (ti_safe ~predicates ~types env x) in
      if t <> Safet
      then raise (Te (WrongType (t,Safet))) in
    List.iter aux xs

  let typecheck_program ~predicates ~types {vars; safe; ensure; maintain} =
    let env = M.empty in
    try
      let tvars,s,env = ti_lets ~predicates ~types env vars in
      let env = apply_subst_env s env in
      verify_safe ~predicates ~types env safe;
      let tsafe = safe in
      let tensure = ensure in
      let tmaintain = maintain in
      Ok {tvars;tsafe;tensure;tmaintain}
    with
    | Te x -> Error x
end
