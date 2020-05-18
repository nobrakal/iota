open Program
open Utils
open Config

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

module Manip = Program.Manip(String)
open Manip

module Subst = Map.Make(String)

type ground =
  | Vt of string (** Type variable *)
  | Litt of string  (** A known litteral *)

module GroundSet = Set.Make(struct type t = ground let compare=compare end)

(** Type of monomorphic types *)
type monoty =
  | G of ground
  | Safet (** A safe expression *)
  | Arrow of (monoty * monoty) (** Arrow *)

let rec string_of_monoty = function
  | G (Vt x) -> "'" ^ x
  | Safet -> "Safe"
  | G (Litt l) -> "Lit " ^ l
  | Arrow (x,y) -> paren (string_of_monoty x) ^ " -> " ^ paren (string_of_monoty y)

type scheme = S of string list * GroundSet.t StringMap.t * monoty

let fresh_ty =
  let internal_counter = ref 0 in
  fun () ->
  internal_counter := !internal_counter + 1;
  G (Vt ("_" ^ string_of_int ! internal_counter))

let rec fv_of_ty = function
  | G (Litt _) | Safet -> StringSet.empty
  | G (Vt x) -> StringSet.singleton x
  | Arrow (x,y) -> StringSet.union (fv_of_ty x) (fv_of_ty y)

let fv_of_scheme (S (xs,_,x)) =
  StringSet.diff (fv_of_ty x) (StringSet.of_list xs)

let scheme_of_mono x = S ([],StringMap.empty,x)

let apply_subst_ty subst =
  let rec aux t =
    match t with
    | G (Litt _) | Safet -> t
    | Arrow (x,y) -> Arrow (aux x, aux y)
    | G (Vt x) ->
       match Subst.find_opt x subst with
       | None -> t
       | Some x -> x in
  aux

let apply_subst_ground subst x =
  match apply_subst_ty subst (G x) with
  | G x -> x
  | _ -> assert false

let mem_opt x = function
  | None -> true
  | Some xs -> StringSet.mem x xs

let verify_links ~links_err links x y =
  if mem_opt y (StringMap.find_opt x  links) && mem_opt x (StringMap.find_opt y links)
  then () else links_err x y

let update_links_map ~links_err clinks subst m =
  let upd vx lx = function
    | None -> Some (GroundSet.singleton (Litt lx) )(*TODO is this case possible ? *)
    | Some xs -> Some (GroundSet.(add (Litt lx) (remove (Vt vx) xs))) in
  let aux_gs vx lx g m =
    match g with
    | Litt y -> verify_links ~links_err clinks lx y; m
    | Vt y -> StringMap.update y (upd vx lx) m in
  let aux x gs m =
    let gs = GroundSet.map (apply_subst_ground subst) gs in
    match apply_subst_ground subst (Vt x) with
    | Litt lx ->
       GroundSet.fold (aux_gs x lx) gs m
    | Vt y -> StringMap.add y gs m in
  StringMap.fold aux m StringMap.empty

let apply_subst_scheme ~links_err clinks subst (S (xs,m,x)) =
  let subst = Subst.filter (fun x _ -> List.mem x xs) subst in
  let nm = update_links_map ~links_err clinks subst m in
  S (xs,nm, apply_subst_ty subst x)

let left_bias _ x _ = Some x

let compose_subst s1 s2 =
  Subst.union left_bias (Subst.map (apply_subst_ty s1) s2) s1

let instanciate ~links_err clinks (S (xs,m,x)) =
  let subst = List.fold_left (fun s k -> Subst.add k (fresh_ty ()) s) Subst.empty xs in
  let nm = update_links_map ~links_err clinks subst m in
  apply_subst_ty subst x,nm

let bindvar u t =
  if (G (Vt u)) = t
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
    | G (Litt x), G (Litt y) when x = y -> Subst.empty
    | G (Vt x),y | y, G (Vt x) -> bindvar x y
    | _ -> raise Exit
  in aux (x,y)

let applys xs fv =
  List.fold_right (fun x acc -> Arrow (x,acc)) xs fv

type type_error =
  | UnboundVar of string
  | WrongType of monoty * monoty (* actual, expected *)
  | Constraint of string * string
  | UnInstanciableVar

exception Te of type_error

let string_of_type_error = function
  | UnboundVar t ->
     "Unbound variable: " ^ t
  | WrongType (x,y) ->
     "Type error:\n"
     ^ "Expected: " ^ string_of_monoty x ^ "\n"
     ^ "Actual:   " ^ string_of_monoty y
  | Constraint (x,y) ->
     "Type error:\n"
     ^ "Trying to link " ^ x ^ " with " ^ y
  | UnInstanciableVar ->
     "Type error:\n"
     ^ "unable to instanciate the type of a free type variable"

let unify x y =
  try unify x y with
  | Exit -> raise (Te (WrongType (x,y)))

let links_err x y = raise (Te (Constraint (x,y)))

let fv_of_env x = M.fold (fun _ x s -> StringSet.union s (fv_of_scheme x)) x StringSet.empty
let apply_subst_env links s x = M.map (apply_subst_scheme ~links_err links s) x

let generalize env m t =
  let vars = StringSet.diff (fv_of_ty t) (fv_of_env env) in
  let vars = StringSet.fold (fun x xs -> x::xs) vars [] in
  (* m \incl vars *)
  S (vars,m,t)

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
  let aux start xs =
    Option.map (fun x -> start,x) (get_accessor_type' f i xs) in
  stringmap_fold_opt aux types

let exists_end_type endt xs =
  List.exists (fun x -> extract_type x = endt) xs

let ti_var ~config env x =
  let rec aux x =
    match x with
    | Program.V x ->
       begin match M.find_opt x env with
       | None -> raise (Te (UnboundVar x))
       | Some t ->
          let t,l = instanciate ~links_err config.links t in
          t,l,Subst.empty end
    | Func (f,i,x) ->
       let tx,lx,sx = aux x in
       begin match get_accessor_type f i config.types with
       | Some (start,endt) ->
          let s = unify tx (G (Litt start)) in
          (G (Litt endt)),lx,(compose_subst s sx)
       | None -> failwith "todo: unbound function" end
    | Parent (startt,endt,x) ->
       let tx,lx,sx = aux x in
       match StringMap.find_opt startt config.types with
       | Some xs when (exists_end_type endt xs) ->
          let s = unify tx (G (Litt endt)) in
          (G (Litt startt)),lx,(compose_subst s sx)
       | _ -> failwith "undefined parent"
  in aux x

let variables_of_lit = function
  | Stat (_,x) -> [x]
  | Dyn (_,x) ->
     match x with
     | Has x | Other (_,x) -> [x]
     | Bin (_,x,y) -> [x; y]

let fst3 (x,_,_) = x

let verif_pred ~predicates xs =
  function
  | Stat (s,_) | Dyn (_,Other (s,_)) ->
     let _,ty = StringMap.find s predicates in
     unify (fst3 (List.hd xs)) (G (Litt ty))
  | _ -> Subst.empty

let get_link clinks links vars =
  let update x v m =
    let a =
      match StringMap.find_opt x m with
      | None -> GroundSet.singleton v
      | Some xs -> GroundSet.add v xs in
    StringMap.add x a m in function
    | Dyn (_,Bin (B Link,_,_)) ->
       begin match fst3 (List.nth vars 0), fst3 (List.nth vars 1) with
       | G x, G y ->
          begin match x,y with
          | Litt x, Litt y ->
             verify_links ~links_err clinks x y;
             links
          | Vt x,Litt y | Litt y, Vt x ->
             update x (Litt y) links
          | Vt x, Vt y ->
             update x (Vt y) (update y (Vt x) links)
          end
       | _ -> assert false
       end
    | _ -> links

let union_links = StringMap.union (fun _ x y -> Some (GroundSet.union x y))

let ti_lit ~config env x =
  let vars = List.map (ti_var ~config env) (variables_of_lit x) in
  let subst,links =
    List.fold_left (fun (subst,links) (_,l,s) -> Subst.union left_bias s subst, union_links links l)
      (Subst.empty,StringMap.empty) vars in
  let link = get_link config.links links vars x in
  link,Subst.union left_bias (verif_pred ~predicates:config.predicates vars x) subst

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

let union_links' ~config x s1 y s2 =
  union_links (update_links_map ~links_err config.links s1 x)
    (update_links_map ~links_err config.links s2 y)

let ti_guards ~config env xs =
  let xs = List.map (fun u -> ti_lit ~config env (Dyn (false,Bin u))) xs in
  match xs with
  | [] -> assert false
  | x::xs ->
     List.fold_left
       (fun (l,subst) (l',subst') -> union_links' ~config l subst' l' subst' ,union subst subst')
       x xs

let ti_safe ~config env x =
  let rec aux env = function
    | Leaf x ->
       let links,t = ti_lit ~config env x in
       Safet,links,t
    | Var x ->
       ti_var ~config env x
    | Apply (x,y) ->
       let tv = fresh_ty () in
       let t,l,s = aux env x in
       let t',l',s' = aux (M.map (apply_subst_scheme ~links_err config.links s) env) y in
       let s'' = unify (apply_subst_ty s' t) (Arrow (t', tv)) in
       let subst = compose_subst s'' (compose_subst s' s) in
       let l'' = union_links' ~config l subst l' subst in
       apply_subst_ty s'' tv,l'', subst
    | Quantif (_,x,u,b) ->
       let nenv = M.add x (scheme_of_mono (fresh_ty ())) env in
       let l,subst = ti_guards ~config nenv u in
       let t,l',b = aux nenv b in
       let s = try_unify t Safet in
       let subst' = union s (union subst b) in
       let l'' = union_links' ~config l subst' l' subst' in
       Safet,l'',subst'
    | Formula f ->
       fold_formula (aux env) (fun x -> x)
         (fun _ (_,l1,x) (_,l2,y) -> Safet, union_links' ~config l1 x l2 y, union x y) f
  in aux env x

let type_of_def ~config env args body =
  let args = List.map (fun x -> x,fresh_ty ()) args in
  let env = List.fold_left (fun acc (x,v) -> M.add x (scheme_of_mono v) acc) env args in
  let t,links,subst = ti_safe ~config env body in
  let t = applys (List.map (fun (_,v) -> apply_subst_ty subst v) args) t in
  let links = update_links_map ~links_err config.links subst links in
  t,links,subst

let print_inf verbose name fv =
  if verbose
  then
    let fv = String.concat "," fv in
    print_warning ("The right hand side of " ^  name ^ " has free variables: " ^ fv)

let ti_let ~verbose ~infer_guards ~config env (Def (name,args,body) as e) =
  let body,(t,links,subst) =
    try F body,type_of_def ~config env args body
    with
    | Te (UnboundVar _) as err  -> (* unbound var, we try to quantify all the unbound var *)
       let fvs =
         List.filter (fun x -> not (M.mem x env)) @@
           Manip.(to_list (fv_of_def e)) in
       print_inf verbose name fvs;
       if infer_guards
       then
         let nenv = List.fold_left (fun env x -> M.add x (scheme_of_mono (fresh_ty ())) env) env fvs in
         let body' = Bracket (fvs,body) in
         body',type_of_def ~config nenv args body
       else raise err
    | x -> raise x in
  let t' = generalize (apply_subst_env config.links subst env) links t in
  let env' = M.add name t' env in
  RDef (name,args,body),subst,env'

let ti_lets ~verbose ~infer_guards ~config env xs =
  let ds,s,env =
    List.fold_left
      (fun (ds,s2,env) x ->
        let d,s1,env = ti_let ~verbose ~infer_guards ~config env x
        in d::ds,compose_subst s1 s2, env) ([],Subst.empty,env) xs
  in
  List.rev ds,s,env

let keys xs =  StringMap.fold (fun x _ acc -> x::acc) xs []

let can_instanciate_types ~types env clinks links =
  let types = List.map (fun x -> G (Litt x)) (keys types) in
  let vars = keys links in
  let try_ty x (penv,plinks) t =
    try
      let penv = (Subst.add x t penv) in
      Some (penv,update_links_map ~links_err clinks penv plinks)
    with
    | Te (Constraint _) -> None in
  let rec aux' pp x xs ts =
    match ts with
    | [] -> None
    | t::ts ->
       match try_ty x pp t with
       | None -> aux' pp x xs ts
       | Some npp ->
          let res = aux npp xs in
          match res with
          | None -> aux' pp x xs ts
          | _ -> res
  and aux pp (xs : string list) =
    match xs with
    | [] -> Some pp
    | x::xs -> aux' pp x xs types
  in
  match aux (env,links) vars with
  | Some _ -> true
  | None -> false

let verify_safe ~config env xs =
  let v_env = M.fold (fun k _ s -> S.add k s) env S.empty in
  let v_xs = List.fold_left S.union S.empty (List.map variables_of_safe xs) in
  let fv = S.diff v_xs v_env in
  let env = S.fold (fun k env -> M.add k (scheme_of_mono (fresh_ty ())) env) fv env in
  let aux x =
    let t,links,subst = ti_safe ~config env x in
    if t <> Safet
    then raise (Te (WrongType (t,Safet)));
    if not (can_instanciate_types ~types:config.types subst config.links links)
    then raise (Te (UnInstanciableVar)) in
  List.iter aux xs

let typecheck_program ~verbose ~infer_guards ~config {vars; safe; ensure; maintain} =
  let env = M.empty in
  try
    let tvars,s,env = ti_lets ~verbose ~infer_guards ~config env vars in
    let env = apply_subst_env config.links s env in
    verify_safe ~config env safe;
    let tsafe = safe in
    let tensure = ensure in
    let tmaintain = maintain in
    Ok {tvars;tsafe;tensure;tmaintain}
  with
  | Te x -> Error x
