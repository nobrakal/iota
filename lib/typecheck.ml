open Program
open Utils

module Subst = Map.Make(String)

(** Type of monomorphic types *)
type monoty =
  | V of string (** Type variable *)
  | Safet (** A safe expression *)
  | Litt of string (** A known litteral *)
  | Arrow of (monoty * monoty) (** Arrow *)

let rec string_of_monoty = function
  | V x -> "'" ^ x
  | Safet -> "Safe"
  | Litt l -> "Lit " ^ l
  | Arrow (x,y) -> "(" ^ string_of_monoty x ^ ") -> (" ^ string_of_monoty y ^ ")"

type scheme = S of string list * monoty

let internal_counter = ref 0

let fresh_ty () =
  internal_counter := !internal_counter + 1;
  V ("_" ^ string_of_int ! internal_counter)

let rec fv_of_ty = function
  | Litt _ | Safet -> StringSet.empty
  | V x -> StringSet.singleton x
  | Arrow (x,y) -> StringSet.union (fv_of_ty x) (fv_of_ty y)

let fv_of_scheme (S (xs,x)) =
  StringSet.diff (fv_of_ty x) (StringSet.of_list xs)

let scheme_of_mono x = S ([],x)

let apply_subst_ty subst =
  let rec aux = function
    | Litt _ | Safet as t -> t
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
  let subst = List.fold_left (fun s k -> Subst.add k (fresh_ty ()) s) Subst.empty xs in
  apply_subst_ty subst x

let bindvar u t =
  if (V u) = t
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
    | V x,y | y, V x -> bindvar x y
    | _ -> raise Exit
  in aux (x,y)

let ty_safe = Safet

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
      types:(Sum_types.ty_dec list) -> t program -> (t program,type_error) result
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

  let ti_var ~types env x =
    let rec aux x =
      match x with
      | Program.V x ->
         begin match M.find_opt x env with
         | None -> raise (Te (UnboundVar x))
         | Some t -> instanciate t,Subst.empty end
      | Func (f,x) ->
         let tx,sx = aux x in
         begin match List.find_opt (fun (_,xs) -> List.exists (fun (n,_) -> f = n) xs) types with
         | Some (start,xs) ->
            let endt = List.assoc f xs in
            let s = unify tx (Litt start) in
            (Litt endt),(compose_subst s sx)
         | None -> failwith "todo: unbound function" end
      | Parent (startt,endt,x) ->
         let tx,sx = aux x in
         begin match List.find_opt (fun (x,xs) -> x=startt && List.exists (fun (_,n) -> endt = n) xs) types with
         | Some _ ->
            let s = unify tx (Litt endt) in
            (Litt startt),(compose_subst s sx)
         | None -> failwith "Undefined parent" end
    in aux x

  let variables_of_lit = function
    | Stat (_,x) -> [x]
    | Dyn (_,x) ->
       match x with
       | Has x -> [x]
       | Bin (_,x,y) -> [x; y]
       | Other (_,x) -> [x]

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
         ty_safe,ti_lit ~predicates ~types env x
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
         let s = try_unify t ty_safe in
         ty_safe,union s (union subst b)
      | Formula f ->
         fold_formula (aux env) (fun x -> x) (fun _ (_,x) (_,y) -> ty_safe, union x y) f
    in aux env x

  let type_of_def ~predicates ~types env args body =
    let args = List.map (fun x -> x,fresh_ty ()) args in
    let env = List.fold_left (fun acc (x,v) -> M.add x (scheme_of_mono v) acc) env args in
    let t,s = ti_safe ~predicates ~types env body in
    let t = applys (List.map (fun (_,v) -> apply_subst_ty s v) args) t in
    t,s

  (* permutations *)
  let distribute c l =
    let rec insert acc1 acc2 = function
      | [] -> acc2
      | hd::tl ->
         insert (hd::acc1) ((List.rev_append acc1 (hd::c::tl)) :: acc2) tl
    in
    insert [] [c::l] l

  let rec permutation = function
    | [] -> [[]]
    | hd::tl ->
       List.fold_left (fun acc x -> List.rev_append (distribute hd x) acc) [] (permutation tl)
  (* *)

  module GI = Guard_inference.Make(Manip)

  (* We also quantify for things that can be functions symbol *)
  let ti_let ~predicates ~types env (Def (name,args,body) as d) =
    let rec try_permut xs =
      match xs with
      | [] -> Some []
      | x::xs ->
         Option.bind
           (GI.infer_guard x xs body)
           (fun e -> Option.map (fun xs -> (x,e) :: xs) (try_permut xs)) in
    let newformula xs =
      List.fold_left (fun acc (e,y) -> Quantif (Exists,e,y,acc)) body xs in
    let body,(t,s) =
      try body,type_of_def ~predicates ~types env args body
      with
      | Te (UnboundVar _) as e ->
         let fv = fv_of_def d in
         let perm = permutation (Seq.fold_left (fun y x -> x::y) [] (S.to_seq fv)) in
         let opt =
           List.fold_left
             (fun acc x ->
               match acc with
               | Some _ -> acc
               | None ->
                  Option.map
                    (fun xs -> let n = newformula xs in (n,type_of_def ~predicates ~types env args n))
                    (try_permut x)
             ) None perm in
         begin match opt with
         | None -> raise e
         | Some x -> x end
      | x -> raise x in
    let t' = generalize (apply_subst_env s env) t in
    let env' = M.add name t' env in
    Def (name,args,body),s,env'

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
      if t <> ty_safe
      then raise (Te (WrongType (t,ty_safe))) in
    List.iter aux xs

  let typecheck_program ~predicates ~types ({vars; safe; _} as e) =
    let env = M.empty in
    try
      let vars,s,env = ti_lets ~predicates ~types env vars in
      let env = apply_subst_env s env in
      verify_safe ~predicates ~types env safe; Ok {e with vars}
    with
    | Te x -> Error x
end
