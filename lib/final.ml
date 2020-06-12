open Program
open Typecheck
open Final_def
open Utils
open Config

type err =
  | UnknownType of string
  | EmptyTLink

let string_of_err = function
  | UnknownType s -> "UnknownType " ^ s
  | EmptyTLink -> "A TLink operator is empty because too constrained"

exception E of err

(* intermediate language allowing bracket *)
type ('a,'l) intermediary =
  | IFormula of ('a,'l) intermediary formula
  | ILeaf of 'l
  | IVar of 'a var
  | IApply of ('a,'l) intermediary * ('a,'l) intermediary
  | IQuantif of quantif * 'a * ('a, rbinpred) guard list * ('a,'l) intermediary
  | IBracket of 'a list *  ('a,'l) intermediary

type ('a,'l) nf =
  | Safe of ('a,'l) intermediary
  | Closure of 'a * 'a list * ('a,'l) intermediary

let safe_of_nf = function
  | Safe x -> x
  | _ -> assert false

let join_closure x xs = function
  | Safe body -> Closure (x,xs,body)
  | Closure (y,ys,body) -> Closure (x,xs@(y::ys),body)

let var_of_safe = function
  | Safe (IVar x) -> x
  | _ -> assert false

let replace_vars vars x =
  let replace x =
    try fold_var (fun x -> var_of_safe (List.assoc x vars)) x
    with Not_found -> x in
  map_lit replace x

(*  The program needs to typecheck ! *)
let rec normal_form vars = function
  | ILeaf x -> Safe (ILeaf (replace_vars vars x))
  | IVar x ->
     begin match x with
     | V xx ->
        begin try List.assoc xx vars
        with Not_found -> Safe (IVar x) end
     | _ -> Safe (IVar x) end
  | IApply (x,y) ->
     begin match normal_form vars x with
     | Closure (arg,args,body) ->
        let y = normal_form vars y in
        let body = normal_form ((arg,y)::vars) body in
        begin match args with
        | [] -> body
        | x::xs -> join_closure x xs body end
     | Safe x -> Safe (IApply (x,y))
     end
  | IQuantif (q,x,f,y) ->
     let vars = List.remove_assoc x vars in
     Safe (IQuantif (q,x,f, safe_of_nf (normal_form vars y)))
  | IFormula f ->
     Safe (IFormula (map_formula (fun x -> safe_of_nf (normal_form vars x)) f))
  | IBracket (fv,body) ->
     let vars = List.fold_left (fun acc x -> List.remove_assoc x acc) vars fv in
     Safe (IBracket (fv, safe_of_nf (normal_form vars body)))

let rec inj_intermediate x =
  match x with
  | Leaf x -> ILeaf x
  | Var x -> IVar x
  | Apply (x,y) -> IApply (inj_intermediate x, inj_intermediate y)
  | Quantif (q,x,f,y) -> IQuantif (q,x,f, inj_intermediate y)
  | Formula f -> IFormula (map_formula inj_intermediate f)

let inline_vars_in_vars vars =
  let aux vars (RDef (name,args,body)) =
    let args' = List.map (fun x -> x,Safe (IVar (V x))) args in
    let args' = args'@vars in
    let body =
      match body with
      | F body -> normal_form (args'@vars) (inj_intermediate body)
      | Bracket (fv,body) ->
         normal_form (List.fold_right List.remove_assoc fv (args'@vars)) @@
         IBracket (fv, inj_intermediate body) in
    let closure =
      match args with
      | [] -> body
      | x::xs -> join_closure x xs body in
    (name,closure)::vars
  in
  List.fold_left aux [] vars

let dyn = function
  | Has x -> Ok (Has x)
  | Other (s,x) -> Ok (Other (s,x))
  | Bin (B x, a, b) -> Ok (Bin (x,a,b))
  | Bin (TLink (s1,s2), a, b) -> Error ((s1,s2),a,b)

let lit' = function
  | Stat (s,x) -> Ok (Stat (s,x))
  | Dyn (b,d) ->
     match dyn d with
     | Ok x -> Ok (Dyn (b,x))
     | Error x -> Error (b,x)

let extract_possible_child x =
  let open Config in function
  | Simple (f,ft) -> [Func (f,None,x),ft]
  | Multiple (f,i,ft) ->
     List.init i (fun i -> Func (f,Some i,x),ft)

let bind x f = List.(concat (map f x))

let rec iter_bind n x f =
  if n = 0
  then x
  else x @ iter_bind (n-1) (bind x f) f

let mem_opt x = function
  | None -> true
  | Some xs -> StringSet.mem x xs

let is_linkable links x y =
  mem_opt y (StringMap.find_opt x  links) && mem_opt x (StringMap.find_opt y links)

let fold_paths ~config fold link x y =
  let link' (x,xt) (y,yt) =
    if is_linkable config.links xt yt
    then Some (link x y)
    else None in
  let paths x =
    iter_bind config.maxdepth [x]
      (fun (x,xt) ->
        match Utils.StringMap.find_opt xt config.types with
        | Some xs -> List.(concat (map (extract_possible_child x) xs))
        | None -> raise (E (UnknownType xt))) in
  let xs = paths x in
  let ys = paths y in
  let aux x acc y =
    match acc,link' x y with
    | None,None -> None
    | Some x,None| None,Some x -> Some x
    | Some x,Some acc -> Some (fold acc x) in
  let res = List.fold_left (fun acc x -> List.fold_left (aux x) acc ys) None xs in
  match res with
  | Some x -> x
  | None -> raise (E EmptyTLink)

let lit ~config x =
  match lit' x with
  | Ok x -> Lit x
  | Error (b, ((s1,s2),x,y)) ->
     let link x y = Lit (Dyn (b,(Bin (Link, x, y)))) in
     let fold a b = Binop (Or,a,b) in
     fold_paths ~config fold link (x,s1) (y,s2)

let remove_tlink ~config f =
  fold_formula (lit ~config) (fun x -> Not x) (fun b x y -> Binop (b,x,y)) f

let fsafe_of_safe ~config x =
  let quantif g =
    match g with
    | (B g,x,y) -> [(g,x,y)]
    | (TLink (s1,s2),x,y) ->
       let link x y = [Link, x, y] in
       let fold a b = a @ b in
       fold_paths ~config fold link (x,s1) (y,s2) in
  let rec aux x =
    match x with
    | IVar _ | IApply _ ->
       assert false
    | ILeaf y ->
       PFormula (map_formula (fun x -> PLeaf x) (lit ~config y))
    | IQuantif (q,x,g,a) ->
       PQuantif(q,x,List.(concat (map quantif g)), aux a)
    | IFormula f ->
       PFormula (map_formula aux f)
    | IBracket (fv,body) ->
       PBracket (fv, aux body)
  in aux x

let conj xs =
  let fmap = List.map in
    fmap List.rev @@ (* NB: this conjunction keeps the order when distributing (Not very monadic...) *)
      List.fold_left
        (fun acc x -> bind acc (fun ys -> fmap (fun y -> y::ys) x))
        ([[]]) xs

let remove_tlink_general ~config (General (xs,f)) =
  let f = remove_tlink ~config f in
  let fold = List.append in
  let link x y = [(Link,x,y)] in
  let aux x =
    match x with
    | (B b, x, y) -> [(b,x,y)]
    | (TLink (s1,s2), x, y) -> fold_paths ~config fold link (x,s1) (y,s2) in
  let xs = conj (List.map aux xs) in
  List.map (fun x -> General (x, f)) xs

let rec simpl_parent_func = function
  | V x -> V x
  | Parent (_,_,Func(_,_,x)) -> simpl_parent_func x
  | Parent (s1,s2,x) -> Parent (s1,s2,simpl_parent_func x)
  | Func (f,i,x) -> Func (f,i,simpl_parent_func x)

let simpl_parent_func_b (b,x,y) = (b, simpl_parent_func x, simpl_parent_func y)

let simpl_parent_func_p = map_lit simpl_parent_func

let rec simpl_parent_func_s (x : ('a,'l) pre_fsafe) =
  match x with
  | PLeaf x -> PLeaf (simpl_parent_func_p x)
  | PQuantif (q,x,g,f) -> PQuantif (q,x,List.map simpl_parent_func_b g, simpl_parent_func_s f)
  | PBracket (xs,f) -> PBracket (xs, simpl_parent_func_s f)
  | PFormula f -> PFormula (map_formula simpl_parent_func_s f)

let simpl_parent_func_g (General (xs,f)) =
  let xs = List.map simpl_parent_func_b xs in
  let f = map_formula simpl_parent_func_p f in
  General (xs,f)

let final_of_program ~config ({tvars;tsafe;tensure;tmaintain} : string Typecheck.typed_program)
    : (string Final_def.pre_final_program,err) result =
  let vars = inline_vars_in_vars tvars in
  try
    let fsafe =
      List.map (fun x ->
          simpl_parent_func_s
          @@ fsafe_of_safe ~config
          @@ safe_of_nf
          @@ normal_form vars
          @@ inj_intermediate x) tsafe in
    let fensure =
      List.map simpl_parent_func_g @@
        bind tensure (fun x -> remove_tlink_general ~config x) in
    let fmaintain =
      List.map simpl_parent_func_g @@
        bind tmaintain (fun x -> remove_tlink_general ~config x) in
    Ok {fsafe; fensure; fmaintain}
  with
  | E e -> Error e
