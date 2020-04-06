open Program
open Typecheck
open Final_def

(* intermediate language allowing bracket *)
type ('a,'l) intermediary =
  | IFormula of ('a,'l) intermediary formula
  | ILeaf of 'l
  | IVar of 'a
  | IApply of ('a,'l) intermediary * ('a,'l) intermediary
  | IQuantif of quantif * 'a * ('a, rbinpred) guard * ('a,'l) intermediary
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
    try map_var (fun x -> var_of_safe (List.assoc x vars)) x
    with Not_found -> x in
  map_lit replace x

let negate_tag = function
  | N -> E
  | E -> N

let rec negate x =
  match x with
  | `FLeaf (x,y) -> `FLeaf (negate_tag x, y)
  | `FQuantif (q,x,b,f) ->
     let q = match q with
       | Forall -> Exists
       | Exists -> Forall in
     `FQuantif (q, x, b, negate f)
  | `FBinop (b,x,y) ->
     let b = match b with
       | And -> Or
       | Or -> And in
     `FBinop (b, negate x, negate y)
  | `FBracket _ -> failwith "TODO: Unhandled negation"

(*  The program needs to typecheck ! *)
let rec normal_form vars = function
  | ILeaf x -> Safe (ILeaf (replace_vars vars x))
  | IVar x ->
     begin try List.assoc x vars
     with Not_found -> Safe (IVar x) end
  | IApply (x,y) ->
     begin match normal_form vars x with
     | Closure (arg,args,body) ->
        let y = normal_form vars y in
        let vars = (arg,y)::vars in
        let body = normal_form vars body in
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
    let args' = List.map (fun x -> x,Safe (IVar x)) args in
    let body =
      match body with
      | F body -> inj_intermediate body
      | Bracket (fv,body) -> IBracket (fv,inj_intermediate body) in
    let body = normal_form (args'@vars) body in
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
  else iter_bind (n-1) (bind x f) f

let fold_paths ~maxprof ~(types : Config.ty_dec list) fold link x y =
  let link x y = link (fst x) (fst y) in
  let paths x =
    iter_bind maxprof [x]
      (fun (x,xt) ->
        match List.assoc_opt xt types with
        | Some xs -> List.(concat (map (extract_possible_child x) xs))
        | None -> failwith "TODO: unknown type") in
  match paths x, paths y with
  | [],_ | _,[] -> assert false
  | x::xs,y::ys ->
     let aux x acc y = fold acc (link x y) in
     List.fold_left
       (fun acc x ->
         List.fold_left (aux x) acc (y::ys))
       (List.fold_left (aux x) (link x y) ys) xs

let lit ~maxprof ~types x =
  match lit' x with
  | Ok x -> Lit x
  | Error (b, ((s1,s2),x,y)) ->
     let link x y = Lit (Dyn (b,(Bin (Link, x, y)))) in
     let fold a b = Binop (Or,a,b) in
     fold_paths ~maxprof ~types fold link (x,s1) (y,s2)

let remove_tlink ~maxprof ~types f =
  fold_formula (lit ~maxprof ~types) (fun x -> Not x) (fun b x y -> Binop (b,x,y)) f

let rec simpl_parent_func = function
  | V x -> V x
  | Parent (_,_,Func(_,_,x)) -> simpl_parent_func x
  | Parent (s1,s2,x) -> Parent (s1,s2,simpl_parent_func x)
  | Func (f,i,x) -> Func (f,i,simpl_parent_func x)

let simpl_parent_func_b (b,x,y) = (b, simpl_parent_func x, simpl_parent_func y)

let simpl_parent_func_p = map_lit simpl_parent_func

let rec simpl_parent_func_s (x : ('a,'l) pre_fsafe) =
  match x with
  | `FLeaf (t,x) -> `FLeaf (t, simpl_parent_func_p x)
  | `FQuantif (q,x,g,f) -> `FQuantif (q,x,simpl_parent_func_b g, simpl_parent_func_s f)
  | `FBracket (xs,f) -> `FBracket (xs, simpl_parent_func_s f)
  | `FBinop (b,x,y) -> `FBinop (b, simpl_parent_func_s x, simpl_parent_func_s y)

let simpl_parent_func_g (General (xs,f)) =
  let xs = List.map simpl_parent_func_b xs in
  let f = map_formula simpl_parent_func_p f in
  General (xs,f)

let fsafe_of_safe ~maxprof ~types x =
  let quantif f g =
    match g with
    | (B g,x,y) -> f (g,x,y)
    | (TLink (s1,s2),x,y) ->
       let link x y = f (Link, x, y) in
       let fold a b = `FBinop (And, a, b) in
       fold_paths ~maxprof ~types fold link (x,s1) (y,s2) in
  let rec aux x =
    match x with
    | IVar _ | IApply _ ->
       assert false
    | ILeaf y ->
       fold_formula (fun x -> `FLeaf (E,x)) negate (fun a b c -> `FBinop (a,b,c)) (lit ~maxprof ~types y)
    | IQuantif (q,x,g,a) ->
       quantif (fun g -> `FQuantif (q, x, g, aux a)) g
    | IFormula f ->
       fold_formula aux
         (fun x -> negate x)
         (fun b x y -> `FBinop (b,x,y)) f
    | IBracket (fv,body) ->
       `FBracket (fv, aux body)
  in aux x

let conj xs =
  let fmap = List.map in
    fmap List.rev @@ (* NB: this conjunction keeps the order when distributing (Not very monadic...) *)
      List.fold_left
        (fun acc x -> bind acc (fun ys -> fmap (fun y -> y::ys) x))
        ([[]]) xs

let remove_tlink_general ~maxprof ~types (General (xs,f)) =
  let f = remove_tlink ~maxprof ~types f in
  let fold = List.append in
  let link x y = [(Link,x,y)] in
  let aux x =
    match x with
    | (B b, x, y) -> [(b,x,y)]
    | (TLink (s1,s2), x, y) -> fold_paths ~maxprof ~types fold link (x,s1) (y,s2) in
  let xs = conj (List.map aux xs) in
  List.map (fun x -> General (x, f)) xs

let final_of_program ~maxprof ~types ({tvars;tsafe;tensure;tmaintain} : string Typecheck.typed_program)
    :  string Final_def.pre_final_program =
  let vars = inline_vars_in_vars tvars in
  let fsafe =
    List.map (fun x ->
        simpl_parent_func_s
        @@ fsafe_of_safe ~maxprof ~types
        @@ safe_of_nf
        @@ normal_form vars
        @@ inj_intermediate x) tsafe in
  let fensure =
    List.map simpl_parent_func_g
      (bind tensure (fun x -> remove_tlink_general ~maxprof ~types x)) in
  let fmaintain =
    List.map simpl_parent_func_g
      (bind tmaintain (fun x -> remove_tlink_general ~maxprof ~types x)) in
  {fsafe; fensure; fmaintain}
