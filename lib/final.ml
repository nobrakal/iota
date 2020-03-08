open Program

(* Not and Exact *)
type tag = N | E

type ('a,'l) pre_fsafe =
  | FLeaf of tag * 'l
  | FForall of 'a * ('a, binpred) guard * ('a,'l) pre_fsafe
  | FExists of 'a * ('a, binpred) guard * ('a,'l) pre_fsafe
  | FBinop of binop * ('a,'l) pre_fsafe * ('a,'l) pre_fsafe

let par x = "(" ^ x ^ ")"
let space x = " " ^ x ^ " "

let string_of_tag = function
  | N -> "not "
  | E -> ""

let string_of_fsafe f e u =
  let rec aux = function
    | FLeaf (x,y) -> string_of_tag x ^ par (f y)
    | FForall (a,g,x) ->
       "forall" ^ space (e a) ^ string_of_guard e string_of_binpred g ^ space "->" ^ par (aux x)
    | FExists (a,g,x) ->
       "exists" ^ space (e a) ^ string_of_guard e string_of_binpred g ^ space "&&" ^ par (aux x)
    | FBinop (b,x,y) -> par (aux x) ^ space (string_of_binop b) ^ par (aux y)
  in aux u

let print_fsafe f e x = print_endline (string_of_fsafe f e x)

type 'a final_program =
  { fsafe : ('a, ('a, binpred) lit) pre_fsafe list
  ; fensure : ('a, ('a, binpred) lit, binpred) general list
  ; fmaintain : ('a, ('a, binpred) lit, binpred) general list }

type ('a,'l) nf =
  | Safe of ('a,'l) pre_safe
  | Closure of 'a * 'a list * ('a,'l) pre_safe

let safe_of_nf = function
  | Safe x -> x
  | Closure _ -> assert false

let join_closure x xs = function
  | Safe body -> Closure (x,xs,body)
  | Closure (y,ys,body) -> Closure (x,xs@(y::ys),body)

let var_of_safe = function
  | Safe (Var x) -> x
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
  | FLeaf (x,y) -> FLeaf (negate_tag x, y)
  | FForall (x,b,f) -> FExists (x, b, negate f)
  | FExists (x,b,f) -> FForall (x, b, negate f)
  | FBinop (b,x,y) ->
     let b = match b with
       | And -> Or
       | Or -> And in
     FBinop (b, negate x, negate y)

(*  The program needs to typecheck ! *)
let rec normal_form vars = function
  | Leaf x -> Safe (Leaf (replace_vars vars x))
  | Var x ->
     begin try List.assoc x vars
     with Not_found -> Safe (Var x) end
  | Apply (x,y) ->
     begin match normal_form vars x with
     | Closure (arg,args,body) ->
        let y = normal_form vars y in
        let vars = (arg,y)::vars in
        let body = normal_form vars body in
        begin match args with
        | [] -> body
        | x::xs -> join_closure x xs body end
     | Safe x -> Safe (Apply (x,y)) end
  | Forall (x,f,y) ->
     let vars = List.remove_assoc x vars in
     Safe (Forall (x,f, safe_of_nf (normal_form vars y)))
  | Exists (x,f,y) ->
     let vars = List.remove_assoc x vars in
     Safe (Exists (x,f, safe_of_nf (normal_form vars y)))
  | Formula f ->
     Safe (Formula (map_formula (fun x -> safe_of_nf (normal_form vars x)) f))

let inline_vars_in_vars vars =
  let aux vars (Def (name,args,body)) =
    let args' = List.map (fun x -> x,Safe (Var x)) args in
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

let bind x f = List.(concat (map f x))

let rec iter_bind n x f =
  if n = 0
  then x
  else iter_bind (n-1) (bind x f) f

let fold_paths ~maxprof ~types fold link x y =
  let link x y = link (fst x) (fst y) in
  let paths x =
    iter_bind maxprof [x]
      (fun (x,xt) ->
        match List.assoc_opt xt types with
        | Some xs -> List.map (fun (f,ft) -> Func (f,x),ft) xs
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
  | Parent (_,_,Func(_,x)) -> simpl_parent_func x
  | Parent (s1,s2,x) -> Parent (s1,s2,simpl_parent_func x)
  | Func (f,x) -> Func (f, simpl_parent_func x)

let simpl_parent_func_b (b,x,y) = (b, simpl_parent_func x, simpl_parent_func y)

let simpl_parent_func_p = map_lit simpl_parent_func

let rec simpl_parent_func_s x =
  match x with
  | FLeaf (t,x) -> FLeaf (t, simpl_parent_func_p x)
  | FForall (x,g,f) -> FForall (x,simpl_parent_func_b g, simpl_parent_func_s f)
  | FExists (x,g,f) -> FExists (x,simpl_parent_func_b g, simpl_parent_func_s f)
  | FBinop (b,x,y) -> FBinop (b, simpl_parent_func_s x, simpl_parent_func_s y)

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
       let fold a b = FBinop (And, a, b) in
       fold_paths ~maxprof ~types fold link (x,s1) (y,s2) in
  let rec aux x =
    match x with
    | Var _ | Apply _ ->
       assert false
    | Leaf y ->
       fold_formula (fun x -> FLeaf (E,x)) negate (fun a b c -> FBinop (a,b,c)) (lit ~maxprof ~types y)
    | Forall (x,g,a) ->
       quantif (fun g -> FForall (x, g, aux a)) g
    | Exists (x,g,a) ->
       quantif (fun g -> FExists (x, g, aux a)) g
    | Formula f ->
       fold_formula aux
         (fun x -> negate x)
         (fun b x y -> FBinop (b,x,y)) f
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

let final_of_program ~maxprof ~types ({vars;safe;ensure;maintain} : string program) : string final_program =
  let vars = inline_vars_in_vars vars in
  let fsafe =
    List.map
      (fun x -> simpl_parent_func_s (fsafe_of_safe ~maxprof ~types (safe_of_nf (normal_form vars x)))) safe in
  let fensure =
    List.map simpl_parent_func_g
      (bind ensure (fun x -> remove_tlink_general ~maxprof ~types x)) in
  let fmaintain =
    List.map simpl_parent_func_g
      (bind maintain (fun x -> remove_tlink_general ~maxprof ~types x)) in
  {fsafe; fensure; fmaintain}

let string_of_final ({fsafe; fensure; fmaintain} : string final_program) =
  let string_of_list f xs = String.concat ";\n" @@
    List.map f xs in
  let id x = x in
  string_of_list (string_of_fsafe (string_of_lit id string_of_binpred) id) fsafe
  ^ "\nensure\n" ^
  string_of_list (string_of_general (string_of_lit id string_of_binpred) id string_of_binpred) fensure
  ^ "\nmaintain\n" ^
   string_of_list (string_of_general (string_of_lit id string_of_binpred) id string_of_binpred) fmaintain

let print_final x = print_endline (string_of_final x)
