open Program

type ('a,'l) pre_fsafe =
  | FLeaf of 'l formula
  | FForall of 'a * ('a, binpred) guard * ('a,'l) pre_fsafe
  | FExists of 'a * ('a, binpred) guard * ('a,'l) pre_fsafe
  | FBinop of binop * ('a,'l) pre_fsafe * ('a,'l) pre_fsafe

let par x = "(" ^ x ^ ")"
let space x = " " ^ x ^ " "

let string_of_fsafe f e u =
  let rec aux = function
    | FLeaf x -> string_of_formula f x
    | FForall (a,g,x) ->
       "forall " ^ e a ^ ". " ^ par (string_of_guard e string_of_binpred g) ^ space "->" ^ par (aux x)
    | FExists (a,g,x) ->
       "exists " ^ e a ^ ". " ^ par (string_of_guard e string_of_binpred g) ^ space "&&" ^ par (aux x)
    | FBinop (b,x,y) -> par (aux x) ^ space (string_of_binop b) ^ par (aux y)
  in aux u

let print_fsafe f e x = print_endline (string_of_fsafe f e x)

type 'a final_program =
  { fsafe : ('a, ('a, binpred) lit) pre_fsafe list
  ; fensure : ('a, ('a, binpred) lit) general list
  ; fmaintain : ('a, ('a, binpred) lit) general list }

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

let replace_vars vars =
  let replace x =
    try map_var (fun x -> var_of_safe (List.assoc x vars)) x
    with Not_found -> x in
  let dyn = function
    | Has x -> Has (replace x)
    | Bin (b,x,y) -> Bin (b,replace x, replace y)
    | Other (s,x) -> Other (s, replace x) in
  let lit = function
    | Stat (x,i) -> Stat (x, replace i)
    | Dyn (b,x) -> Dyn (b, dyn x) in
  fold_formula (fun x -> Lit (lit x)) (fun x -> Not x) (fun x y z -> Binop (x,y,z))

let rec negate x =
  match x with
  | FLeaf _ -> x
  | FForall (x,b,f) -> FExists (x, b, negate f)
  | FExists (x,b,f) -> FForall (x, b, negate f)
  | FBinop (b,x,y) ->
     let b = match b with
       | And -> Or
       | Or -> And in
     FBinop (b, negate x, negate y)

let rec fsafe_of_safe x =
  match x with
  | Leaf x -> FLeaf x
  | Forall (x,g,a) -> FForall (x, g, fsafe_of_safe a)
  | Exists (x,g,a) -> FExists (x, g, fsafe_of_safe a)
  | Var _ | Apply _ -> assert false
  | Formula f ->
     fold_formula (fun x -> fsafe_of_safe x)
       (fun x -> negate x)
       (fun b x y -> FBinop (b,x,y)) f

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
     Safe (Formula
             (fold_formula (fun x -> Lit (safe_of_nf (normal_form vars x)))
             (fun x -> Not x)
             (fun b x y -> Binop (b,x,y)) f))

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
  | Has x -> Has x
  | Other (s,x) -> Other (s,x)
  | Bin (B x, a, b) -> Bin (x,a,b)
  | Bin _ -> assert false

let lit = function
    | Stat (s,x) -> Stat (s,x)
    | Dyn (b,d) -> Dyn (b,dyn d)

let remove_tlink' f =
  fold_formula (fun x -> Lit (lit x)) (fun x -> Not x) (fun b x y -> Binop (b,x,y)) f

let rec remove_tlink x = match x with
  | FLeaf f -> FLeaf (remove_tlink' f)
  | FForall (a,g,x) -> FForall (a, g, remove_tlink x)
  | FExists (a,g,x) -> FExists (a, g, remove_tlink x)
  | FBinop (b,x,y) -> FBinop (b, remove_tlink x, remove_tlink y)

let remove_tlink_general (General (xs,f)) = General (xs, remove_tlink' f)

let final_of_program ({vars;safe;ensure;maintain} : string program) : string final_program =
  let vars = inline_vars_in_vars vars in
  let fsafe = List.map (fun x -> remove_tlink (fsafe_of_safe (safe_of_nf (normal_form vars x)))) safe in
  let fensure = List.map remove_tlink_general ensure in
  let fmaintain = List.map remove_tlink_general maintain in
  {fsafe; fensure; fmaintain}

let print_final ({fsafe; fensure; fmaintain} : string final_program) =
  let print_list f xs =
    List.iter (fun x -> f x; Printf.printf ";\n") xs in
  let id x = x in
  print_list (print_fsafe (string_of_lit id string_of_binpred) id) fsafe;
  Printf.printf "\nensure\n";
  print_list (print_general (string_of_lit id string_of_binpred) id) fensure;
    Printf.printf "\nmaintain\n";
  print_list (print_general (string_of_lit id string_of_binpred) id) fmaintain
