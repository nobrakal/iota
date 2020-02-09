open Program

type 'a final_program =
  { fsafe : ('a, 'a lit) pre_safe list
  ; fensure : ('a, 'a lit) general list
  ; fmaintain : ('a, 'a lit) general list }

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
  fold_formula (fun x -> Lit (lit x)) (fun x y -> Unop (x,y)) (fun x y z -> Binop (x,y,z))

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
  | Pbin (b,x,y) ->
     let x = safe_of_nf (normal_form vars x) in
     let y = safe_of_nf (normal_form vars y) in
     Safe (Pbin (b,x,y))

let inline_vars_in_vars vars =
  let aux vars (Def (name,args,body)) =
    let args' = List.map (fun x -> x,(Safe (Var x))) args in
    let body = normal_form (args'@vars) body in
    let closure =
      match args with
      | [] -> body
      | x::xs -> join_closure x xs body in
    (name,closure)::vars
  in
  List.fold_left aux [] vars

let final_of_program {vars;safe;ensure;maintain} =
  let vars = inline_vars_in_vars vars in
  let fsafe = List.map (fun x -> safe_of_nf (normal_form vars x)) safe in
  let fensure = ensure in
  let fmaintain = maintain in
  {fsafe; fensure; fmaintain}

let print_final {fsafe; fensure; fmaintain} =
  let print_list f xs =
    List.iter (fun x -> f x; Printf.printf ";\n") xs in
  let id x = x in
  print_list (print_safe (print_lit id) id) fsafe;
  Printf.printf "\nensure\n";
  print_list (print_general (print_lit id) id) fensure;
    Printf.printf "\nmaintain\n";
  print_list (print_general (print_lit id) id) fmaintain
