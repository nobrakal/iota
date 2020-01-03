open Program

type 'a final_program =
  { fsafe : ('a, 'a lit) pre_safe list
  ; fensure : 'a lit general list
  ; fmaintain : 'a lit general list }

type ('a,'l) nf =
  | Safe of ('a,'l) pre_safe
  | Closure of 'a * 'a list * ('a,'l) pre_safe

let safe_of_nf = function
  | Safe x -> x
  | Closure _ -> assert false

let join_closure x xs = function
  | Safe body -> Closure (x,xs,body)
  | Closure (y,ys,body) -> Closure (x,xs@(y::ys),body)

(* The program needs to typecheck ! *)
let rec normal_form vars = function
  | Leaf x -> Safe (Leaf x)
  | Var x ->
     begin try List.assoc x vars
     with Not_found -> Safe (Var x) end
  | Apply (x,y) ->
     let y = normal_form vars y in
     begin match normal_form vars x with
     | Closure (arg,args,body) ->
        let vars = (arg,y)::vars in
        let body = normal_form vars body in
        begin match args with
        | [] -> body
        | x::xs -> join_closure x xs body end
     | x -> x end
  | Forall (x,f,y) ->
     let vars = List.remove_assoc x vars in
     Safe (Forall (x,f, safe_of_nf (normal_form vars y)))
  | Exists (x,f,y) ->
     let vars = List.remove_assoc x vars in
     Safe (Exists (x,f, safe_of_nf (normal_form vars y)))
  | Pand (x,y) ->
     let x = safe_of_nf (normal_form vars x) in
     let y = safe_of_nf (normal_form vars y) in
     Safe (Pand (x,y))
  | Por (x,y) ->
     let x = safe_of_nf (normal_form vars x) in
     let y = safe_of_nf (normal_form vars y) in
     Safe (Por (x,y))

let inline_vars_in_vars vars =
  let aux vars (Def (name,args,body)) =
    let args' = List.map (fun x -> x,(Safe (Var x))) args in
    let body = normal_form (args'@vars) body in
    let closure =
      match List.rev args with
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

let print_final {fsafe; _} =
  let id x = x in
  List.iter (fun x -> print_safe (print_lit id) id x; Printf.printf ";\n") fsafe
