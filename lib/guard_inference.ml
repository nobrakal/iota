open Program
open Utils
open Final_def

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

module M = Manip(String)

type err = Err of string list

let string_of_err (Err xs) =
  "Could not infer guards for " ^ String.concat "," xs

(** [infer_guard v fv x] Infer a guard for variable [v] with free variables [fv] in [x].
      [v] must be free in [x]. *)
let infer_guard v fv x =
  let fv = M.S.of_list fv in
  let lit fv l =
    match l with
    | Stat _ | Dyn (true,_) -> []
    | Dyn (false,x) ->
       match x with
       | Has _ | Other _ -> []
       | Bin (b,x,y) ->
          let x' = extract_var x in
          let y' = extract_var y in
          if (v = x' && v <> y' && not (M.S.mem y' fv))
             || (v = y' && v <> x' && not (M.S.mem x' fv))
          then [(b,x,y)]
          else [] in
  let binop b x y =
    match b with
    | Or -> x@y (* TODO *)
    | And ->
       match x with
       | [] -> y
       | _ -> x in
  let rec aux fv x =
    match x with
    | FBinop (b,x,y) ->
       binop b (aux fv x) (aux fv y)
    | FQuantif (_,y,_,f) ->
       aux (M.S.add y fv) f
    | FLeaf (i,l) ->
       match i with
       | N -> []
       | E -> lit fv l
  in
  match aux fv x with
  | [] -> None
  | xs -> Some xs

let try_permut body xs =
  let rec aux xs =
    match xs with
    | [] -> Some []
    | x::xs ->
       Option.bind
         (infer_guard x xs body)
         (fun e -> Option.map (fun xs -> (x,e) :: xs) (aux xs))
  in aux xs

let newformula (body : (string, (string, binpred) lit) fsafe) xs =
  List.fold_right (fun (e,y) acc -> FQuantif (Exists,e,y,acc)) xs body

exception Err of string list

let negate_tag = function
  | N -> E
  | E -> N

let rec negate x =
  match x with
  | FLeaf (x,y) -> FLeaf (negate_tag x, y)
  | FQuantif (q,x,b,f) ->
     let q = match q with
       | Forall -> Exists
       | Exists -> Forall in
     FQuantif (q, x, b, negate f)
  | FBinop (b,x,y) ->
     let b = match b with
       | And -> Or
       | Or -> And in
     FBinop (b, negate x, negate y)

let rec run_infer x =
  match x with
  | PLeaf y -> FLeaf (E,y)
  | PQuantif (q,x,g,f) -> FQuantif (q,x,g,run_infer f)
  | PFormula f -> fold_formula run_infer negate (fun b x y -> FBinop (b,x,y)) f
  | PBracket (fv,f) ->
     let f = run_infer f in
     let perm = permutation fv in
     let opt =
       let aux x = Option.map (newformula f) (try_permut f x) in
       fold_opt aux perm in
     match opt with
     | None -> raise (Err fv)
     | Some x -> x

let run ({fsafe;fensure;fmaintain} : string pre_final_program) : (string final_program, err) result =
  try
    let fsafe = List.map (fun x -> replace_generated_vars (run_infer x)) fsafe in
    let fensure = fensure in
    let fmaintain = fmaintain in
    Ok {fsafe;fensure;fmaintain}
  with
    Err fv -> Error (Err fv)
