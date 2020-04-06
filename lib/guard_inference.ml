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

module type Guard_inference = sig
  type t
  module M : Manip with type t = t

  type err

  val string_of_err : (t -> string) -> err -> string

  val run : t pre_final_program -> (t final_program,err) result
end

module Make(M : Manip) : Guard_inference with type t = M.t = struct
  type t = M.t
  module M = M

  type err = Err of t list

  let string_of_err f (Err xs) =
    let str = List.fold_right (fun x acc -> f x ^ " " ^ acc) xs "" in
    "Could not infer guards for " ^ str

  (** [infer_guard v fv x] Infer a guard for variable [v] with free variables [fv] in [x].
      [v] must be free in [x]. *)
  let infer_guard v fv x =
    let fv = M.S.of_list fv in
    let lit l =
      match l with
      | Stat _ | Dyn (true,_) -> None
      | Dyn (false,x) ->
         match x with
         | Has _ | Other _ -> None
         | Bin (b,x,y) ->
            let x' = extract_var x in
            let y' = extract_var y in
             if (v = x' && v <> y' && not (M.S.mem y' fv))
               || (v = y' && v <> x' && not (M.S.mem x' fv))
            then Some (b,x,y)
            else None in
    let binop b x y =
      match b with
      | Or -> None (* TODO *)
      | And ->
         match x with
         | None -> y
         | _ -> x in
    let rec aux x = (* return (was_negated, guard) option *)
      match x with
      | `FBinop (b,x,y) -> binop b (aux x) (aux y)
      | `FQuantif _ -> None (* TODO verify *)
      | `FLeaf (i,l) ->
         match i with
         | N -> None
         | E -> lit l
    in
    aux x

  let try_permut body xs =
    let rec aux xs =
      match xs with
      | [] -> Some []
      | x::xs ->
         Option.bind
           (infer_guard x xs body)
           (fun e -> Option.map (fun xs -> (x,e) :: xs) (aux xs))
    in aux xs

  let newformula (body : (t, (t, binpred) lit) fsafe) xs =
    List.fold_left (fun acc (e,y) -> `FQuantif (Exists,e,y,acc)) body xs

  exception E of t list

  let rec run_infer x =
    match x with
    | `FLeaf (x,y) -> `FLeaf (x,y)
    | `FQuantif (q,x,g,f) -> `FQuantif (q,x,g,run_infer f)
    | `FBinop (b,x,y) -> `FBinop (b,run_infer x,run_infer y)
    | `FBracket (fv,f) ->
       let f = run_infer f in
       let perm = permutation fv in
       let opt =
         let aux x = Option.map (newformula f) (try_permut f x) in
         fold_opt aux perm in
       match opt with
       | None -> raise (E fv)
       | Some x -> x

  let run ({fsafe;fensure;fmaintain} : t pre_final_program) : (t final_program, err) result =
    try
      let fsafe = List.map run_infer fsafe in
      let fensure = fensure in
      let fmaintain = fmaintain in
      Ok {fsafe;fensure;fmaintain}
    with
      E fv -> Error (Err fv)
end
