open Program
open Utils

type ('a,'l) pre_gsafe =
  | F of ('a,'l) pre_safe
  | Bracket of 'a list * ('a,'l) pre_safe (* existentially quantified variables, without guards *)

type ('a,'l) gdef =
  | GDef of ('a * 'a list * ('a,'l) pre_gsafe)

type 'a gfinal_program =
  { gvars : ('a, ('a, rbinpred) lit) gdef list
  ; gsafe : ('a, ('a, rbinpred) lit) pre_safe list
  ; gensure : ('a, ('a, rbinpred) lit, rbinpred) general list
  ; gmaintain : ('a, ('a, rbinpred) lit, rbinpred) general list }

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

  val run : t gfinal_program -> (t program,err) result
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
    let binop b x y =
      match b with
      | Or -> None (* TODO *)
      | And ->
         match x with
         | None | Some (true,_) -> y
         | _ -> x in
    let rec aux x = (* return (was_negated, guard) option *)
      match x with
      | Var _ -> None
      | Leaf l ->
         begin match l with
         | Stat _ | Dyn (true,_) -> None
         | Dyn (false,x) ->
            match x with
            | Has _ | Other _ -> None
            | Bin (b,x,y) ->
               let x' = extract_var x in
               let y' = extract_var y in
               if (v = x' && v <> y' && not (M.S.mem y' fv))
                  || (v = y' && v <> x' && not (M.S.mem x' fv))
               then Some (false,(b,x,y))
               else None end
      | Formula f ->
         fold_formula
           aux
           (Option.map (fun (x,y) -> (not x, y)))
           binop f
      | Quantif _ -> None (* TODO verify *)
      | Apply _ -> None (* TODO This should not be the case *)
    in
    match aux x with
    | None | Some (true,_) -> None
    | Some (false,x) -> Some x

  let try_permut body xs =
    let rec aux xs =
      match xs with
      | [] -> Some []
      | x::xs ->
         Option.bind
           (infer_guard x xs body)
           (fun e -> Option.map (fun xs -> (x,e) :: xs) (aux xs))
    in aux xs

  let newformula body xs =
    List.fold_left (fun acc (e,y) -> Quantif (Exists,e,y,acc)) body xs

  exception E of t list

  let letdef (GDef (a,b,c)) =
    let body =
      match c with
      | F c -> c
      | Bracket (fv,body) ->
         let perm = permutation fv in
         let opt =
           let aux x = Option.map (newformula body) (try_permut body x) in
           fold_opt aux perm in
         match opt with
         | None -> raise (E fv)
         | Some x -> x in
    Def (a,b,body)

  let run {gvars;gsafe;gensure;gmaintain} =
    try
      let vars = List.map letdef gvars in
      let safe = gsafe in
      let ensure = gensure in
      let maintain = gmaintain in
      Ok {vars;safe;ensure;maintain}
    with
      E fv -> Error (Err fv)
end
