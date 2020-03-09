open Program

module type Guard_inference = sig
  type t
  module M : Manip with type t = t

  val infer_guard : t -> t list -> (t, (t, rbinpred) lit) pre_safe -> (t,rbinpred) guard option
end

module Make(M : Manip) : Guard_inference with type t = M.t = struct
  type t = M.t
  module M = M

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
end
