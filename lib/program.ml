open Utils

type binop = And | Or

type binpred = Eq | Link

type quantif = Forall | Exists

type rbinpred =
  | TLink of string * string
  | B of binpred

type 'a var =
  | V of 'a
  | Parent of string * string * 'a var (* string for the expected type *)
  | Func of string * int option * 'a var

type ('a,'b) guard = 'b * 'a var * 'a var

type ('a,'b) dynamic =
  | Has of 'a var
  | Bin of ('a,'b) guard
  | Other of string * 'a var

type ('a,'b) lit =
  | Dyn of bool * ('a,'b) dynamic
  | Stat of string * 'a var

type 'a formula =
  | Lit of 'a
  | Not of 'a formula
  | Binop of binop * 'a formula * 'a formula

type ('a,'l) pre_safe =
  | Formula of ('a,'l) pre_safe formula
  | Leaf of 'l
  | Var of 'a var
  | Apply of ('a,'l) pre_safe * ('a,'l) pre_safe
  | Quantif of quantif * 'a * ('a, rbinpred) guard * ('a,'l) pre_safe

type 'a safe = ('a, ('a, rbinpred) lit) pre_safe

type ('a,'l,'b) general =
  | General of ('a, 'b) guard list * 'l formula

type ('a,'l) def =
  | Def of ('a * 'a list * ('a,'l) pre_safe)

type ('a,'l) pre_program =
  { vars : ('a,'l) def list
  ; safe : ('a, 'l) pre_safe list
  ; ensure : ('a, 'l, rbinpred) general list
  ; maintain : ('a, 'l, rbinpred) general list }

type 'a parsed_program = ('a, bool * ('a, rbinpred) dynamic) pre_program
type 'a program = ('a, ('a, rbinpred) lit) pre_program

let fold_var f x =
  let rec aux = function
    | V x -> f x
    | Parent (s1,s2,x) -> Parent (s1,s2,aux x)
    | Func (s,i,x) -> Func (s,i, aux x)
  in aux x

let map_var f x = fold_var (fun x -> V (f x)) x

let map_lit f x = match x with
  | Stat (s,v) -> Stat (s,f v)
  | Dyn (b,x) ->
     let x =
       match x with
       | Has x -> Has (f x)
       | Bin (b,x,y) -> Bin (b, f x, f y)
       | Other (s,x) -> Other (s,f x) in
     Dyn (b,x)

let rec extract_var x =
  match x with
  | V x -> x
  | Parent (_,_,x) | Func (_,_,x) -> extract_var x

let fold_formula l u b =
  let rec aux = function
    | Lit x -> l x
    | Not y -> u (aux y)
    | Binop (x,y,z) -> b x (aux y) (aux z)
  in aux

let map_formula f x =
  fold_formula (fun x -> Lit (f x)) (fun x -> Not x) (fun a b c -> Binop (a,b,c)) x

type parse_error =
  | UnboundDynamic of string
  | UnboundSymbol of string

let string_of_parse_error = function
  | UnboundDynamic s -> "Unbound dynamic: " ^ s
  | UnboundSymbol s -> "Unbound symbol: " ^ s

let string_of_var s =
  let rec aux = function
  | V x -> s x
  | Parent (s1,s2,x) -> "parent<" ^ s1  ^ "," ^ s2 ^ ">" ^ paren (aux x)
  | Func (s,i,x) ->
     let acc =
       match i with
       | None -> ""
       | Some i -> ":" ^ string_of_int i in
     aux x ^ "." ^ s ^ acc
  in aux

let string_of_binpred = function
  | Link -> "Link"
  | Eq -> "Eq"

let string_of_rbinpred = function
  | TLink (s1,s2)-> "TLink<" ^ s1 ^ "," ^ s2 ^ ">"
  | B b -> string_of_binpred b

let string_of_guard s f (b,x,y) =
  let s = string_of_var s in
  f b ^ paren ((s x) ^ "," ^ (s y))

let string_of_dynamic s f =
  let s' = string_of_var s in function
  | Has x -> "Has" ^ paren (s' x)
  | Other (x,y) -> x ^ paren (s' y)
  | Bin t -> string_of_guard s f t

let string_of_lit s f = function
  | Dyn (b,x) ->
     let pref = if b then "+" else "" in
     pref ^ string_of_dynamic s f x
  | Stat (x,y) -> x ^ paren (string_of_var s y)

let string_of_binop = function
  | And -> "&&"
  | Or -> "||"

let string_of_formula lit =
  let rec aux = function
    | Lit x -> lit x
    | Not f ->
       "not " ^ paren (aux f)
    | Binop (u,x,y) ->
       paren (aux x) ^ space (string_of_binop u) ^ paren (aux y)
  in aux

let print_formula lit x = print_endline (string_of_formula lit x)

let string_of_safe s p x =
  let rec aux = function
    | Leaf x -> s x
    | Var x -> string_of_var p x
    | Apply (x,y) ->
       paren (aux x) ^ paren (aux y)
    | Quantif (Forall,x,y,z) ->
       "forall " ^ p x  ^ paren (string_of_guard p string_of_rbinpred y) ^ paren (aux z)
    | Quantif (Exists,x,y,z) ->
       "exists " ^ p x  ^ paren (string_of_guard p string_of_rbinpred y) ^ paren (aux z)
    | Formula f -> string_of_formula aux f
  in aux x

let print_safe s p x = print_endline (string_of_safe s p x)

let string_of_general s p g (General (xs,x)) =
  let str =
    match xs with
    | [] -> ""
    | x::xs -> List.fold_left (fun acc x -> acc ^ space "->" ^ string_of_guard p g x)
                 (string_of_guard p g x) xs in
  str ^ space "=>" ^ string_of_formula s x

let print_general s p g x = print_endline (string_of_general s p g x)

exception ParseError of parse_error

let mk_lit ~static ~dynamic (b,dyn) =
  match dyn with
  | Has _ | Bin _ -> Dyn (b,dyn)
  | Other (s,x) ->
     if StringSet.mem s dynamic then Dyn (b,dyn)
     else
       if b
       then raise (ParseError (UnboundDynamic s))
       else
         if StringSet.mem s static then Stat (s,x)
         else raise (ParseError (UnboundSymbol s))

let safe_of_parsed ~static ~dynamic =
  let rec aux = function
    | Leaf x -> Leaf (mk_lit ~static ~dynamic x)
    | Var x -> Var x
    | Apply (x,y) -> Apply (aux x, aux y)
    | Quantif (q,a,l,x) -> Quantif (q, a, l, aux x)
    | Formula f -> Formula (map_formula aux f)
  in aux

let program_of_parsed ~static ~dynamic {vars; safe; ensure; maintain} =
  try
    let general (General (xs,x)) =
      General (xs,(map_formula (mk_lit ~static ~dynamic) x)) in
    let vars =
      List.map
        (fun (Def (name,args,x)) -> Def (name, args, safe_of_parsed ~static ~dynamic x)) vars in
    let safe = List.map (safe_of_parsed ~static ~dynamic) safe in
    let ensure = List.map general ensure in
    let maintain = List.map general maintain in
    Ok ({vars; safe; ensure; maintain})
  with
    ParseError s -> Error s

module type Manip =
  sig
    type t

    module S : Set.S with type elt = t
    val to_list : S.t -> S.elt list
    module M : Map.S with type key = t

    val variables_of_dynamic : (S.elt,'a) dynamic -> S.t
    val variables_of_lit : (S.elt,'a) lit -> S.t
    val variables_of_formula : (S.elt,'a) lit formula -> S.t
    val variables_of_safe : S.elt safe -> S.t

    val fv_of_safe : S.elt safe -> S.t
    val fv_of_def : (S.elt, (S.elt, rbinpred) lit) def -> S.t
  end

module Manip (V : Set.OrderedType) : Manip with type t = V.t = struct
  type t = V.t
  module S = Set.Make(V)
  module M = Map.Make(V)
  let to_list s = S.fold (fun x y -> x::y) s []

  let fold_fomula_union f x =
    fold_formula f (fun x -> x) (fun _ -> S.union) x

  let variables_of_dynamic = function
    | Has x | Other (_,x) -> S.singleton (extract_var x)
    | Bin (_,x,y) -> S.of_list [extract_var x;extract_var y]

  let variables_of_lit = function
    | Dyn (_,x) -> variables_of_dynamic x
    | Stat (_,x) -> S.singleton (extract_var x)

  let variables_of_formula x =
    fold_fomula_union variables_of_lit x

  let variables_of_guard (_,x,y) = S.of_list [extract_var x; extract_var y]

  let rec variables_of_safe = function
    | Leaf f -> variables_of_lit f
    | Var x -> S.singleton (extract_var x)
    | Apply (x,y) -> S.union (variables_of_safe x) (variables_of_safe y)
    | Quantif (_,_,f,x) -> S.union (variables_of_guard f) (variables_of_safe x)
    | Formula f -> fold_fomula_union variables_of_safe f

  let rec fv_of_safe = function
    | Leaf f -> variables_of_lit f
    | Var x -> S.singleton (extract_var x)
    | Apply (x,y) -> S.union (variables_of_safe x) (variables_of_safe y)
    | Quantif (_,u,f,x) -> S.remove u (S.union (variables_of_guard f) (variables_of_safe x))
    | Formula f -> fold_fomula_union fv_of_safe  f

  let fv_of_def (Def (_,xs,x)) =
    S.diff (fv_of_safe x) (S.of_list xs)
end
