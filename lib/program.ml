type binop = And | Or

type binpred = Eq | Link

type rbinpred =
  | TLink
  | B of binpred

type 'a var =
  | V of 'a
  | Parent of 'a var
  | Func of string * 'a var

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
  | Leaf of 'l formula
  | Var of 'a
  | Apply of ('a,'l) pre_safe * ('a,'l) pre_safe
  | Forall of 'a * ('a, binpred) guard * ('a,'l) pre_safe
  | Exists of 'a * ('a, binpred) guard * ('a,'l) pre_safe

type 'a safe = ('a, ('a, rbinpred) lit) pre_safe

type ('a,'l) general =
  | General of ('a, binpred) guard list * 'l formula

type ('a,'l) def =
  | Def of ('a * 'a list * ('a,'l) pre_safe)

type ('a,'l) pre_program =
  { vars : ('a,'l) def list
  ; safe : ('a, 'l) pre_safe list
  ; ensure : ('a, 'l) general list
  ; maintain : ('a, 'l) general list }

type 'a parsed_program = ('a, bool * ('a, rbinpred) dynamic) pre_program
type 'a program = ('a, ('a, rbinpred) lit) pre_program

let map_var f x =
  let rec aux = function
    | V x -> V (f x)
    | Parent x -> Parent (aux x)
    | Func (s,x) -> Func (s, aux x)
  in aux x

let rec extract_var x =
  match x with
  | V x -> x
  | Parent x -> extract_var x
  | Func (_,x) -> extract_var x

let fold_formula l u b =
  let rec aux = function
    | Lit x -> l x
    | Not y -> u (aux y)
    | Binop (x,y,z) -> b x (aux y) (aux z)
  in aux

type parse_error =
  | UnboundDynamic of string
  | UnboundSymbol of string

let string_of_parse_error = function
  | UnboundDynamic s -> "Unbound dynamic: " ^ s
  | UnboundSymbol s -> "Unbound symbol: " ^ s

let paren s = "(" ^ s ^ ")"
let space s = " " ^ s ^ " "

let string_of_var s =
  let rec aux = function
  | V x -> s x
  | Parent x -> "parent" ^ paren (aux x)
  | Func (s,x) -> aux x ^ "." ^ s
  in aux

let string_of_binpred = function
  | Link -> "Link"
  | Eq -> "Eq"

let string_of_rbinpred = function
  | TLink -> "TLink"
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
    | Var x -> p x
    | Apply (x,y) ->
       paren (aux x) ^ paren (aux y)
    | Forall (x,y,z) ->
       "forall " ^ p x  ^ paren (string_of_guard p string_of_binpred y) ^ paren (aux z)
    | Exists (x,y,z) ->
       "exists " ^ p x  ^ paren (string_of_guard p string_of_binpred y) ^ paren (aux z)
    | Formula f -> auxf f
  and auxf = function
    | Lit x -> aux x
    | Not x -> "not " ^ paren (auxf x)
    | Binop (b,x,y) ->
       paren (auxf x) ^ space (string_of_binop b) ^ paren (auxf y)
  in aux x

let print_safe s p x = print_endline (string_of_safe s p x)

let string_of_general s p (General (xs,x)) =
  let str =
    match xs with
    | [] -> ""
    | x::xs -> List.fold_left (fun acc x -> acc ^ "->" ^ string_of_guard p string_of_binpred x)
                 (string_of_guard p string_of_binpred x) xs in
  str ^ space "=>" ^ string_of_formula s x

let print_general s p x = print_endline (string_of_general s p x)

module SString = Set.Make(String)

exception ParseError of parse_error

let final_of_formula ~static ~dynamic =
  let mk_lit (b,dyn) =
    match dyn with
    | Has _ | Bin _ -> Dyn (b,dyn)
    | Other (s,x) ->
       if SString.mem s dynamic then Dyn (b,dyn)
       else
         if b
         then raise (ParseError (UnboundDynamic s))
         else
           if SString.mem s static then Stat (s,x)
           else raise (ParseError (UnboundSymbol s))
  in
  fold_formula (fun x -> Lit (mk_lit x))
    (fun x -> Not x) (fun b x y -> Binop (b,x,y))

let safe_of_parsed ~static ~dynamic =
  let rec aux = function
    | Leaf x -> Leaf (final_of_formula ~static ~dynamic x)
    | Var x -> Var x
    | Apply (x,y) -> Apply (aux x, aux y)
    | Forall (a,l,x) -> Forall (a, l, aux x)
    | Exists (a,l,x) -> Exists (a, l, aux x)
    | Formula f ->
       Formula
         (fold_formula (fun x -> Lit (aux x)) (fun x -> Not x) (fun b x y -> Binop (b,x,y)) f)
  in aux

let program_of_parsed ~static ~dynamic {vars; safe; ensure; maintain} =
  try
    let general (General (xs,x)) =
      General (xs,(final_of_formula ~static ~dynamic x)) in
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
  end

module Manip (V : Set.OrderedType) : Manip with type t = V.t = struct
  type t = V.t
  module S = Set.Make(V)
  module M = Map.Make(V)
  let to_list s = S.fold (fun x y -> x::y) s []

  let variables_of_dynamic = function
    | Has x -> S.singleton (extract_var x)
    | Bin (_,x,y) -> S.of_list [extract_var x;extract_var y]
    | Other (_,x) -> S.singleton (extract_var x)

  let variables_of_lit = function
    | Dyn (_,x) -> variables_of_dynamic x
    | Stat (_,x) -> S.singleton (extract_var x)

  let variables_of_formula x =
    fold_formula variables_of_lit (fun x -> x) (fun _ -> S.union) x

  let variables_of_guard (_,x,y) = S.of_list [extract_var x; extract_var y]

  let rec variables_of_safe = function
    | Leaf f -> variables_of_formula f
    | Var x -> S.singleton x
    | Apply (x,y) -> S.union (variables_of_safe x) (variables_of_safe y)
    | Forall (_,f,x) | Exists (_,f,x) -> S.union (variables_of_guard f) (variables_of_safe x)
    | Formula f ->
       fold_formula variables_of_safe (fun x -> x) (fun _ -> S.union) f
end
