type unop = Not

type binop = And | Or

type binpred = Eq | Link

type 'a var =
  | V of 'a
  | Parent of 'a var

type 'a guard = binpred * 'a var * 'a var

type 'a dynamic =
  | Has of 'a var
  | Bin of 'a guard
  | Other of string * 'a var

type 'a lit =
  | Dyn of bool * 'a dynamic
  | Stat of string * 'a var

type 'a formula =
  | Lit of 'a
  | Unop of unop * 'a formula
  | Binop of binop * 'a formula * 'a formula

type ('a,'l) pre_safe =
  | Leaf of 'l formula
  | Var of 'a
  | Apply of ('a,'l) pre_safe * ('a,'l) pre_safe
  | Forall of 'a * 'a guard * ('a,'l) pre_safe
  | Exists of 'a * 'a guard * ('a,'l) pre_safe
  | Pbin of binop * ('a,'l) pre_safe * ('a,'l) pre_safe

type 'a safe = ('a, 'a lit) pre_safe

type ('a,'l) general =
  | General of 'a guard list * 'l formula

type ('a,'l) def =
  | Def of ('a * 'a list * ('a,'l) pre_safe)

type ('a,'l) pre_program =
  { vars : ('a,'l) def list
  ; safe : ('a, 'l) pre_safe list
  ; ensure : ('a, 'l) general list
  ; maintain : ('a, 'l) general list }

type 'a parsed_program = ('a, bool * 'a dynamic) pre_program
type 'a program = ('a, 'a lit) pre_program

let map_var f x =
  let rec aux = function
    | V x -> V (f x)
    | Parent x -> Parent (aux x)
  in aux x

let rec extract_var x =
  match x with
  | V x -> x
  | Parent x -> extract_var x

let fold_formula l u b =
  let rec aux = function
    | Lit x -> l x
    | Unop (x,y) -> u x (aux y)
    | Binop (x,y,z) -> b x (aux y) (aux z)
  in aux

type parse_error =
  | UnboundDynamic of string
  | UnboundSymbol of string

let string_of_parse_error = function
  | UnboundDynamic s -> "Unbound dynamic: " ^ s
  | UnboundSymbol s -> "Unbound symbol: " ^ s

let string_of_var s =
  let rec aux = function
  | V x -> s x
  | Parent x ->  "Parent(" ^ aux x ^ ")"
  in aux

let print_guard s (b,x,y) =
  let s = string_of_var s in
  let b = match b with
    | Link -> "Link"
    | Eq -> "Eq" in
  Printf.printf "%s(%s,%s)" b (s x) (s y)

let print_dynamic s =
  let s' = string_of_var s in function
  | Has x -> Printf.printf "Has(%s)" (s' x)
  | Other (x,y) -> Printf.printf "%s(%s)" x (s' y)
  | Bin t -> print_guard s t

let print_lit s = function
  | Dyn (b,x) ->
     if b
     then Printf.printf "+";
     print_dynamic s x
  | Stat (x,y) -> Printf.printf "%s(%s)" x (string_of_var s y)

let string_of_binop = function
  | And -> "&&"
  | Or -> "||"

let print_formula lit =
  let rec aux = function
    | Lit x -> lit x
    | Unop (Not,f) ->
       Printf.printf "not (";
       aux f;
       Printf.printf ")"
    | Binop (u,x,y) ->
       Printf.printf "(";
       aux x;
       Printf.printf ") %s (" (string_of_binop u);
       aux y;
       Printf.printf ")"
  in aux

let print_safe s p =
  let rec aux = function
    | Leaf x ->
       Printf.printf "{";
       print_formula s x;
       Printf.printf "}"
    | Var x ->
       Printf.printf "%s" (p x)
    | Apply (x,y) ->
       Printf.printf "(";
       aux x;
       Printf.printf ") ";
       Printf.printf "(";
       aux y;
       Printf.printf ")"
    | Forall (x,y,z) ->
       Printf.printf "forall %s (" (p x);
       print_guard p y;
       Printf.printf ") (";
       aux z;
       Printf.printf ")"
    | Exists (x,y,z) ->
       Printf.printf "exists %s (" (p x);
       print_guard p y;
       Printf.printf ") (";
       aux z;
       Printf.printf ")"
    | Pbin (b,x,y) ->
       Printf.printf "(";
       aux x;
       Printf.printf ") %s (" (string_of_binop b);
       aux y;
       Printf.printf ")"
  in aux

let print_general s p (General (xs,x)) =
  List.iter (fun x -> print_guard p x; Printf.printf " -> ") xs;
  print_formula s x

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
    (fun u x -> Unop (u,x)) (fun b x y -> Binop (b,x,y))

let safe_of_parsed ~static ~dynamic =
  let rec aux = function
    | Leaf x -> Leaf (final_of_formula ~static ~dynamic x)
    | Var x -> Var x
    | Apply (x,y) -> Apply (aux x, aux y)
    | Forall (a,l,x) -> Forall (a, l, aux x)
    | Exists (a,l,x) -> Exists (a, l, aux x)
    | Pbin (b,x,y) -> Pbin (b, aux x, aux y)
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

    val variables_of_dynamic : S.elt dynamic -> S.t
    val variables_of_lit : S.elt lit -> S.t
    val variables_of_formula : S.elt lit formula -> S.t
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

  let variables_of_formula =
    fold_formula variables_of_lit (fun _ x -> x) (fun _ -> S.union)

  let variables_of_guard (_,x,y) = S.of_list [extract_var x; extract_var y]

  let rec variables_of_safe = function
    | Leaf f -> variables_of_formula f
    | Var x -> S.singleton x
    | Apply (x,y) -> S.union (variables_of_safe x) (variables_of_safe y)
    | Forall (_,f,x) | Exists (_,f,x) -> S.union (variables_of_guard f) (variables_of_safe x)
    | Pbin (_,x,y) -> S.union (variables_of_safe x) (variables_of_safe y)
end
