type unop = Not

type binop =
  | And
  | Or

type 'a dynamic =
  | Has of 'a
  | Link of 'a * 'a
  | Other of string * 'a

type 'a lit =
  | Dyn of bool * 'a dynamic
  | Stat of string * 'a

type 'a formula =
  | Lit of 'a
  | Unop of unop * 'a formula
  | Binop of binop * 'a formula * 'a formula

type ('a,'l) pre_safe =
  | Leaf of 'l formula
  | Var of string
  | Forall of 'a  * 'l formula * ('a,'l) pre_safe
  | Exists of 'a * 'l formula * ('a,'l) pre_safe
  | Pand of ('a,'l) pre_safe * ('a,'l) pre_safe
  | Por of ('a,'l) pre_safe * ('a,'l) pre_safe

type 'a safe = ('a, 'a lit) pre_safe

type 'l general =
  | General of 'l formula list * 'l formula

type ('a,'l) def =
  | Def of (string * 'a list * ('a,'l) pre_safe)

type ('a,'l) pre_program =
  { vars : ('a,'l) def list
  ; safe : ('a, 'l) pre_safe list
  ; ensure : 'l general list
  ; maintain : 'l general list }

type 'a parsed_program = ('a, bool * 'a dynamic) pre_program
type 'a program = ('a, 'a lit) pre_program

let fold_formula l u b =
  let rec aux = function
    | Lit x -> l x
    | Unop (x,y) -> u x (aux y)
    | Binop (x,y,z) -> b x (aux y) (aux z)
  in aux

type gen = Ensure | Maintain

type parse_error =
  | UnboundDynamic of string
  | UnboundSymbol of string

let string_of_parse_error = function
  | UnboundDynamic s -> "Unbound dynamic: " ^ s
  | UnboundSymbol s -> "Unbound symbol: " ^ s

module SString = Set.Make(String)

exception ParseError of parse_error

let final_of_formula ~static ~dynamic =
  let mk_lit (b,dyn) =
    match dyn with
    | Has _ | Link _ -> Dyn (b,dyn)
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
    | Forall (a,l,x) -> Forall (a, final_of_formula ~static ~dynamic l, aux x)
    | Exists (a,l,x) -> Exists (a, final_of_formula ~static ~dynamic l, aux x)
    | Pand (x,y) -> Pand (aux x, aux y)
    | Por (x,y) -> Por (aux x, aux y)
  in aux

let program_of_parsed ~static ~dynamic {vars; safe; ensure; maintain} =
  try
    let general (General (xs,x)) =
      General ((List.map (final_of_formula ~static ~dynamic) xs),(final_of_formula ~static ~dynamic x)) in
    let vars =
      List.map
        (fun (Def (name,args,x)) -> Def (name, args, safe_of_parsed ~static ~dynamic x)) vars in
    let safe = List.map (safe_of_parsed ~static ~dynamic) safe in
    let ensure = List.map general ensure in
    let maintain = List.map general maintain in
    Ok ({vars; safe; ensure; maintain})
  with
    ParseError s -> Error s
