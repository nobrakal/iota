open Program
open Utils

(* Not and Exact *)
type tag = N | E

type ('a,'l) pre_fsafe =
  PLeaf of 'l
| PQuantif of quantif * 'a * ('a, binpred) guard list * ('a,'l) pre_fsafe
| PBracket of 'a list * ('a,'l) pre_fsafe
| PFormula of ('a,'l) pre_fsafe formula

type ('a,'l) fsafe =
  FLeaf of tag * 'l
| FQuantif of quantif * 'a * ('a, binpred) guard list * ('a,'l) fsafe
| FBinop of binop * ('a,'l) fsafe * ('a,'l) fsafe

let fold_fsafe l q b x =
  let rec aux = function
    | FLeaf (t,x) -> l t x
    | FQuantif (a,z,b,c) -> q a z b (aux c)
    | FBinop (a,x,y) -> b a (aux x) (aux y)
  in aux x

module Manip = Manip(String)

let vars_of_fsafe x =
  fold_fsafe
    (fun _ x -> Manip.variables_of_lit x)
    (fun _ z xs c ->
      Manip.(unions ([S.singleton z;variables_of_guards xs;c])))
    (fun _ x y -> Manip.S.union x y)
    x

let replace_vars ms x =
  let f x =
    match Manip.M.find_opt x ms with
    | None -> x
    | Some x -> x in
  fold_fsafe
    (fun a x -> FLeaf (a,map_lit (map_var f) x))
    (fun a b c d -> FQuantif (a,f b,List.map (fun (a,b,c) -> (a,map_var f b,map_var f c)) c, d))
    (fun a b c -> FBinop (a,b,c))
    x

let replace_generated_vars x =
  let counter = ref 0 in
  let rec aux i (vs,ms) =
    let id = "x" ^ string_of_int !counter in
    counter := !counter +1;
    if Manip.S.mem id vs
    then aux i (vs,ms)
    else (Manip.S.add id vs,Manip.M.add i id ms) in
  let vs = Manip.S.filter (fun v -> String.get v 0 = '_') (vars_of_fsafe x) in
  let _,ms = Manip.S.fold aux vs (vs,Manip.M.empty) in
  replace_vars ms x

let string_of_tag = function
  | N -> "not "
  | E -> ""

let string_of_fsafe f e u =
  let rec aux = function
    | FLeaf (x,y) -> string_of_tag x ^ paren (f y)
    | FQuantif (Forall,a,g,x) ->
       "forall" ^ space (e a) ^ string_of_guards e string_of_binpred g ^ space "->" ^ paren (aux x)
    | FQuantif (Exists,a,g,x) ->
       "exists" ^ space (e a) ^ string_of_guards e string_of_binpred g ^ space "&&" ^ paren (aux x)
    | FBinop (b,x,y) -> paren (aux x) ^ space (string_of_binop b) ^ paren (aux y)
  in aux u

let print_fsafe f e x = print_endline (string_of_fsafe f e x)

type ('a,'s) pre_fprogram =
  { fsafe : 's list
  ; fensure : ('a, ('a, binpred) lit, binpred) general list
  ; fmaintain : ('a, ('a, binpred) lit, binpred) general list }

type 'a pre_final_program = ('a, ('a, ('a, binpred) lit) pre_fsafe) pre_fprogram
type 'a final_program = ('a, ('a, ('a, binpred) lit) fsafe) pre_fprogram

let string_of_final ({fsafe; fensure; fmaintain} : string final_program) =
  let string_of_list f xs = String.concat ";\n" @@
    List.map f xs in
  let id x = x in
  string_of_list (string_of_fsafe (string_of_lit id string_of_binpred) id) (List.map replace_generated_vars fsafe)
  ^ "\nensure\n" ^
  string_of_list (string_of_general (string_of_lit id string_of_binpred) id string_of_binpred) fensure
  ^ "\nmaintain\n" ^
   string_of_list (string_of_general (string_of_lit id string_of_binpred) id string_of_binpred) fmaintain

let print_final x = print_endline (string_of_final x)
