open Program
open Utils

(* Not and Exact *)
type tag = N | E

type ('a,'l) pre_fsafe =
  | FLeaf of tag * 'l
  | FQuantif of quantif * 'a * ('a, binpred) guard * ('a,'l) pre_fsafe
  | FBinop of binop * ('a,'l) pre_fsafe * ('a,'l) pre_fsafe

let string_of_tag = function
  | N -> "not "
  | E -> ""

let string_of_fsafe f e u =
  let rec aux = function
    | FLeaf (x,y) -> string_of_tag x ^ paren (f y)
    | FQuantif (Forall,a,g,x) ->
       "forall" ^ space (e a) ^ string_of_guard e string_of_binpred g ^ space "->" ^ paren (aux x)
    | FQuantif (Exists,a,g,x) ->
       "exists" ^ space (e a) ^ string_of_guard e string_of_binpred g ^ space "&&" ^ paren (aux x)
    | FBinop (b,x,y) -> paren (aux x) ^ space (string_of_binop b) ^ paren (aux y)
  in aux u

let print_fsafe f e x = print_endline (string_of_fsafe f e x)

type 'a final_program =
  { fsafe : ('a, ('a, binpred) lit) pre_fsafe list
  ; fensure : ('a, ('a, binpred) lit, binpred) general list
  ; fmaintain : ('a, ('a, binpred) lit, binpred) general list }

let string_of_final ({fsafe; fensure; fmaintain} : string final_program) =
  let string_of_list f xs = String.concat ";\n" @@
    List.map f xs in
  let id x = x in
  string_of_list (string_of_fsafe (string_of_lit id string_of_binpred) id) fsafe
  ^ "\nensure\n" ^
  string_of_list (string_of_general (string_of_lit id string_of_binpred) id string_of_binpred) fensure
  ^ "\nmaintain\n" ^
   string_of_list (string_of_general (string_of_lit id string_of_binpred) id string_of_binpred) fmaintain

let print_final x = print_endline (string_of_final x)
