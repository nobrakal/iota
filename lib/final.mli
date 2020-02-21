open Program

type ('a,'l) pre_fsafe =
  | FLeaf of 'l formula
  | FForall of 'a * ('a, binpred) guard * ('a,'l) pre_fsafe
  | FExists of 'a * ('a, binpred) guard * ('a,'l) pre_fsafe
  | FBinop of binop * ('a,'l) pre_fsafe * ('a,'l) pre_fsafe

val string_of_fsafe : ('b -> string) -> ('a -> string) -> ('a,'b) pre_fsafe -> string
val print_fsafe : ('b -> string) -> ('a -> string) -> ('a,'b) pre_fsafe -> unit

type 'a final_program =
  { fsafe : ('a, ('a, binpred) lit) pre_fsafe list
  ; fensure : ('a, ('a, binpred) lit, binpred) general list
  ; fmaintain : ('a, ('a, binpred) lit, binpred) general list }

(** Transform a program that typecheck into a final one *)
val final_of_program :
  maxprof:int ->
  functions:string list ->
  string Program.program -> string final_program

val string_of_final : string final_program -> string

val print_final : string final_program -> unit

val normalize : 'a final_program -> 'a final_program
