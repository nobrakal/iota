open Program

type ('a,'l) pre_fsafe =
  | FLeaf of 'l formula
  | FForall of 'a * 'a guard * ('a,'l) pre_fsafe
  | FExists of 'a * 'a guard * ('a,'l) pre_fsafe
  | FBinop of binop * ('a,'l) pre_fsafe * ('a,'l) pre_fsafe

val string_of_fsafe : ('b -> string) -> ('a -> string) -> ('a,'b) pre_fsafe -> string
val print_fsafe : ('b -> string) -> ('a -> string) -> ('a,'b) pre_fsafe -> unit

type 'a final_program =
  { fsafe : ('a, 'a lit) pre_fsafe list
  ; fensure : ('a, 'a lit) general list
  ; fmaintain : ('a, 'a lit) general list }

(** Transform a program that typecheck into a final one *)
val final_of_program : string program -> string final_program

val print_final : string final_program -> unit
