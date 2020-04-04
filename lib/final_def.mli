open Program

type tag =
  | N (** Not *)
  | E (** Exact *)

type ('a,'l) pre_fsafe =
  | FLeaf of tag * 'l
  | FQuantif of quantif * 'a * ('a, binpred) guard * ('a,'l) pre_fsafe
  | FBinop of binop * ('a,'l) pre_fsafe * ('a,'l) pre_fsafe

val string_of_fsafe : ('b -> string) -> ('a -> string) -> ('a,'b) pre_fsafe -> string
val print_fsafe : ('b -> string) -> ('a -> string) -> ('a,'b) pre_fsafe -> unit

type 'a final_program =
  { fsafe : ('a, ('a, binpred) lit) pre_fsafe list
  ; fensure : ('a, ('a, binpred) lit, binpred) general list
  ; fmaintain : ('a, ('a, binpred) lit, binpred) general list }

val string_of_final : string final_program -> string

val print_final : string final_program -> unit
