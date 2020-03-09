open Program

(** This module compiles a typechecking program to a final one. It
- Inlines all let definitions.
- Moves negation only on leaves.
- Compiles [TLink] to the corresponding list of [Link].
- Simplifies [Link(x.f)] into [x].
*)

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

(** Transform a program that typecheck into a final one *)
val final_of_program :
  maxprof:int ->
  types:(Sum_types.ty_dec list) ->
  string Program.program -> string final_program

val string_of_final : string final_program -> string

val print_final : string final_program -> unit
