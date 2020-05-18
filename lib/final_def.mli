open Program

(** This module declares a final type, made in two variants:
    one allowing "Brackets" (or unbounded existential quantification) and the other not. *)

type tag =
  | N (** Not *)
  | E (** Exact *)

type ('a,'l) pre_fsafe =
  PLeaf of 'l
| PQuantif of quantif * 'a * ('a, binpred) guard list * ('a,'l) pre_fsafe
| PBracket of 'a list * ('a,'l) pre_fsafe
| PFormula of ('a,'l) pre_fsafe formula

type ('a,'l) fsafe =
  FLeaf of tag * 'l
| FQuantif of quantif * 'a * ('a, binpred) guard list * ('a,'l) fsafe
| FBinop of binop * ('a,'l) fsafe * ('a,'l) fsafe

val string_of_fsafe : ('b -> string) -> ('a -> string) -> ('a,'b) fsafe -> string
val print_fsafe : ('b -> string) -> ('a -> string) -> ('a,'b) fsafe -> unit

type ('a,'s) pre_fprogram =
  { fsafe : 's list
  ; fensure : ('a, ('a, binpred) lit, binpred) general list
  ; fmaintain : ('a, ('a, binpred) lit, binpred) general list }

type 'a pre_final_program = ('a, ('a, ('a, binpred) lit) pre_fsafe) pre_fprogram
type 'a final_program = ('a, ('a, ('a, binpred) lit) fsafe) pre_fprogram

val replace_generated_vars : (string, (string, 'a) Program.lit) fsafe -> (string, (string, 'a) Program.lit) fsafe

val string_of_final : string final_program -> string

val print_final : string final_program -> unit
