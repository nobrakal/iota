open Program

type ('a,'l) pre_gsafe =
  | F of ('a,'l) pre_safe
  | Bracket of 'a list * ('a,'l) pre_safe (* existentially quantified variables, without guards *)

type ('a,'l) gdef =
  | GDef of ('a * 'a list * ('a,'l) pre_gsafe)

type 'a gfinal_program =
  { gvars : ('a, ('a, rbinpred) lit) gdef list
  ; gsafe : ('a, ('a, rbinpred) lit) pre_safe list
  ; gensure : ('a, ('a, rbinpred) lit, rbinpred) general list
  ; gmaintain : ('a, ('a, rbinpred) lit, rbinpred) general list }

module type Guard_inference = sig
  type t
  module M : Manip with type t = t

  type err

  val string_of_err : (t -> string) -> err -> string

  val run : t gfinal_program -> (t program,err) result
end

module Make(M : Manip) : Guard_inference with type t = M.t
