open Program
open Final_def

(** This module tries to infer missing guards and moves negation only on leaves. *)

module type Guard_inference = sig
  type t
  module M : Manip with type t = t

  type err

  val string_of_err : (t -> string) -> err -> string

  val run : t pre_final_program -> (t final_program,err) result
end

module Make(M : Manip) : Guard_inference with type t = M.t
