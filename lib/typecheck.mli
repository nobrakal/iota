open Program

type base_ty = Safet | Litt

type monoty =
  | V of string
  | T of base_ty
  | Arrow of (monoty * monoty)

module type Typecheck =
  sig

    type t

    type type_error =
      | UnboundVar of t
      | WrongType of monoty * monoty (* actual, expected *)

    val string_of_type_error : (t -> string) -> type_error -> string

    val typecheck_program : t program -> (t program,type_error) result
  end


module Make(Manip : Manip) : Typecheck with type t = Manip.t
