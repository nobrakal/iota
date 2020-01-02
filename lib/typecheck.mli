open Program

type base_ty = Safet | Litt

type monoty =
  | V of string
  | T of base_ty
  | Arrow of (monoty * monoty)

module Make(Manip : Manip) : sig

  type type_error =
    | UnboundVar of Manip.t
    | WrongType of monoty * monoty (* actual, expected *)

  val string_of_type_error : (Manip.t -> string) -> type_error -> string

  val typecheck_program : Manip.t program -> type_error option
end
