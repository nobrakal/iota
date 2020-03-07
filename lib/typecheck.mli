open Program

(** Type of monomorphic types *)
type monoty =
  | V of string (** Type variable *)
  | Safet (** A safe expression *)
  | Litt of string (** A known litteral *)
  | Arrow of (monoty * monoty) (** Arrow *)

module type Typecheck =
  sig

    type t

    type type_error =
      | UnboundVar of t
      | WrongType of monoty * monoty (* actual, expected *)

    val string_of_type_error : (t -> string) -> type_error -> string

    val typecheck_program : types:(Sum_types.ty_dec list) -> t program -> (t program,type_error) result
  end


module Make(Manip : Manip) : Typecheck with type t = Manip.t
