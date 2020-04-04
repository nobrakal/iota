open Program
open Gfinal

(** This module
- Runs algorithm W on the code
- Tries to infer missing guards
*)

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

    val typecheck_program :
      predicates:('a * string * string) list ->
      types:(Config.ty_dec list) -> t program -> (t gfinal_program,type_error) result
  end


module Make(Manip : Manip) : Typecheck with type t = Manip.t
