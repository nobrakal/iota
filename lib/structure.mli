open Program

(** This module ensures that:
- Guards are well formed
- General formulas are valid.
*)

type gen = Ensure | Maintain

module type Structure =
  sig
    type t

    type invalidity =
    | IllFormedGuard
    | IllFormedGeneral of gen

  val string_of_invalidity : invalidity -> string

  (** Returns good iff guards are really guards and ensure and maintain are well-formed *)
  val validate_program : t Final.final_program -> invalidity option
  end

module Make(M : Manip) : Structure with type t = M.t
