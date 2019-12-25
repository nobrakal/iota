open Program

module Make(M : Manip) :
sig
  type validity =
    | Good
    | IllFormedGuard
    | IllFormedGeneral of gen
    | UnboundVar of string

  val string_of_validity : validity -> string

  (** Retun good iff guards are really guards and ensure and maintain are well-formed *)
  val validate_program : M.t program -> validity
end
