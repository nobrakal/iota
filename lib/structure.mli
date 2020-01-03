open Program

module Make(M : Manip) :
sig
  type invalidity =
    | IllFormedGuard
    | IllFormedGeneral of gen

  val string_of_invalidity : invalidity -> string

  (** Retun good iff guards are really guards and ensure and maintain are well-formed *)
  val validate_program : M.t Final.final_program -> invalidity option
end
