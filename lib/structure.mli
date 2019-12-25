open Program

module Make :
functor (V : Set.OrderedType) ->
sig
  module S : Set.S with type elt = V.t

  type validity =
    | Good
    | IllFormedGuard
    | IllFormedGeneral of gen
    | UnboundVar of string

  val string_of_validity : validity -> string

  val variables_of_dynamic : S.elt dynamic -> S.t
  val variables_of_lit : S.elt lit -> S.t
  val variables_of_formula : S.elt lit formula -> S.t
  val extract_guard : S.elt lit formula -> (S.elt * S.elt) option

  (** Retun good iff guards are really guards and ensure and maintain are well-formed *)
  val validate_program : V.t program -> validity
end
