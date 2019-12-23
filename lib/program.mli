type unop = Not

type binop = And | Or

type 'a dynamic = Has of 'a | Link of 'a * 'a | Other of string * 'a

type 'a lit = Dyn of bool * 'a dynamic | Stat of string * 'a

type 'a formula =
    Lit of 'a lit
  | Unop of unop * 'a formula
  | Binop of binop * 'a formula * 'a formula

type 'a safe =
    Leaf of 'a formula
  | Forall of 'a * 'a formula * 'a safe
  | Exists of 'a * 'a formula * 'a safe
  | Pand of 'a safe * 'a safe
  | Por of 'a safe * 'a safe

type 'a general = General of 'a formula list * 'a formula

type 'a program = {
  safe : 'a safe;
  ensure : 'a general;
  maintain : 'a general;
  }

val fold_formula :
  ('a lit -> 'b) ->
  (unop -> 'b -> 'b) -> (binop -> 'b -> 'b -> 'b) -> 'a formula -> 'b

module type Variables = sig type t val compare : t -> t -> int end

module About :
  functor (V : Variables) ->
    sig
      type nonrec program = V.t program
      module S : Set.S with type elt = V.t
      val to_list : S.t -> S.elt list
      val variables_of_dynamic : S.elt dynamic -> S.t
      val variables_of_formula : S.elt formula -> S.t
      val extract_guard : S.elt formula -> (S.elt * S.elt) option
      val is_guard_for : S.elt -> S.elt formula -> bool
      val verify_guards : S.elt safe -> bool
      val extract_guards : S.elt formula list -> (S.elt * S.elt) list option
      val extract_xvar_candidates_of : S.t -> S.t list -> S.elt list
      val valid_candidate : S.elt formula -> S.elt list -> S.elt option
      val verify_general : S.elt general -> bool
      val is_valid_program : program -> bool
    end
