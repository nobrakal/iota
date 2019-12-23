type unop = Not

type binop =
  | And
  | Or

type 'a dynamic =
  | Has of 'a
  | Link of 'a * 'a
  | Other of string * 'a

type 'a lit =
  | Dyn of bool * 'a dynamic
  | Stat of string * 'a

type 'a formula =
  | Lit of 'a
  | Unop of unop * 'a formula
  | Binop of binop * 'a formula * 'a formula

(** A safe syntax which can be meaningless *)
type ('a,'l) pre_safe =
  | Leaf of 'l formula
  | Forall of 'a  * 'l formula * ('a,'l) pre_safe
  | Exists of 'a * 'l formula * ('a,'l) pre_safe
  | Pand of ('a,'l) pre_safe * ('a,'l) pre_safe
  | Por of ('a,'l) pre_safe * ('a,'l) pre_safe

(** A safe syntax with some meaning *)
type 'a safe = ('a, 'a lit) pre_safe

type 'l general =
  | General of 'l formula list * 'l formula

(** A program which can be meaningless *)
type ('a,'l) pre_program =
  { safe : ('a, 'l) pre_safe
  ; ensure : 'l general option
  ; maintain : 'l general option }

(** A program which doesn't distinguish static and dynamic predicates *)
type 'a parsed_program = ('a, bool * 'a dynamic) pre_program

(** A well-formed program *)
type 'a program = ('a, 'a lit) pre_program

val fold_formula :
  ('a -> 'b) ->
  (unop -> 'b -> 'b) -> (binop -> 'b -> 'b -> 'b) -> 'a formula -> 'b

module type Variables = sig type t val compare : t -> t -> int end

module About :
functor (V : Variables) ->
sig
  type nonrec parsed_program = V.t parsed_program
  type nonrec program = V.t program

  module S : Set.S with type elt = V.t
  module SString : Set.S with type elt = string

  val variables_of_dynamic : S.elt dynamic -> S.t
  val variables_of_lit : S.elt lit -> S.t
  val variables_of_formula : S.elt lit formula -> S.t
  val extract_guard : S.elt lit formula -> (S.elt * S.elt) option

  (** Transform a parsed program into a real one knowing static and dynamic predicates *)
  val program_of_parsed : static:SString.t -> dynamic:SString.t -> parsed_program -> program

  (** Retun true iff guards are really guards and ensure and maintain are well-formed *)
  val is_valid_program : program -> bool

end
