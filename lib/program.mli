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

val print_dynamic : ('a -> string) -> 'a dynamic -> unit
val print_lit : ('a -> string) -> 'a lit -> unit
val print_formula : ('a -> unit) -> 'a formula -> unit

(** A safe syntax which can be meaningless *)
type ('a,'l) pre_safe =
  | Leaf of 'l formula
  | Var of 'a
  | Apply of ('a,'l) pre_safe * ('a,'l) pre_safe
  | Forall of 'a  * 'l formula * ('a,'l) pre_safe
  | Exists of 'a * 'l formula * ('a,'l) pre_safe
  | Pand of ('a,'l) pre_safe * ('a,'l) pre_safe
  | Por of ('a,'l) pre_safe * ('a,'l) pre_safe

(** A safe syntax with some meaning *)
type 'a safe = ('a, 'a lit) pre_safe

type 'l general =
  | General of 'l formula list * 'l formula

val print_general : ('a -> unit) -> 'a general -> unit

type ('a,'l) def =
  | Def of ('a * 'a list * ('a,'l) pre_safe)

(** A program which can be meaningless *)
type ('a,'l) pre_program =
  { vars : ('a,'l) def list
  ; safe : ('a, 'l) pre_safe list
  ; ensure : 'l general list
  ; maintain : 'l general list }

val print_safe : ('a -> unit) -> ('b -> string) -> ('b,'a) pre_safe -> unit

(** A program which doesn't distinguish static and dynamic predicates *)
type 'a parsed_program = ('a, bool * 'a dynamic) pre_program

(** A well-formed program *)
type 'a program = ('a, 'a lit) pre_program

val fold_formula :
  ('a -> 'b) ->
  (unop -> 'b -> 'b) -> (binop -> 'b -> 'b -> 'b) -> 'a formula -> 'b

type gen = Ensure | Maintain

type parse_error =
  | UnboundDynamic of string
  | UnboundSymbol of string

val string_of_parse_error : parse_error -> string

module SString : Set.S with type elt = string

(** Transform a parsed program into a real one knowing static and dynamic predicates *)
val program_of_parsed :
  static:SString.t -> dynamic:SString.t ->
  'a parsed_program -> ('a program, parse_error) result

module type Manip =
  sig
    type t

    module S : Set.S with type elt = t
    val to_list : S.t -> S.elt list
    module M : Map.S with type key = t

    val variables_of_dynamic : S.elt dynamic -> S.t
    val variables_of_lit : S.elt lit -> S.t
    val variables_of_formula : S.elt lit formula -> S.t
    val variables_of_safe : S.elt safe -> S.t
    val extract_guard : S.elt lit formula -> (S.elt * S.elt) option
  end

module Manip(V : Set.OrderedType) : Manip with type t = V.t
