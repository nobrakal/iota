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
  | Var of string
  | Forall of 'a  * 'l formula * ('a,'l) pre_safe
  | Exists of 'a * 'l formula * ('a,'l) pre_safe
  | Pand of ('a,'l) pre_safe * ('a,'l) pre_safe
  | Por of ('a,'l) pre_safe * ('a,'l) pre_safe

(** A safe syntax with some meaning *)
type 'a safe = ('a, 'a lit) pre_safe

type 'l general =
  | General of 'l formula list * 'l formula

type ('a,'l) def =
  | Def of (string * 'a list * ('a,'l) pre_safe)

(** A program which can be meaningless *)
type ('a,'l) pre_program =
  { vars : ('a,'l) def list
  ; safe : ('a, 'l) pre_safe list
  ; ensure : 'l general list
  ; maintain : 'l general list }

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
