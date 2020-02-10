type binop = And | Or

type binpred = Eq | Link

type 'a var =
  | V of 'a
  | Parent of 'a var

type 'a guard = binpred * 'a var * 'a var

type 'a dynamic =
  | Has of 'a var
  | Bin of 'a guard
  | Other of string * 'a var

type 'a lit =
  | Dyn of bool * 'a dynamic
  | Stat of string * 'a var

type 'a formula =
  | Lit of 'a
  | Not of 'a formula
  | Binop of binop * 'a formula * 'a formula

(** A safe syntax which can be meaningless *)
and ('a,'l) pre_safe =
  | Formula of ('a,'l) pre_safe formula
  | Leaf of 'l formula
  | Var of 'a
  | Apply of ('a,'l) pre_safe * ('a,'l) pre_safe
  | Forall of 'a * 'a guard * ('a,'l) pre_safe
  | Exists of 'a * 'a guard * ('a,'l) pre_safe

(** A safe syntax with some meaning *)
type 'a safe = ('a, 'a lit) pre_safe

val map_var : ('a -> 'b) -> 'a var -> 'b var
val extract_var : 'a var -> 'a

val string_of_var : ('a -> string) -> 'a var -> string
val string_of_guard : ('a -> string) -> binpred * 'a var * 'a var -> string
val string_of_dynamic : ('a -> string) -> 'a dynamic -> string
val string_of_lit : ('a -> string) -> 'a lit -> string
val string_of_binop : binop -> string
val string_of_formula : ('a -> string) -> 'a formula -> string
val print_formula : ('a -> string) -> 'a formula -> unit

val string_of_safe :
  ('a formula -> string) -> ('b -> string) -> ('b, 'a) pre_safe -> string
val print_safe :
  ('a formula -> string) -> ('b -> string) -> ('b, 'a) pre_safe -> unit

type ('a,'l) general =
  | General of 'a guard list * 'l formula

val string_of_general :
  ('a -> string) -> ('b -> string) -> ('b, 'a) general -> string
val print_general :
  ('a -> string) -> ('b -> string) -> ('b, 'a) general -> unit

type ('a,'l) def =
  | Def of ('a * 'a list * ('a,'l) pre_safe)

(** A program which can be meaningless *)
type ('a,'l) pre_program =
  { vars : ('a,'l) def list
  ; safe : ('a, 'l) pre_safe list
  ; ensure : ('a,'l) general list
  ; maintain : ('a,'l) general list }

(** A program which doesn't distinguish static and dynamic predicates *)
type 'a parsed_program = ('a, bool * 'a dynamic) pre_program

(** A well-formed program *)
type 'a program = ('a, 'a lit) pre_program

val fold_formula :
  ('a -> 'b) ->
  ('b -> 'b) -> (binop -> 'b -> 'b -> 'b) -> 'a formula -> 'b

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
  end

module Manip(V : Set.OrderedType) : Manip with type t = V.t
