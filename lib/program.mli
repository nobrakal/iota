type binop = And | Or

type binpred = Eq | Link

type rbinpred =
  | TLink
  | B of binpred

type 'a var =
  | V of 'a
  | Parent of 'a var
  | Func of string * 'a var

type ('a,'b) guard = 'b * 'a var * 'a var

type ('a,'b) dynamic =
  | Has of 'a var
  | Bin of ('a,'b) guard
  | Other of string * 'a var

type ('a,'b) lit =
  | Dyn of bool * ('a,'b) dynamic
  | Stat of string * 'a var

type 'a formula =
  | Lit of 'a
  | Not of 'a formula
  | Binop of binop * 'a formula * 'a formula

(** A safe syntax which can be meaningless *)
type ('a,'l) pre_safe =
  | Formula of ('a,'l) pre_safe formula
  | Leaf of 'l formula
  | Var of 'a
  | Apply of ('a,'l) pre_safe * ('a,'l) pre_safe
  | Forall of 'a * ('a, rbinpred) guard * ('a,'l) pre_safe
  | Exists of 'a * ('a, rbinpred) guard * ('a,'l) pre_safe

(** A safe syntax with some meaning *)
type 'a safe = ('a, ('a, rbinpred) lit) pre_safe

val map_var : ('a -> 'b) -> 'a var -> 'b var
val extract_var : 'a var -> 'a

val string_of_var : ('a -> string) -> 'a var -> string
val string_of_binpred : binpred -> string
val string_of_rbinpred : rbinpred -> string
val string_of_guard : ('a -> string) -> ('b -> string) -> 'b * 'a var * 'a var -> string
val string_of_dynamic : ('a -> string) -> ('b -> string) -> ('a, 'b) dynamic -> string
val string_of_lit : ('a -> string) -> ('b -> string) -> ('a, 'b) lit -> string
val string_of_binop : binop -> string
val string_of_formula : ('a -> string) -> 'a formula -> string
val print_formula : ('a -> string) -> 'a formula -> unit

val string_of_safe :
  ('a formula -> string) -> ('b -> string) -> ('b, 'a) pre_safe -> string
val print_safe :
  ('a formula -> string) -> ('b -> string) -> ('b, 'a) pre_safe -> unit

type ('a,'l,'b) general =
  | General of ('a, 'b) guard list * 'l formula

val string_of_general :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('b, 'a, 'c) general -> string
val print_general :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('b, 'a, 'c) general -> unit

type ('a,'l) def =
  | Def of ('a * 'a list * ('a,'l) pre_safe)

(** A program which can be meaningless *)
type ('a,'l) pre_program =
  { vars : ('a, 'l) def list
  ; safe : ('a, 'l) pre_safe list
  ; ensure : ('a, 'l, rbinpred) general list
  ; maintain : ('a, 'l, rbinpred) general list }

(** A program which doesn't distinguish static and dynamic predicates *)
type 'a parsed_program = ('a, bool * ('a, rbinpred) dynamic) pre_program

(** A well-formed program *)
type 'a program = ('a, ('a, rbinpred) lit) pre_program

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

    val variables_of_dynamic : (S.elt,'a) dynamic -> S.t
    val variables_of_lit : (S.elt,'a) lit -> S.t
    val variables_of_formula : (S.elt,'a) lit formula -> S.t
    val variables_of_safe : S.elt safe -> S.t
  end

module Manip(V : Set.OrderedType) : Manip with type t = V.t
