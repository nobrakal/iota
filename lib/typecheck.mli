open Program

(** This module runs algorithm W on the code, modified to verify constraints on links. *)

type ('a,'l) rich_pre_safe =
  | F of ('a,'l) pre_safe
  | Bracket of 'a list * ('a,'l) pre_safe (* existentially quantified variables, without guards *)

type ('a,'l) rich_def =
  | RDef of ('a * 'a list * ('a,'l) rich_pre_safe)

type 'a typed_program =
  { tvars : ('a, ('a, rbinpred) lit) rich_def list
  ; tsafe : ('a, ('a, rbinpred) lit) pre_safe list
  ; tensure : ('a, ('a, rbinpred) lit, rbinpred) general list
  ; tmaintain : ('a, ('a, rbinpred) lit, rbinpred) general list }

type ground =
  | Vt of string (** Type variable *)
  | Litt of string  (** A known litteral *)

(** Type of monomorphic types *)
type monoty =
  | G of ground (** A ground type *)
  | Safet (** A safe expression *)
  | Arrow of (monoty * monoty) (** Arrow *)

module type Typecheck =
  sig

    type t

    type type_error =
      | UnboundVar of t
      | WrongType of monoty * monoty (* actual, expected *)
      | Constraint of string * string
      | UnInstanciableVar

    val string_of_type_error : (t -> string) -> type_error -> string

    val typecheck_program :
      verbose:((t -> string) option) -> infer_guards:bool ->
      config:Config.config -> t program -> (t typed_program,type_error) result
  end


module Make(Manip : Manip) : Typecheck with type t = Manip.t
