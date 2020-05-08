(** This module compiles a typechecking program to a final one. It
- Inlines all let definitions.
- Compiles [TLink] to the corresponding list of [Link] (taking into account links constraint).
- Simplifies [Link(x.f)] into [x].
 *)

type err =
  | UnknownType of string
  | EmptyTLink

val string_of_err : err -> string

(** Transform a program that typechecks into a final one *)
val final_of_program :
  config:Config.config ->
  string Typecheck.typed_program -> (string Final_def.pre_final_program,err) result
