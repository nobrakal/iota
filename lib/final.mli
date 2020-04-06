(** This module compiles a typechecking program to a final one. It
- Inlines all let definitions.
- Moves negation only on leaves.
- Compiles [TLink] to the corresponding list of [Link].
- Simplifies [Link(x.f)] into [x].
 *)

(** Transform a program that typechecks into a final one *)
val final_of_program :
  maxprof:int ->
  types:(Config.ty_dec list) ->
  string Typecheck.typed_program -> string Final_def.pre_final_program
