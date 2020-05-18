open Final_def

(** This module tries to infer missing guards and moves negation only on leaves. *)
type err

val string_of_err : err -> string

val run : string pre_final_program -> (string final_program,err) result
