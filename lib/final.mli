open Program

type 'a final_program =
  { fsafe : ('a, 'a lit) pre_safe list
  ; fensure : 'a lit general list
  ; fmaintain : 'a lit general list }

(** Transform a program that typecheck into a final one *)
val final_of_program : 'a program -> 'a final_program

val print_final : string final_program -> unit
