module Manip : Program.Manip with type t = String.t
module Structure : Structure.Structure with type t = String.t
module Typecheck : Typecheck.Typecheck with type t = String.t

type err =
  | Type of Typecheck.type_error
  | Parse of Program.parse_error
  | Structure of Structure.invalidity

val print_err : err -> unit

val main :
  static:(string list) -> dynamic:(string list) -> in_channel -> (string Final.final_program, err) result