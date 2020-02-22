module Manip : Program.Manip with type t = String.t
module Structure : Structure.Structure with type t = String.t
module Typecheck : Typecheck.Typecheck with type t = String.t

type err =
  | Menhir
  | Type of Typecheck.type_error
  | Parse of Program.parse_error
  | Structure of Structure.invalidity

val print_err : err -> unit

val config : Lexing.lexbuf -> Config.config

val main :
  Config.config ->
  Lexing.lexbuf -> (string Final.final_program, err) result
