module Manip : Program.Manip with type t = String.t
module Structure : Structure.Structure with type t = String.t
module Typecheck : Typecheck.Typecheck with type t = String.t
module Gfinal : Gfinal.Guard_inference with type t = String.t

type err =
  | Menhir
  | Type of Typecheck.type_error
  | Parse of Program.parse_error
  | Structure of Structure.invalidity
  | GuardInference of Gfinal.err
val print_err : err -> unit

val config : Lexing.lexbuf -> Config.config

(**
  Provided a configuration and a Lexbuf to parse, this function compiles a Iota code. It:
+ Parses it using Menhir.
+ Typechecks it (with {!Iota.Typecheck}).
+ Compiles it to a much simpler final object (with {!Iota.Final}).
+ Ensures its validty (with {!Iota.Structure}).

*)
val main :
  Config.config ->
  Lexing.lexbuf -> (string Final.final_program, err) result
