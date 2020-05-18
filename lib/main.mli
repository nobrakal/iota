module Manip : Program.Manip with type t = String.t
module Structure : Structure.Structure with type t = String.t

type err =
  | Menhir
  | Type of Typecheck.type_error
  | Parse of Program.parse_error
  | Structure of Structure.invalidity
  | GuardInference of Guard_inference.err
  | Simplification of Final.err

val print_err : err -> unit

val config : Lexing.lexbuf -> Config.config

type options =
  { verbose : bool
  ; infer_guards : bool }

val default_options : options

(**
  Provided a configuration and a Lexbuf to parse, this function compiles a Iota code. It:
+ Parses it using Menhir.
+ Typechecks it (with {!Iota.Typecheck}).
+ Compiles it to a much simpler final object (with {!Iota.Final}).
+ Tries to infer some information (with {!Iota.Guard_inference}).
+ Does some simplification (with {!Iota.Final}).
+ Ensures its validty (with {!Iota.Structure}).

*)
val main :
  options ->
  Config.config ->
  Lexing.lexbuf -> (string Final_def.final_program, err) result
