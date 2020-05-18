module Manip = Program.Manip(String)
module Structure = Structure.Make(Manip)

open Config

type err =
  | Menhir
  | Type of Typecheck.type_error
  | Parse of Program.parse_error
  | Structure of Structure.invalidity
  | GuardInference of Guard_inference.err
  | Simplification of Final.err

let print_err x =
  let str =
    match x with
    | Menhir -> "Menhir"
    | Type e ->
       "Type: " ^ Typecheck.string_of_type_error e
    | Parse e ->
       "Parsing: " ^  Program.string_of_parse_error e
    | Structure e ->
       "Structure: " ^ Structure.string_of_invalidity e
    | GuardInference e ->
       "GuardInference: " ^ Guard_inference.string_of_err e
    | Simplification e ->
       "Simplification: " ^ Final.string_of_err e
  in Printf.eprintf "%s\n" str

type options =
  { verbose : bool
  ; infer_guards : bool }

let default_options =
  {verbose=true; infer_guards=true}

let config buf =
  Parser.config Lexer.token buf

let main options config lexbuf =
  match try Some (Parser.program Lexer.token lexbuf) with Parser.Error -> None with
  | None -> Error Menhir
  | Some ast ->
     let static = static_of_config config in
     let dynamic = dynamic_of_config config in
     (* Transform the parsed AST into a "real one", knowing static and dynamic functions *)
     match Program.program_of_parsed ~static ~dynamic ast with
     | Error e -> Error (Parse e)
     | Ok ast ->
        (* Typecheck with algorithm W *)
        match Typecheck.typecheck_program ~verbose:options.verbose
                ~infer_guards:options.infer_guards ~config ast with
        | Error e -> Error (Type e)
        | Ok ast ->
           (* Inline every possible defintion of a valid program *)
           match Final.final_of_program ~config ast with
           | Error e -> Error (Simplification e)
           | Ok ast ->
              (* Try to infer guards *)
              match Guard_inference.run ast with
              | Error e -> Error (GuardInference e)
              | Ok ast ->
                 (* Verify that the structure is valid *)
                 match Structure.validate_program ast with
                 | Some e -> Error (Structure e)
                 | None -> Ok ast
