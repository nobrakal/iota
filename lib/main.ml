module Manip = Program.Manip(String)
module Structure = Structure.Make(Manip)
module Typecheck = Typecheck.Make(Manip)

type err =
  | Menhir
  | Type of Typecheck.type_error
  | Parse of Program.parse_error
  | Structure of Structure.invalidity

let print_err x =
  let str =
    match x with
    | Menhir -> "Menhir"
    | Type e ->
       "Type: " ^ (Typecheck.string_of_type_error (fun x -> x) e)
    | Parse e ->
       "Parsing: " ^  Program.string_of_parse_error e
    | Structure e ->
       "Structure: " ^ Structure.string_of_invalidity e
  in Printf.eprintf "%s\n" str

let main ~maxprof ~functions ~static ~dynamic lexbuf =
  match try Some (Parser.program Lexer.token lexbuf) with Parser.Error -> None with
  | None -> Error Menhir
  | Some ast ->
     let static = Program.SString.of_list static in
     let dynamic = Program.SString.of_list dynamic in
     (* Transform the parsed AST into a "real one", knowing static and dynamic functions *)
     match Program.program_of_parsed ~static ~dynamic ast with
     | Error e -> Error (Parse e)
     | Ok ast ->
        (* Typecheck with algorithm W *)
        match Typecheck.typecheck_program ast with
        | Some e -> Error (Type e)
        | None ->
           (* Inline every possible defintion of a valid program *)
           let ast = Final.final_of_program ~maxprof ~functions ast in
           (* Verify that the structure is valid *)
           match Structure.validate_program ast with
           | Some e -> Error (Structure e)
           | None -> Ok ast
