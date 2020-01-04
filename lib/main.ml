module Manip = Program.Manip(String)
module Structure = Structure.Make(Manip)
module Typecheck = Typecheck.Make(Manip)

type err =
  | Type of Typecheck.type_error
  | Parse of Program.parse_error
  | Structure of Structure.invalidity

let print_err x =
  let str =
    match x with
    | Type e ->
       "Type: " ^ (Typecheck.string_of_type_error (fun x -> x) e)
    | Parse e ->
       "Parsing: " ^  Program.string_of_parse_error e
    | Structure e ->
       "Structure: " ^ Structure.string_of_invalidity e
  in Printf.eprintf "%s\n" str

let main ~static ~dynamic chan =
  let ast = Parser.program Lexer.token (Lexing.from_channel chan) in
  let static = Program.SString.of_list static in
  let dynamic = Program.SString.of_list dynamic in
  match Program.program_of_parsed ~static ~dynamic ast with
  | Error e -> Error (Parse e)
  | Ok ast ->
     match Typecheck.typecheck_program ast with
     | Some e -> Error (Type e)
     | None ->
        let ast = Final.final_of_program ast in
        match Structure.validate_program ast with
        | Some e -> Error (Structure e)
        | None -> Ok ast
