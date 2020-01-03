open Iota

module Manip = Program.Manip(String)
module Structure = Structure.Make(Manip)
module Typecheck = Typecheck.Make(Manip)

let main filename =
  let chan = open_in filename in
  let ast = Parser.program Lexer.token (Lexing.from_channel chan) in
  let static = Program.SString.of_list ["E"] in
  let dynamic = Program.SString.of_list ["Active"; "Open"] in
  match Program.program_of_parsed ~static ~dynamic ast with
  | Error s -> print_endline (Program.string_of_parse_error s)
  | Ok ast ->
     match Typecheck.typecheck_program ast with
     | Some e ->
        Printf.printf "Type: %s\n" (Typecheck.string_of_type_error (fun x -> x) e)
     | None ->
        let ast = Final.final_of_program ast in
        match Structure.validate_program ast with
        | Some e ->
           Printf.printf "Structure: %s\n" (Structure.string_of_invalidity e)
        | None ->
           print_endline "Good";
           Final.print_final ast

let () = main (Sys.argv.(1))
