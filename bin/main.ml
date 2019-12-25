open Iota

module Manip = Program.Manip(String)
module Structure = Structure.Make(Manip)

let main filename =
  let chan = open_in filename in
  let ast = Parser.program Lexer.token (Lexing.from_channel chan) in
  let static = Program.SString.empty in
  let dynamic = Program.SString.empty in
  match Program.program_of_parsed ~static ~dynamic ast with
  | Error s -> print_endline (Program.string_of_parse_error s)
  | Ok ast ->
     print_endline (Structure.string_of_validity (Structure.validate_program ast))

let () = main (Sys.argv.(1))
