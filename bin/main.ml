open Iota

module P = Program.Make(String)

let main filename =
  let chan = open_in filename in
  let ast = Parser.program Lexer.token (Lexing.from_channel chan) in
  let static = Program.SString.empty in
  let dynamic = Program.SString.empty in
  match P.program_of_parsed ~static ~dynamic ast with
  | Error s -> print_endline (Program.string_of_parse_error s)
  | Ok ast ->
     print_endline (Program.string_of_validity (P.validate_program ast))

let () = main (Sys.argv.(1))
