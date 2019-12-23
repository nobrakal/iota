open Iota

module P = Program.Make(String)

let main filename =
  let chan = open_in filename in
  let ast = Parser.program Lexer.token (Lexing.from_channel chan) in
  let static = Program.SString.empty in
  let dynamic = Program.SString.empty in
  let ast = P.program_of_parsed ~static ~dynamic ast in
  if P.is_valid_program ast
  then print_endline "Good"
  else print_endline "Bad"

let () = main (Sys.argv.(1))
