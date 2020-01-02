open Iota

module Manip = Program.Manip(String)
module Structure = Structure.Make(Manip)
module Typecheck = Typecheck.Make(Manip)

let def_good f x = Option.value (Option.map f x) ~default:"Good"

let main filename =
  let chan = open_in filename in
  let ast = Parser.program Lexer.token (Lexing.from_channel chan) in
  let static = Program.SString.of_list ["E"] in
  let dynamic = Program.SString.of_list ["Active"; "Open"] in
  match Program.program_of_parsed ~static ~dynamic ast with
  | Error s -> print_endline (Program.string_of_parse_error s)
  | Ok ast ->
     Printf.printf "Structure: %s\n" (def_good Structure.string_of_invalidity (Structure.validate_program ast));
     Printf.printf "Type: %s\n" (def_good (Typecheck.string_of_type_error (fun x -> x)) (Typecheck.typecheck_program ast))

let () = main (Sys.argv.(1))
