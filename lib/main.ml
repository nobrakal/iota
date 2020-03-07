module Manip = Program.Manip(String)
module Structure = Structure.Make(Manip)
module Typecheck = Typecheck.Make(Manip)

open Config

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

let config buf =
  Parser.config Lexer.token buf

let map_filter f p xs =
  List.(map f (filter p xs))

let main config lexbuf =
  match try Some (Parser.program Lexer.token lexbuf) with Parser.Error -> None with
  | None -> Error Menhir
  | Some ast ->
     let static =
       Program.SString.of_list (map_filter (fun (_,x,_) -> x) (fun (b,_,_) -> not b) config.predicates) in
     let dynamic =
       Program.SString.of_list (map_filter (fun (_,x,_) -> x) (fun (b,_,_) -> b) config.predicates) in
     (* Transform the parsed AST into a "real one", knowing static and dynamic functions *)
     match Program.program_of_parsed ~static ~dynamic ast with
     | Error e -> Error (Parse e)
     | Ok ast ->
        (* Typecheck with algorithm W *)
        match Typecheck.typecheck_program ~types:config.types ast with
        | Error e -> Error (Type e)
        | Ok ast ->
           (* Inline every possible defintion of a valid program *)
           let ast = Final.final_of_program ~maxprof:config.maxprof ~types:config.types ast in
           (* Verify that the structure is valid *)
           match Structure.validate_program ast with
           | Some e -> Error (Structure e)
           | None -> Ok ast
