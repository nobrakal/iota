{ (* -*- tuareg -*- *)
  open Parser

  let count = ref 0

  let down_stack () = count := !count + 1
  let up_stack () =
    count := !count -1;
    if !count < 0
    then failwith "unbalanced paranthesis"
    else !count = 0

}

(** Regexpr **)
let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let ident = (lowercase | uppercase | digit | '_')*

rule token = parse
  (** Layout *)
  | newline  { token lexbuf }
  | blank+   { token lexbuf }
  | eof      { EOF }

  | "maintain" { MAINTAIN }
  | "ensure" { ENSURE }
  | "let"    { LET }

  | "type" { TYPE }

  | "forall" { FORALL }
  | "exists" { EXISTS }

  | "maxprof" { MAXPROF }
  | "static" { STATIC }
  | "dynamic" { DYNAMIC }
  | "about"   { ABOUT }

  | "=>"     { BIGARROW }
  | "->"     { ARROW }
  | "+"      { PLUS }
  | "&&"     { LAND }
  | "||"     { LOR }

  | "<"      { LCHEVRON }
  | ">"      { RCHEVRON }

  | "|"    { GUARD }
  | "to"   { TO }

  | ";"      { SEMICOLON }
  | ","      { COMMA }
  | "in"     { IN }

  | "="      { EQ }
  | "not"    { NOT }

  | "."      { DOT }

  | "Has"    { HAS }
  | "TLink"  { TLINK }
  | "Link"   { LINK }
  | "parent" { PARENT }
  | "Eq"     { EQQ }

  | "("      { LPAREN }
  | ")"      { RPAREN }

  | digit+ as i { Int(int_of_string i) }
  | lowercase ident* as id    { LowerId(id) }
  | uppercase ident* as id    { UpperId(id) }

  | "(*"                { down_stack () ; comment lexbuf }

  (** Lexing error. *)
  | _                   { failwith ("unexpected character: " ^ (Lexing.lexeme lexbuf)) }

and comment = parse
  | "(*"    { down_stack () ; comment lexbuf }
  | "*)"    { if  up_stack () then token lexbuf else comment lexbuf }
  | _       { comment lexbuf }
