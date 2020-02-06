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

  | "forall" { FORALL }
  | "exists" { EXISTS }

  | "->"     { ARROW }
  | "+"      { PLUS }
  | "&&"     { LAND }
  | "||"     { LOR }

  | ";"      { SEMICOLON }
  | ","      { COMMA }
  | "in"     { IN }

  | "="      { EQ }
  | "not"    { NOT  }

  | "Has"    { HAS }
  | "Link"   { LINK }
  | "Parent" { PARENT }
  | "Eq"     { EQQ }

  | "("      { LPAREN }
  | ")"      { RPAREN }

  | lowercase ident* as id    { LowerId(id) }
  | uppercase ident* as id    { UpperId(id) }

  | "(*"                { down_stack () ; comment lexbuf }

  (** Lexing error. *)
  | _                   { failwith ("unexpected character: " ^ (Lexing.lexeme lexbuf)) }

and comment = parse
  | "(*"    { down_stack () ; comment lexbuf }
  | "*)"    { if  up_stack () then token lexbuf else comment lexbuf }
  | _       { comment lexbuf }
