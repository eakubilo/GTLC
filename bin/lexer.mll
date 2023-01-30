{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let arrow = ['\\']
rule read = 
  parse
  | white { read lexbuf }
  | "." {read lexbuf}
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "\\" {LAM}
  | ":"  white* "int" {INT_TYPE}
  | ":" white* "bool" {BOOL_TYPE}
  | ":" white* "dyn" {DYN_TYPE}
  |"int" {INT_TYPE}
  |"bool"{BOOL_TYPE}
  |"dyn" {DYN_TYPE}
  | "->" white* {ARROW_TYPE}
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
