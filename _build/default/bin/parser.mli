
(* The type of tokens. *)

type token = 
  | UNTYPEDLAM
  | TRUE
  | RPAREN
  | LPAREN
  | LAM
  | LABEL of (string)
  | INT_TYPE
  | INT of (int)
  | ID of (string)
  | FALSE
  | EOF
  | DYN_TYPE
  | BOOL_TYPE
  | ARROW_TYPE
  | APP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (AST.expr)
