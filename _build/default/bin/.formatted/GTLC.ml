open AST

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec consistency (t1 : types) (t2 : types) : bool =
  match (t1, t2) with
  | Dyn, t ->
      true
  | t, Dyn ->
      true
  | B t, B t' ->
      true
  | Arrow (t1, t2), Arrow (t3, t4) ->
      consistency t1 t3 && consistency t2 t4
  | v ->
      false
