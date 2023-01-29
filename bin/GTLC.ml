open AST

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec consistency (t1, t2 )  =
  match (t1, t2) with
  | Dyn, _->
      true
  | _, Dyn ->
      true
  | B _, B _ ->
      true
  | Arrow (t1, t2), Arrow (t3, t4) ->
      consistency(t1,t3) && consistency(t2,t4)
  | _, _ ->
      false
exception TypeError;;
exception UnboundVariable;;
exception WrongApplication;;
exception InconsistentType;;
let rec lookup(var, gamma) = 
    match gamma with
    |(x,t)::xs -> (if x = var 
              then t
        else lookup(var, xs))
    |[] -> raise UnboundVariable

let matched_fun(t) =
    match t with
    |Arrow(t1,t2) -> Arrow(t1,t2)
    |Dyn -> Arrow(Dyn, Dyn)
    |_ -> raise WrongApplication
(*Γ ⊢ e : T*)
let rec well_typed_gtlc(gamma, e) =
    match e with
    |True -> B Bool
    |False -> B Bool
    |Int(_) -> B Int
    |Var(s) -> lookup(s, gamma)
    |Lam(Var x, t, e) -> Arrow(t, well_typed_gtlc((x,t)::gamma, e))
    |App(e1, e2, _) ->  let t1 = well_typed_gtlc(gamma, e1) in
                        let t2 = well_typed_gtlc(gamma, e2) in
                        let Arrow(t11, t12) = matched_fun(t1) in
                            if consistency(t2, t11)
                            then t12
                        else raise InconsistentType
    |_ -> raise TypeError;;