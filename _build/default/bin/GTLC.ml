open AST

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec consistency (t1, t2) =
  match (t1, t2) with
  | Dyn, _ ->
      true
  | _, Dyn ->
      true
  | B _, B _ ->
      true
  | Arrow (t1, t2), Arrow (t3, t4) ->
      consistency (t1, t3) && consistency (t2, t4)
  | _, _ ->
      false

exception TypeError

exception UnboundVariable

exception WrongApplication

exception InconsistentType

let rec lookup (var, gamma) =
  match gamma with
  | (x, t) :: xs ->
      if x = var then t else lookup (var, xs)
  | [] ->
      raise UnboundVariable

let matched_fun t =
  match t with
  | Arrow (t1, t2) ->
      Arrow (t1, t2)
  | Dyn ->
      Arrow (Dyn, Dyn)
  | _ ->
      raise WrongApplication

(*Γ ⊢ e : T*)
let rec well_typed_gtlc gamma (e : expr) =
  match e with
  | True ->
      B Bool
  | False ->
      B Bool
  | Int _ ->
      B Int
  | Var s ->
      lookup (s, gamma)
  | Lam (Var x, t, e) ->
      Arrow (t, well_typed_gtlc ((x, t) :: gamma) e)
  | App (e1, e2, _) ->
      let t1 = well_typed_gtlc gamma e1 in
      let t2 = well_typed_gtlc gamma e2 in
      let (Arrow (t11, t12)) = matched_fun t1 in
      if consistency (t2, t11) then t12 else raise InconsistentType
  | _ ->
      raise TypeError

(*dynamic syntax*)
let ground_type t =
  match t with B _ -> true | Arrow (Dyn, Dyn) -> true | _ -> false

let rec is_value f =
  match f with
  | True ->
      true
  | False ->
      true
  | Int i ->
      true
  | Lam (_, _, _) ->
      true
  | F (v, Contract (Arrow (_, _), Arrow (_, _), Label _)) when is_value v ->
      true
  | F (v, Contract (t, Dyn, Label _)) when ground_type t && is_value v ->
      true
  | _ ->
      false

let is_result r =
  match r with v when is_value v -> true | Blame (_, _) -> true | _ -> false

(*cast insertion*)
(*Γ ⊢ e ~> f : T*)
let rec insert_cast gamma (e : expr) =
  match e with
  | App (e1, e2, l) ->
      let f1, t1 = insert_cast gamma e1 in
      let f2, t2 = insert_cast gamma e2 in
      let (Arrow (t11, t12)) = matched_fun t1 in
      if consistency (t2, t11) then
        ( App
            ( F (f1, Contract (t1, Arrow (t11, t12), l))
            , F (f2, Contract (t2, t11, l)) )
        , t12 )
      else raise InconsistentType
  | True ->
      (True, B Bool)
  | False ->
      (False, B Bool)
  | Int i ->
      (Int i, B Int)
  | Var s ->
      (Var s, lookup (s, gamma))
  | Lam (Var x, t, e) ->
      let e1, t1 = insert_cast ((x, t) :: gamma) e in
      (Lam (Var x, t, e1), Arrow (t, t1))
  | _ ->
      (True, B Bool)

(* counter for fresh variables *)
let x = ref 0

let make_fresh_var () =
  x := !x + 1 ;
  "_x" ^ string_of_int !x

(* rename free variables. newname is to be a fresh name.
   Therefore capture cannot occur, so we do not consider the problem.
    i.e. reame name newname tm REPLACES every free occurrence of name 
   by newname in tm, without trying to avoid capture. It must be used to
   define subst.
   rename:string -> string -> term -> term
 *)
let rec rename name newname tm =
  match tm with
  | True ->
      True
  | False ->
      False
  | Var s ->
      if s = name then Var newname else Var s
  | App (t1, t2) ->
      App (rename name newname t1, rename name newname t2)
  | Lam (Var x, t, e) ->
      if x = name then Lam (Var x, t, e)
      else Lam (Var x, t, rename name newname e)
  | _ ->
      tm

let rec subst var s term =
  match term with
  | True ->
      True
  | False ->
      False
  | Int i ->
      Int i
  | F (f, c) ->
      F (subst var s f, c)
  | Var y ->
      if y = var then s else term
  | App (t1, t2) ->
      App (subst var s t1, subst var s t2)
  | Lam (Var y, t, e) ->
      if y = var then Lam (Var y, t, e)
      else
        let fresh = make_fresh_var () in
        let newE = rename y fresh e in
        Lam (Var fresh, t, newE)
  | Blame (_, _) ->
      term

let rec eval f =
  match f with
  | App (Lam (Var x, t, e), v) when is_value v ->
      subst x v e
  | F (v, Contract (B _, B _, _)) when is_value v ->
      v
  | F (v, Contract (Dyn, Dyn, _)) when is_value v ->
      v
  | F (F (v, Contract (g1, Dyn, l1)), Contract (Dyn, g2, l2))
    when ground_type g1 && is_value v && g1 = g2 ->
      v
  | F (F (v, Contract (g1, Dyn, l1)), Contract (Dyn, g2, l2))
    when ground_type g1 && ground_type g2 && is_value v ->
      Blame (g2, l2)
  | App (F (v1, Contract (Arrow (t1, t2), Arrow (t3, t4), l)), v2) ->
      F (App (v1, F (v2, Contract (t3, t1, l))), Contract (t2, t4, l))
  | F (v, Contract (t, Dyn, l)) when is_value v && not (ground_type t) ->
      F
        ( F (v, Contract (t, Arrow (Dyn, Dyn), l))
        , Contract (Arrow (Dyn, Dyn), Dyn, l) )
  | F (v, Contract (Dyn, t, l)) when is_value v && not (ground_type t) ->
      F
        ( F (v, Contract (Dyn, Arrow (Dyn, Dyn), l))
        , Contract (Arrow (Dyn, Dyn), t, l) )
  | F (f, c) when eval f <> f ->
      let f' = eval f in
      if f <> f' then F (f', c) else f
  | F (Blame (t, l), Contract (t1, t2, _)) when t = t1 ->
      Blame (t2, l)
  | App (e, f) when not (is_value f) ->
      App (e, eval f)

let rec eval' t =
  if is_result t then t
  else
    match eval t with exception _ -> t | t' -> eval' t'

let evalString s =
  let ast = parse s in
  match well_typed_gtlc [] ast with
  | exception _ ->
      raise TypeError
  | _ ->
      let e, t = insert_cast [] ast in
      eval' e
