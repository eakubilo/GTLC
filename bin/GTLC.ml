open AST

exception TypeError

exception UnboundVariable

exception WrongApplication

exception InconsistentType

let a = ref 0

(*returns a label with an identifier that is not equal to any previously created label*)
let make_fresh_label () =
  a := !a + 1 ;
  Label !a

(*takes an expression with unlabeled applications returns expression with applications labelled*)
let rec label_expression e : expr =
  match e with
  | UnlabeledApp (e1, e2) ->
      App (label_expression e1, label_expression e2, make_fresh_label ())
  | Lam (x, t, e) ->
      Lam (x, t, label_expression e)
  | _ ->
      e

(*parse a string and return an expression with applications labelled (if they exist).*)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  label_expression ast

(*consistency relation on two types, returns true if t1 ~ t2*)
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

(*looks up variable in context. if variable is bound to a type, return *)
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
  | F (v, Cast (Arrow (_, _), Arrow (_, _), Label _)) when is_value v ->
      true
  | F (v, Cast (t, Dyn, Label _)) when ground_type t && is_value v ->
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
            (F (f1, Cast (t1, Arrow (t11, t12), l)), F (f2, Cast (t2, t11, l)))
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
let y = ref 0

let make_fresh_var () =
  y := !y + 1 ;
  "_x" ^ string_of_int !y

(* rename free variables. newname is to be a fresh name.
   rename:string -> string -> term -> term
 *)
let rec rename name newname tm =
  match tm with
  | Var s ->
      if s = name then Var newname else Var s
  | App (t1, t2) ->
      App (rename name newname t1, rename name newname t2)
  | Lam (Var x, t, e) ->
      if x = name then Lam (Var x, t, e)
      else Lam (Var x, t, rename name newname e)
  | F (f, c) ->
      F (rename name newname f, c)
  | _ ->
      tm

(*capture-avoiding substitutiton*)
let rec subst var s term =
  match term with
  | F (f, c) ->
      F (subst var s f, c)
  | Var b ->
      if b = var then s else term
  | App (t1, t2) ->
      App (subst var s t1, subst var s t2)
  | Lam (Var y, t, e) ->
      if y = var then Lam (Var y, t, e)
      else
        let fresh = make_fresh_var () in
        let newE = rename y fresh e in
        Lam (Var fresh, t, subst var s newE)
  | _ ->
      term

(*one-step evaluation*)
let rec eval f =
  match f with
  | App (Lam (Var x, t, e), v) when is_value v ->
      subst x v e (*BETA*)
  | F (v, Cast (B _, B _, _)) when is_value v ->
      v (*IDBASE*)
  | F (v, Cast (Dyn, Dyn, _)) when is_value v ->
      v (*IDSTAR*)
  | F (F (v, Cast (g1, Dyn, l1)), Cast (Dyn, g2, l2))
    when ground_type g1 && is_value v && g1 = g2 ->
      v (*SUCCEED*)
  | F (F (v, Cast (g1, Dyn, l1)), Cast (Dyn, g2, l2))
    when ground_type g1 && ground_type g2 && is_value v ->
      Blame (g2, l2) (*FAIL*)
  | App (F (v1, Cast (Arrow (t1, t2), Arrow (t3, t4), l)), v2) ->
      F (App (v1, F (v2, Cast (t3, t1, l))), Cast (t2, t4, l)) (*APPCAST*)
  | F (v, Cast (t, Dyn, l)) when is_value v && not (ground_type t) ->
      F (F (v, Cast (t, Arrow (Dyn, Dyn), l)), Cast (Arrow (Dyn, Dyn), Dyn, l))
      (*GROUND*)
  | F (v, Cast (Dyn, t, l)) when is_value v && not (ground_type t) ->
      F (F (v, Cast (Dyn, Arrow (Dyn, Dyn), l)), Cast (Arrow (Dyn, Dyn), t, l))
      (*EXPAND*)
  | F (f, c) when eval f <> f ->
      let f' = eval f in
      if f <> f' then F (f', c) else f
      (*CONG*)
  | F (Blame (t, l), Cast (t1, t2, _)) when t = t1 ->
      Blame (t2, l) (*BLAME*)
  | App (e, f) when not (is_value e) ->
      App (eval e, f) (*CONG*)
  | App (e, f) when not (is_value f) ->
      App (e, eval f)

(*CONG*)

(*helper functions to evaluate*)
let rec eval' t =
  if is_result t then t
  else match eval t with exception _ -> t | t' -> eval' t'

let evalString s =
  let ast = parse s in
  match well_typed_gtlc [] ast with
  | exception _ ->
      raise TypeError
  | _ ->
      let e, t = insert_cast [] ast in
      eval' e
