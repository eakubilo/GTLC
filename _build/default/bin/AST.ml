(*GTLC language*)
type blame_label = Label of int

type base_types = Int | Bool

type types = B of base_types | Arrow of types * types | Dyn

type var = Var of string

type constants = Inc

type expr =
  | True
  | False
  | Int of int
  | Var of string
  | Lam of var * types * expr
  | UnlabeledApp of expr * expr
  | App of expr * expr * blame_label

type contract = Contract of types * types * blame_label

type cast_expressions =
  | True
  | False
  | Int of int
  | Var of string
  | Lam of var * types * cast_expressions
  | App of cast_expressions * cast_expressions
  | F of cast_expressions * contract
  | Blame of types * blame_label
