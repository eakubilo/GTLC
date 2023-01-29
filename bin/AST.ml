type blame_label = Label of string

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
  | UntypedLam of var * expr
  | App of expr * expr * blame_label
