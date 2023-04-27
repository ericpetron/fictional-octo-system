(** The type of binary operators. *)
type bop = 
  | Add
  | Subt
  | Mult
  | Div
  | Pow

(** The type of the abstract syntax tree (AST). *)
type 'a expr =
  | Int of int
  | Var of 'a
  | Fun of string * ('a expr) list
  | Binop of bop * ('a expr) * ('a expr)
  | Ddx of 'a * ('a expr)

type 'a rule = Rule of ('a expr * 'a expr)
