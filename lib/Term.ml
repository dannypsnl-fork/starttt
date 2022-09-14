open Type

type term =
  (* constant of a type *)
  | Const of typ
  (* just a variable *)
  | Var of string
  (* λ x . M *)
  | Lambda of string * term
  | App of term * term
