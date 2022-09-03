open Type

type term =
  (* constant of a type *)
  | Const of typ
  | Lambda of string * typ * term
