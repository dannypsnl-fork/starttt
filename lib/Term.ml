type term =
  | Int of int
  | Float of float
  | String of string
  | Lambda of string * term
