type expr = Int of int | Float of float | Word of string
type top = Let of string * expr
type program = Program of top list
