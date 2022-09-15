exception TODO

open Term
open Type

exception TypeMismatch of typ * typ

type top = Let of string * typ * term | Run of term
type program = top list
