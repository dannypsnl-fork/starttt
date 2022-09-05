type typ =
  (* dummy placeholder *)
  | Ty of string
  (* T -> T *)
  | Arrow of typ * typ
