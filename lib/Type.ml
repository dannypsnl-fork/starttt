type typ =
  | Placeholder
  (* type constant *)
  | Ty of string
  (* T -> T *)
  | Arrow of typ * typ

let rec type_to_str : typ -> string =
 fun t ->
  match t with
  | Placeholder -> "_"
  | Ty s -> s
  | Arrow (a, b) -> type_to_str a ^ " -> " ^ type_to_str b
