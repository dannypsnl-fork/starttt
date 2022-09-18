open Ast
open Term
open Type
module Map = Hashtbl

type value =
  (* constant of a type *)
  | Const of typ
  (* just a variable *)
  | Var of string
  (* λ x . M *)
  | Lambda of string * value
  | App of value * value

let rec value_to_str : value -> string =
 fun v ->
  match v with
  | Const ty -> "#{" ^ type_to_str ty ^ "}"
  | Var x -> x
  | Lambda (x, v) -> "lam " ^ x ^ " . " ^ value_to_str v
  | App (v1, v2) -> value_to_str v1 ^ " " ^ value_to_str v2

(* global runtime context bounds
   ---------------------
      string => value *)
type context = (string, value) Map.t

let empty_context : context = Map.create 0

let rec eval_program : program -> unit =
  let ctx = empty_context in
  fun p ->
    match p with
    | h :: l ->
        eval_top ctx h;
        eval_program l
    | [] -> ()

and eval_top : context -> top -> unit =
 fun ctx t ->
  match t with
  | Let (x, _, e) -> Map.add ctx x (eval ctx e)
  | Run e ->
      let v = eval ctx e in
      print_string (value_to_str v);
      print_newline ()

and eval : context -> term -> value =
 fun ctx tm ->
  match tm with
  | App (Lambda (x, b), e) ->
      let new_ctx = Map.copy ctx in
      Map.add new_ctx x (eval ctx e);
      eval ctx b
  (* normalize the lambda *)
  | Lambda (x, e) -> Lambda (x, eval ctx e)
  (* stuck app *)
  | App (f, arg) -> App (eval ctx f, eval ctx arg)
  | Const ty -> Const ty
  | Var x -> ( match Map.find_opt ctx x with None -> Var x | Some v -> v)
