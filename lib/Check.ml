open Term
open Type
open Ast
module Map = Hashtbl

(* TODO: conversion check
   This module should return a really to run code!

   conversion of module:
   1. rearrange the definitions
      extracts the definition with non-mono type(here, only arrow type) to the front of the program and binds them with their type
   2. check term of definition with mono type
   3. check term of definition with non-mono type now, because all type bindings are ready now for mutual access

   when converting a module, remember no redefined allowed here
*)

(* global context bounds
   ---------------------
      string => type *)
type context = (string, typ) Map.t

let empty_context : context = Map.create 0

let rec check_top : context -> top -> unit =
 fun ctx tm ->
  match tm with
  | Let (x, ty, e) ->
      check_tm ctx e ty;
      print_string ("[ok] let " ^ x ^ ": checked");
      print_newline ();
      Map.add ctx x ty

(* check term has type *)
and check_tm : context -> term -> typ -> unit =
 fun ctx tm ty ->
  let ty' = infer_ty ctx tm in
  equate_ty ty ty'

(* synthesize type from term *)
and infer_ty : context -> term -> typ =
 fun ctx t ->
  match t with
  (* test *)
  | Const t' -> t'
  (* complex case *)
  | Lambda (x, tm) ->
      let dummy = Placeholder in
      Map.add ctx x dummy;
      Arrow (dummy, infer_ty ctx tm)
  | Var x -> Map.find ctx x

and equate_ty : typ -> typ -> unit =
 fun t1 t2 ->
  match (t1, t2) with
  | Ty s1, Ty s2 ->
      if not (String.equal s1 s2) then raise (TypeMismatch (t1, t2))
  | Arrow (a1, r1), Arrow (a2, r2) ->
      equate_ty a1 a2;
      equate_ty r1 r2
  | Placeholder, _ -> ()
  | _, Placeholder -> ()
  | _ -> raise (TypeMismatch (t1, t2))

let rec check_all : context -> program -> unit =
 fun ctx p ->
  match p with
  | t :: rest ->
      check_top ctx t;
      check_all ctx rest
  | [] -> ()
