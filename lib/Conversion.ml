(* This module should return a really to run code!

   conversion of module:
   1. rearrange the definitions
      extracts the definition with non-mono type(here, only arrow type) to the front of the program and binds them with their type
   2. check term of definition with mono type
   3. check term of definition with non-mono type now, because all type bindings are ready now for mutual access

   when converting a module, remember no redefined allowed here
*)
open Type
open Ast
open Check
module Map = Hashtbl

let isMono : context -> top -> bool =
 fun ctx t ->
  match t with
  | Let (x, (Arrow (_, _) as ty), _) ->
      Map.add ctx x ty;
      false
  | Let (_, _, _) -> true
  | Run _ -> true

let isBind : top -> bool =
 fun t -> match t with Let (_, _, _) -> true | Run _ -> false

let conversion : program -> program =
 fun p ->
  let init_ctx = empty_context in
  let monos, non_monos = List.partition (isMono init_ctx) p in
  check_all init_ctx non_monos;
  check_all init_ctx monos;
  let new_prog = List.append non_monos monos in
  let binds, non_binds = List.partition isBind new_prog in
  List.append binds non_binds
